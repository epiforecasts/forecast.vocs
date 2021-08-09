#' Format data for use with stan
#' @param obs A data frame with the following variables:
#'  date, cases, seq_delta, and seq_total.
#' @param horizon Integer forecast horizon. Defaults to 4.
#' @param delta Numeric vector of length 2. Prior mean and
#' standard deviation for the initial growth rate modifier
#' due to the variant.
#' @param variant_relationship Character string, defaulting to "pooled".
#' Controls the relationship of strains with options being "pooled" (dependence
#' determined from the data), "scaled" (a fixed scaling between strains), and
#' "independent" (fully independent strains after initial scaling).
#' @param overdisperion Logical, defaults to `TRUE`. Should the observations
#' used include overdispersion.
#' @param likelihood Logical, defaults to `TRUE`. Should the likelihood be
#' included in the model.extract
#' @param output_loglikelihood Logical, defaults to `FALSE`. Should the log
#' likelihood be output. Disabling this will speed up fitting if evaluating the
#' model fit is not required.
#' @export
#' @examples
#' stan_data(latest_obs(germany_obs))
stan_data <- function(obs, horizon = 4, delta = c(0.2, 0.2),
                      variant_relationship = "pooled",
                      overdispersion = TRUE,
                      likelihood = TRUE,
                      output_loglikelihood = TRUE) {
  variant_relationship <- match.arg(
    variant_relationship,
    choices = c("pooled", "scaled", "independent")
  )

  obs <- data.table::as.data.table(obs)
  data <- list(
    # time indices
    t = nrow(obs) + horizon,
    t_nots = nrow(obs[!is.na(cases)]),
    t_nseq = nrow(obs[is.na(seq_available)]),
    t_seq = nrow(obs[!is.na(seq_delta)]),
    t_seqf = nrow(obs) + horizon - nrow(obs[is.na(seq_available)]),
    # weekly incidences
    X = obs[!is.na(cases)]$cases,
    # total number of sequenced samples
    N = obs[!is.na(seq_total)]$seq_total,
    # number of sequenced samples with delta variant
    Y = obs[!is.na(seq_total)]$seq_delta,
    likelihood = as.numeric(likelihood),
    output_loglik = as.numeric(output_loglikelihood),
    start_date = min(obs$date),
    delta_mean = delta[1],
    delta_sd = delta[2],
    relat = fcase(
      variant_relationship %in% "pooled", 1,
      variant_relationship %in% "scaled", 0,
      variant_relationship %in% "independent", 2
    ),
    overdisp = as.numeric(overdispersion)
  )
  # assign time where strains share a noise parameter
  data$t_dep <- ifelse(data$relat == 2, data$t_nseq, data$t - 2)
  return(data)
}

#' Set up initial conditions for model
#' @export
#' @importFrom purrr map_dbl
#' @examples
#' dt <- stan_data(latest_obs(germany_obs))
#' inits <- stan_inits(dt)
#' inits
#' inits()
stan_inits <- function(data, strains = 2) {
  init_fn <- function() {
    inits <- list(
      init_cases = purrr::map_dbl(
        c(
          data$X[1],
          data$X[data$t_nseq + 1] * data$Y[1] / data$N[1]
        ),
        ~ log(abs(rnorm(1, ., . * 0.01)))
      ),
      r_init = rnorm(1, 0, 0.05),
      r_noise = abs(rnorm(1, 0, 0.01)),
      eta = rnorm(data$t_dep, 0, 0.01),
      beta = rnorm(1, 0, 0.1),
      sqrt_phi = abs(rnorm(2, 0, 0.01))
    )
    if (strains == 1) {
      inits$init_cases <- inits$init_cases[1]
      inits$sqrt_phi <- inits$sqrt_phi[1]
    } else {
      inits$delta_mod <- rnorm(
        1, data$delta_mean,
        data$delta_sd * 0.1
      )
      inits$delta_noise <- abs(rnorm(1, 0, 0.01))
      inits$ndelta_noise <- abs(rnorm(1, 0, 0.01))
      inits$delta_eta <- rnorm(data$t_seqf - 2, 0, 0.01)
      inits$ndelta_eta <- rnorm(data$t_seqf - 2, 0, 0.01)
    }
    return(inits)
  }
  return(init_fn)
}

#' Load and compile a strain model
#' @param compile Logical, defaults to `TRUE`. Should the model
#' be loaded and compiled using `cmdstanr::cmstan_model()`.
#' @param ... Additional arguments passed to `cmdstanr::cmstan_model()`.
#' @export
#' @examples
#' \dontrun{
#' # one strain model
#' mod <- load_model(strains = 1)
#'
#' # two strain model
#' two_strain_mod <- load_model(strains = 2)
#' }
load_model <- function(strains = 2, compile = TRUE, ...) {
  if (strains == 1) {
    model <- "stan/bp.stan"
  } else if (strains == 2) {
    model <- "stan/twostrainbp.stan"
  } else {
    stop("Only 1 or 2 strain models are supported")
  }

  model <- system.file(model, package = "bp.delta")
  if (compile) {
    cmdstanr::cmdstan_model(model, ...)
  }
  return(model)
}

#' Fit a brancing process strain model
#' @param data A list of data as produced by `stan_data()`
#' @param model A `cmdstanr` model object as loaded by `load_model()`
#' @param save_path Character string indicating the save path to use for results
#' if required. Defaults to empty meaning that nothing is saved
#' @param diagnostics Logical, defaults to `TRUE`. Should fitting diagnostics
#' be shown.
#' @param ... Additional parameters passed to the `sample` method of `cmdstanr`.
#' @export
#' @examples
#' \dontrun{
#' # parallisation
#' options(mc.cores = 4)
#' # format example data
#' dt <- stan_data(latest_obs(germany_obs))
#'
#' # single strain model
#' inits <- stan_inits(dt, strains = 1)
#' mod <- load_model(strains = 1)
#' fit <- stan_fit(dt,
#'   model = mod, init = inits, adapt_delta = 0.99,
#'   max_treedepth = 15
#' )
#' fit
#'
#' # two strain model
#' inits <- stan_inits(dt, strains = 2)
#' mod <- load_model(strains = 2)
#' two_strain_fit <- stan_fit(dt,
#'   model = mod, init = inits,
#'   adapt_delta = 0.99, max_treedepth = 15
#' )
#' two_strain_fit
#' }
stan_fit <- function(data,
                     model = bp.delta::load_model(strains = 2),
                     save_path = NULL, diagnostics = TRUE, ...) {
  cdata <- data
  cdata$start_date <- NULL
  model <- cmdstanr::cmdstan_model(model)
  fit <- model$sample(data = cdata, ...)

  if (!is.null(save_path)) {
    fit$save_object(file = file.path(save_path, "fit.rds"))
  }

  if (diagnostics) {
    fit$cmdstan_diagnose()
  }

  sfit <- fit$summary()
  sfit <- data.table::setDT(sfit)

  if (!is.null(save_path)) {
    data.table::fwrite(sfit, file.path(save_path, "summarised_posterior.csv"))
  }

  out <- list(
    fit = fit,
    data = data,
    posterior = sfit
  )
  return(out)
}
