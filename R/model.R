#' Format data for use with stan
#' @param obs A data frame with the following variables:
#'  date, cases, seq_delta, and seq_total.
#' @param horizon Integer forecast horizon. Defaults to 4.
#' @param delta Numeric vector of length 2. Prior mean and
#' standard deviation for the initial growth rate modifier
#' due to the variant.
#' @export
#' @examples
#' stan_data(latest_obs(germany_obs))
stan_data <- function(obs, horizon = 4, delta = c(0.2, 0.2),
                      likelihood = TRUE,
                      output_loglikelihood = FALSE) {
  obs <- data.table::as.data.table(obs)
  data <- list(
    # time indices
    t = nrow(obs) + horizon,
    t_nots = nrow(obs[!is.na(cases)]),
    t_nseq = nrow(obs[is.na(seq_available)]),
    t_seq = nrow(obs[!is.na(seq_delta)]),
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
    delta_sd = delta[2]
  )
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
          data$X[t_init + 1] * data$Y[1] / data$N[1]
        ),
        ~ log(abs(rnorm(1, ., . * 0.01)))
      ),
      r = rnorm(1, 0, 0.05),
      r_noise = abs(rnorm(1, 0, 0.01)),
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
    }
    return(inits)
  }
  return(init_fn)
}

#' Load and compile a strain model
#' @export
#' @examples
#' \dontrun{
#' # one strain model
#' bp_mod <- load_model(strains = 1)
#'
#' # two strain model
#' twostrain_bp_mod <- load_model(strains = 2)
#' }
load_model <- function(strains = 2) {
  if (strains == 1) {
    model <- "stan/bp.stan"
  } else if (strains == 2) {
    model <- "stan/twostrainbp.stan"
  } else {
    stop("Only 1 or 2 strain models are supported")
  }

  model <- system.file(model, package = "bp.delta")
  model <- cmdstanr::cmdstan_model(model)
  return(model)
}

#' Fit a brancing process strain model
#' @export
#' @examples
#' \dontrun{
#' # format example data
#' dt <- stan_data(latest_obs(germany_obs))
#'
#' # single strain model
#' inits <- stan_inits(dt, strains = 1)
#' mod <- load_model(strains = 1)
#' fit <- stan_fit(dt, model = mod, init = inits, adapt_delta = 0.99)
#' fit
#'
#' # two strain model
#' inits <- stan_inits(dt, strains = 2)
#' mod <- load_model(strains = 2)
#' two_strain_fit <- stan_fit(dt, model = mod, init = inits, adapt_delta = 0.99)
#' two_strain_fit
#' }
stan_fit <- function(data,
                     model = bp.delta::load_model(strains = 2),
                     save_path, diagnostics = TRUE, ...) {
  cdata <- data
  cdata$start_date <- NULL
  fit <- model$sample(data = cdata, ...)

  if (!missing(save_path)) {
    fit$save_object(file = file.path(save_path, "fit.rds"))
  }

  if (diagnostics) {
    fit$cmdstan_diagnose()
  }

  sfit <- fit$summary()
  sfit <- data.table::setDT(sfit)

  if (!missing(save_path)) {
    data.table::fwrite(sfit, file.path(save_path, "summarised_posterior.csv"))
  }

  out <- list(
    fit = fit,
    data = data,
    posterior = sfit
  )
  return(out)
}
