#' @export
#' @examples
#' stan_data(germany_cases)
stan_data <- function(cases, horizon = 4, likelihood = TRUE,
                      output_loglikelihood = FALSE) {

  cases <- data.table::as.data.table(cases)
  data <- list(
    # time indices
    t = nrow(cases) + horizon,
    t_nots = nrow(cases),
    t_seq = nrow(cases[!is.na(seq_B.1.1617.2)]),
    # weekly incidences
    X  = cases$inc7,
    # total number of sequenced samples
    N = cases[!is.na(seq_total)]$seq_total,
    # number of sequenced samples with delta variant
    Y = cases[!is.na(seq_total)]$seq_B.1.1617.2,
    likelihood = as.numeric(likelihood),
    output_loglik = as.numeric(output_loglikelihood),
    start_date = min(cases$date)
  )
  return(data)
}

#' @export
#' @importFrom purrr map_dbl
#' @examples
#' dt <- stan_data(germany_cases)
#' stan_inits(dt)
stan_inits <- function(data, strains = 2) {
  init_fn <- function() {
    inits <- list(
      init_cases = purrr::map_dbl(
        c(data$X[1],
          data$X[1] * data$Y[1] / data$N[1]),
        ~ log(abs(rnorm(1, ., . * 0.01)))),
      r = rnorm(1, 0, 0.05),
      r_noise = abs(rnorm(1, 0, 0.01)),
      sqrt_phi = abs(rnorm(2, 0, 0.01))
    )
    if (strains == 1) {
      inits$init_cases <- inits$init_cases[1]
      inits$sqrt_phi <- inits$sqrt_phi[1]
    }else{
      inits$delta_mod <- rnorm(1, 0.2, 0.05)
      inits$delta_noise <- abs(rnorm(1, 0, 0.01))
    }
    return(inits)
  }
  return(init_fn)
}

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
  }else if (strains == 2) {
    model <- "stan/twostrainbp.stan"
  }else{
    stop("Only 1 or 2 strain models are supported")
  }

  model <- system.file(model, package = "bp.delta")
  model <- cmdstanr::cmdstan_model(model)
  return(model)
}

#' @export
#' @examples
#' \dontrun{
#' # format example data
#' dt <- stan_data(germany_cases)
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
#' 2strain_fit <- stan_fit(dt, model = mod, init = inits, adapt_delta = 0.99)
#' 2strain_fit
#' }
stan_fit <- function(data,
                     model = bp.delta::load_model(strains = 2),
                     save_path, diagnostics = TRUE, ...) {
  cdata <- data
  cdata$start_date <- NULL
  fit <- mod$sample(data = cdata, ...)

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
