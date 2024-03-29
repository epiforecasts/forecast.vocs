#' Format data for use with stan
#'
#' @param obs A data frame with the following variables:
#'  `date`, `cases`, `seq_voc`, and `seq_total`.
#'
#' @param horizon Integer forecast horizon. Defaults to 4.
#'
#' @param r_init Numeric vector of length 2. Mean and
#' standard deviation for the normal prior on the initial log growth rate.
#'
#' @param r_step Integer, defaults to 1. The number of observations between
#' each change in the growth rate.
#'
#' @param r_forecast Logical, defaults `TRUE`. Should the growth rate be
#' forecast beyond the data horizon.
#'
#' @param beta Numeric vector, defaults to c(0, 0.5). Represents the mean and
#' standard deviation of the normal prior (truncated at 1 and -1) on the
#' weighting in the differenced AR process of  the previous difference.
#' Placing a tight prior around zero effectively reduces the AR process to a
#' random walk on the growth rate.
#'
#' @param lkj Numeric defaults to 0.5. The assumed prior covariance between
#' variants growth rates when using the "correlated" model. This sets the shape
#' parameter for the Lewandowski-Kurowicka-Joe (LKJ) prior distribution. If set
#' to 1 assigns a uniform prior for all correlations, values less than 1
#' indicate increased belief in strong correlations and values greater than 1
#' indicate increased belief weaker correlations. Our default setting places
#' increased weight on some correlation between strains.
#'
#' @param voc_scale Numeric vector of length 2. Prior mean and
#' standard deviation for the initial growth rate modifier
#' due to the variant of concern.
#'
#' @param period Logical defaults to `NULL`. If specified should be a function
#' that accepts a vector of dates. This can be used to assign periodic effects
#' to dates which will then be adjusted for in the case model. An example
#' is adjusting for day of the week effects for which the [fv_dow_period()]
#' can be used.
#'
#' @param special_periods A vector of dates to pass to the `period` function
#' argument with the same name to be treated as "special" for example holidays
#' being treated as sundays in [fv_dow_period()].
#'
#' @param variant_relationship Character string, defaulting to "correlated".
#' Controls the relationship of strains with options being "correlated"
#' (strains growth rates are correlated over time), "scaled" (a fixed scaling
#'  between strains), and "independent" (fully independent strains after
#'  initial scaling).
#'
#' @param overdispersion Logical, defaults to `TRUE`. Should the observations
#' used include overdispersion.
#'
#' @param likelihood Logical, defaults to `TRUE`. Should the likelihood be
#' included in the model
#'
#' @param output_loglik Logical, defaults to `FALSE`. Should the
#' log-likelihood be output. Disabling this will speed up fitting
#' if evaluating the model fit is not required.
#'
#' @param debug Logical, defaults to `FALSE`. Should within model debug
#' information be returned.
#'
#' @return A list as required by stan.
#'
#' @family model
#' @export
#' @examples
#' fv_as_data_list(latest_obs(germany_covid19_delta_obs))
fv_as_data_list <- function(obs, horizon = 4,
                            r_init = c(0, 0.25),
                            r_step = 1, r_forecast = TRUE,
                            beta = c(0, 0.5),
                            lkj = 0.5, voc_scale = c(0, 0.2),
                            period = NULL, special_periods = c(),
                            variant_relationship = "correlated",
                            overdispersion = TRUE,
                            likelihood = TRUE,
                            output_loglik = TRUE,
                            debug = FALSE) {
  variant_relationship <- match.arg(
    variant_relationship,
    choices = c("correlated", "scaled", "independent")
  )
  check_observations(obs)
  check_param(horizon, "horizon", length = 1, type = "numeric")
  check_param(r_init, "r_init", length = 2, type = "numeric")
  check_param(voc_scale, "voc_scale", length = 2, type = "numeric")
  check_param(overdispersion, "overdispersion", type = "logical")
  check_param(likelihood, "likelihood", type = "logical")
  check_param(output_loglik, "output_loglik", type = "logical")
  check_param(debug, "debug", type = "logical")

  obs <- data.table::as.data.table(obs)
  data.table::setorderv(obs, cols = c("date"))
  seq_start_date <- obs[!is.na(seq_voc)][date == min(date)]$date

  # find initial dates with no sequences
  data <- list(
    # time indices
    t = nrow(obs) + horizon,
    t_nots = nrow(obs[!is.na(cases)]),
    t_nseq = nrow(obs[date < seq_start_date]),
    t_seq = nrow(obs[!is.na(seq_voc)]),
    t_seqf = nrow(obs) + horizon - nrow(obs[date < seq_start_date]),
    # weekly incidences
    X = obs[!is.na(cases)]$cases,
    # total number of sequenced samples
    N = obs[!is.na(seq_total)]$seq_total,
    # number of sequenced samples with voc variant
    Y = obs[!is.na(seq_total)]$seq_voc,
    start_date = min(obs$date),
    seq_start_date = seq_start_date,
    r_init_mean = r_init[1],
    r_init_sd = r_init[2],
    beta_mean = beta[1],
    beta_sd = beta[2],
    voc_mean = voc_scale[1],
    voc_sd = voc_scale[2],
    relat = fcase(
      variant_relationship %in% "correlated", 2,
      variant_relationship %in% "scaled", 0,
      variant_relationship %in% "independent", 1
    ),
    overdisp = as.numeric(overdispersion),
    likelihood = as.numeric(likelihood),
    output_loglik = as.numeric(output_loglik),
    debug = as.numeric(debug)
  )

  ## add period
  if (is.null(period)) {
    data$period <- 1
    data$periodic <- rep(1, data$t)
  } else {
    data$periodic <- period(
      t = data$t, obs$date[1],
      specials = special_periods
    )
    data$period <- max(data$periodic)
  }

  ## add autoregressive control terms
  r_steps <- piecewise_steps(data$t - 2, r_step,
    offset = nrow(obs) - 2,
    steps_post_offset = r_forecast
  )
  if (data$relat == 0) {
    voc_r_steps <- list(n = 0, steps = numeric())
  } else {
    voc_r_steps <- r_steps$steps[(data$t_nseq + 1):(data$t - 2)]
    voc_r_steps <- list(n = sum(voc_r_steps), steps = voc_r_steps)
  }
  data <- c(
    data,
    list(
      eta_n = r_steps$n,
      eta_loc = r_steps$steps,
      voc_eta_n = voc_r_steps$n,
      voc_eta_loc = voc_r_steps$steps,
      lkj_prior = lkj
    )
  )
  return(data)
}

#' Set up initial conditions for model
#'
#' @param data A list of data as produced by [fv_as_data_list()].
#'
#' @return A function that when called returns a list of initial conditions
#' for the package stan models.
#'
#' @family model
#' @export
#' @inheritParams fv_model
#' @importFrom purrr map_dbl
#' @importFrom stats runif
#' @examples
#' dt <- fv_as_data_list(latest_obs(germany_covid19_delta_obs))
#' inits <- fv_inits(dt)
#' inits
#' inits()
fv_inits <- function(data, strains = 2) {
  init_fn <- function() {
    inits <- list(
      init_cases = array(log(abs(rnorm(1, data$X[1], data$X[1] * 0.01)))),
      r_init = rnorm(1, data$r_init_mean, data$r_init_sd * 0.1),
      r_scale = abs(rnorm(1, 0, 0.01)),
      eta = rnorm(data$eta_n, 0, 0.01),
      beta = rnorm(1, 0, 0.1),
      sqrt_phi = array(abs(rnorm(2, 0, 0.01))),
      period_eff = numeric(0),
      period_sd = numeric(0)
    )
    if (data$period > 1) {
      inits$period_eff <- array(rnorm(data$period, 0, 0.1))
      inits$period_sd <- array(abs(rnorm(1, 0, 0.1)))
    }
    if (strains == 1) {
      inits$sqrt_phi <- array(inits$sqrt_phi[1])
    } else {
      inits$init_voc_cases <- array(
        log(abs(rnorm(
          1, max(2, data$X[data$t_nseq + 1] * data$Y[1] / data$N[1]),
          max(2, data$X[data$t_nseq + 1] * data$Y[1] / data$N[1]) * 0.01
        )))
      )
      inits$voc_mod <- rnorm(
        1, data$voc_mean,
        data$voc_sd * 0.1
      )
      if (data$relat == 1) {
        inits$voc_beta <- array(rnorm(1, 0, 0.1))
      }else {
        inits$voc_beta <- numeric(0)
      }
      if (data$relat > 0) {
        inits$voc_scale <- array(abs(rnorm(1, 0, 0.01)))
        inits$voc_eta <- array(rnorm(data$voc_eta_n, 0, 0.01))
      } else {
        inits$voc_scale <- numeric(0)
        inits$voc_eta <- numeric(0)
      }
      if (data$relat == 2) {
        inits$L_Omega <- matrix(c(1, runif(1), 0, runif(1)), 2, 2) # nolint
      }
    }

    if (data$overdisp == 0) {
      inits$sqrt_phi <- numeric(0)
    }
    return(inits)
  }
  return(init_fn)
}

#' Load and compile a strain model
#'
#'
#' @param model A character string indicating the path to the model.
#' If not supplied the package default model is used.
#'
#' @param include A character string specifying the path to any stan
#' files to include in the model. If missing the package default is used.
#'
#' @param strains Integer number of strains. Defaults to 2. Current
#' maximum is 2.
#'
#' @param compile Logical, defaults to `TRUE`. Should the model
#' be loaded and compiled using [cmdstanr::cmdstan_model()].
#'
#' @param verbose Logical, defaults to `TRUE`. Should verbose
#' messages be shown.
#'
#' @param ... Additional arguments passed to [cmdstanr::cmdstan_model()].
#'
#' @return A `cmdstanr` model.
#'
#' @family model
#' @export
#' @examplesIf interactive()
#' # one strain model
#' mod <- fv_model(strains = 1)
#'
#' # two strain model
#' two_strain_mod <- fv_model(strains = 2)
fv_model <- function(model, include, strains = 2, compile = TRUE,
                     verbose = FALSE, ...) {
  check_param(strains, "strains", "numeric")
  check_param(compile, "compile", "logical")
  if (missing(model)) {
    if (strains == 1) {
      model <- "stan/bp.stan"
    } else if (strains == 2) {
      model <- "stan/twostrainbp.stan"
    } else {
      stop("Only 1 or 2 strain models are supported")
    }
    model <- system.file(model, package = "forecast.vocs")
  }
  if (missing(include)) {
    include <- system.file("stan", package = "forecast.vocs")
  }

  if (compile) {
    if (verbose) {
      model <- cmdstanr::cmdstan_model(model,
        include_path = include, ...
      )
    } else {
      suppressMessages(
        model <- cmdstanr::cmdstan_model(model,
          include_path = include, ...
        )
      )
    }
  }
  return(model)
}

#' Fit a brancing process strain model
#'
#' @param data A list of data as produced by [fv_as_data_list()].
#'
#' @param model A `cmdstanr` model object as loaded by [fv_model()].
#'
#' @param diagnostics Logical, defaults to `TRUE`. Should fitting diagnostics
#' be returned as a `data.frame`.
#'
#' @param ... Additional parameters passed to the `sample` method of `cmdstanr`.
#'
#' @return A `data.frame` containing the `cmdstanr` fit, the input data, the
#' fitting arguments, and optionally summary diagnostics.
#'
#' @family model
#' @export
#' @importFrom cmdstanr cmdstan_model
#' @importFrom posterior rhat
#' @examplesIf interactive()
#' options(mc.cores = 4)
#'
#' # format example data
#' obs <- filter_by_availability(
#'   germany_covid19_delta_obs,
#'   date = as.Date("2021-06-12"),
#' )
#' dt <- fv_as_data_list(obs)
#'
#' # single strain model
#' inits <- fv_inits(dt, strains = 1)
#' mod <- fv_model(strains = 1)
#' fit <- fv_sample(
#'   dt,
#'   model = mod, init = inits,
#'   adapt_delta = 0.99, max_treedepth = 15
#' )
#' fit
#'
#' # two strain model
#' inits <- fv_inits(dt, strains = 2)
#'
#' mod <- fv_model(strains = 2)
#'
#' two_strain_fit <- fv_sample(dt,
#'   model = mod, init = inits,
#'   adapt_delta = 0.99, max_treedepth = 15
#' )
#' two_strain_fit
fv_sample <- function(data, model = forecast.vocs::fv_model(strains = 2),
                      diagnostics = TRUE, ...) {
  check_param(data, "data", "list")
  check_param(diagnostics, "diagnostics", "logical")
  cdata <- data
  cdata$start_date <- NULL
  cdata$seq_start_date <- NULL
  fit <- model$sample(data = cdata, ...)

  out <- data.table(
    fit = list(fit),
    data = list(data),
    fit_args = list(list(...))
  )

  if (diagnostics) {
    diag <- fit$sampler_diagnostics(format = "df")
    diagnostics <- data.table(
      samples = nrow(diag),
      max_rhat = round(max(
        fit$summary(
          variables = NULL, posterior::rhat,
          .args = list(na.rm = TRUE)
        )$`posterior::rhat`,
        na.rm = TRUE
      ), 2),
      divergent_transitions = sum(diag$divergent__),
      per_divergent_transitions = sum(diag$divergent__) / nrow(diag),
      max_treedepth = max(diag$treedepth__)
    )
    diagnostics[, no_at_max_treedepth := sum(diag$treedepth__ == max_treedepth)]
    diagnostics[, per_at_max_treedepth := no_at_max_treedepth / nrow(diag)]
    out <- cbind(out, diagnostics)

    timing <- round(fit$time()$total, 1)
    out[, time := timing]
  }
  return(out[])
}
