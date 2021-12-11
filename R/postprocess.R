#' Link dates by time for posterior parameter estimates
#'
#' @param posterior A data frame of summarised posterior estimates
#' as returned by [cmdstanr::summary()] with  an additional type variable
#' which contains the following character string options: "non-VOC",
#' "VOC", "Combined", "Overall".
#'
#' @param data A list of data as returned in the "data" entry of the output
#' returned by [fv_sample()].
#'
#' @param mod_end Integer, defaults to 0. Amount to shift the end date of
#' estimates.
#'
#' @return A posterior `data.frame` with an additional data column.
#'
#' @family postprocess
link_dates_with_posterior <- function(posterior, data, mod_end = 0) {
  # extract info from lis
  t <- data$t
  t_nseq <- data$t_nseq
  t_seqf <- data$t_seqf
  start_date <- data$start_date
  end_date <- start_date + 7 * t - 1 - mod_end

  # build dates data frame
  dates <- data.table(
    start = c(rep(start_date, 3), start_date + t_nseq * 7),
    end = end_date,
    type = c("non-VOC", "Combined", "Overall", "VOC")
  )
  dates <- dates[, .(date = seq(start, end, by = "weeks")), by = "type"]
  dates <- dates[, id := 1:.N, by = "type"]

  # link to input data frame
  posterior <- setDT(posterior)
  posterior <- posterior[, id := 1:.N, by = "type"]
  posterior <- merge(posterior, dates, by = c("type", "id"))
  posterior <- posterior[, id := NULL]
  setcolorder(posterior, neworder = c("type", "date"))
  return(posterior)
}


#' Link posterior estimates with observed data
#'
#' Link posterior estimates with observed data and flag if values are observed
#' or not.
#'
#' @param obs Numeric vector of observed data to link to posterior estimates.
#'
#' @param horizon Integer indicating the horizon of unobserved forecasts. If not
#' specified will be inferred from `obs`.
#'
#' @param target_types A character vector of types (as specified in the `type`
#' variable) to modify.
#'
#' @return The input `data.frame` combined with `obs` and `observed` variables.
#'
#' @family postprocess
#' @inheritParams link_dates_with_posterior
link_obs_with_posterior <- function(posterior, obs, horizon, target_types) {
  posterior <- setDT(posterior)
  if (missing(horizon) & !missing(obs)) {
    horizon <- max(posterior[type %in% target_types,
      .(n = .N),
      by = "type"
    ]$n) - length(obs)
  }
  if (!missing(obs)) {
    posterior[type %in% target_types,
      obs := c(obs, rep(NA_real_, horizon)),
      by = "type"
    ]
  }
  if (!missing(horizon)) {
    posterior[type %in% target_types,
      observed := c(
        rep(TRUE, .N - horizon),
        rep(FALSE, horizon)
      ),
      by = "type"
    ]
  }
  posterior[type %in% target_types, forecast_start := FALSE]
  posterior[type %in% target_types & observed == TRUE &
    shift(observed == FALSE, type = "lead"), forecast_start := TRUE]

  setcolorder(posterior,
    neworder = intersect(
      colnames(posterior),
      c("type", "date", "obs", "observed", "forecast_start")
    )
  )
  return(posterior[])
}

#' Summarise the posterior
#'
#' @description A generic wrapper around [posterior::summarise_draws()] with
#' opinionated defaults. See [fv_tidy_posterior()] for a more
#' opinionated wrapper with further cleaning and tidying
#' including linking to observed data, tidying parameter names,
#' and transforming parameters for interpretability.
#'
#' @param fit List of output as returned by [fv_sample()].
#'
#' @param probs A vector of numeric probabilities to produce
#' quantile summaries for. By default these are the 5%, 20%, 80%,
#' and 95% quantiles which are also the minimum set required for
#' plotting functions to work (such as [plot_cases()], [plot_rt()],
#' and [plot_voc()]).
#'
#' @param digits Numeric, defaults to 3. Number of digits to round summary
#' statistics to.
#' 
#' @param ... Additional arguments that may be passed but will not be used.
#'
#' @return A dataframe summarising the model posterior.
#'
#' @family postprocess
#' @export
#' @importFrom purrr reduce
#' @importFrom posterior quantile2 default_convergence_measures
#' @importFrom data.table .SD .N :=
#' @examplesIf interactive()
#' options(mc.cores = 4)
#' obs <- filter_by_availability(
#'   germany_covid19_delta_obs,
#'   date = as.Date("2021-06-12"),
#' )
#' dt <- fv_as_data_list(obs)
#' inits <- fv_inits(dt)
#' fit <- fv_sample(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' fv_posterior(fit)
fv_posterior <- function(fit, probs = c(0.05, 0.2, 0.8, 0.95), digits = 3,
                         ...) {
  check_dataframe(
    fit,
    req_vars = c("fit", "data"),
    req_types = c("list", "list"),
    rows = 1
  )
  # order probs
  probs <- probs[order(probs)]
  # NULL out variables
  variable <- type <- NULL
  # extract useful model info
  fit <- fit$fit[[1]]

  # extract summary parameters of interest and join
  sfit <- list(
    fit$summary(
      variables = NULL, mean, median, sd, mad,
      .args = list(na.rm = TRUE)
    ),
    fit$summary(
      variables = NULL, quantile2,
      .args = list(probs = probs, na.rm = TRUE)
    ),
    fit$summary(
      variables = NULL, posterior::default_convergence_measures(),
      .args = list(na.rm = TRUE)
    )
  )
  cbind_custom <- function(x, y) {
    x <- setDT(x)
    y <- setDT(y)[, variable := NULL]
    cbind(x, y)
  }
  sfit <- purrr::reduce(sfit, cbind_custom)
  ncols <- colnames(sfit)[sapply(sfit, is.numeric)]
  sfit[, (ncols) := lapply(.SD, signif, digits = digits), .SDcols = ncols]
  return(sfit[])
}

#' Summarise the posterior tidily
#'
#' @description A very opinionated wrapper around
#' [posterior::summarise_draws()] with cleaning and tidying including linking
#' to observed data, tidying parameter names, and transforming parameters for
#' interpretability. See [fv_posterior()] for a more generic solution.
#'
#' @param voc_label A character string, default to "VOC". Defines the label
#' to assign to variant of concern specific parameters. Example usage is to
#' rename parameters to use variant specific terminology.
#'
#' @param scale_r Numeric, defaults to 1. Rescale the timespan over which
#' the growth rate and reproduction number is calculated. An example use case
#' is rescaling the growth rate from weekly to be scaled by the mean of
#' the generation time (for COVID-19 for example this would be 5.5 / 7.
#'
#' @return A dataframe summarising the model posterior. Output is stratified
#' by `value_type` with posterior summaries by case, voc, voc advantage vs
#' non-voc over time, rt, growth, model, and the raw posterior summary.
#'
#' @family postprocess
#' @export
#' @inheritParams fv_posterior
#' @importFrom purrr map walk
#' @importFrom data.table .SD .N := setcolorder
#' @examplesIf interactive()
#' options(mc.cores = 4)
#' obs <- filter_by_availability(
#'   germany_covid19_delta_obs,
#'   date = as.Date("2021-06-12"),
#' )
#' dt <- fv_as_data_list(obs)
#' inits <- fv_inits(dt)
#' fit <- fv_sample(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' fv_tidy_posterior(fit)
fv_tidy_posterior <- function(fit, probs = c(0.05, 0.2, 0.8, 0.95),
                              digits = 3, voc_label = "VOC", scale_r = 1) {
  check_dataframe(
    fit,
    req_vars = c("fit", "data"),
    req_types = c("list", "list"),
    rows = 1
  )
  check_param(voc_label, "voc_label", type = "character", length = 1)
  # NULL out variables
  variable <- type <- NULL
  # extract useful model info
  data <- fit$data[[1]]
  case_horizon <- data$t - data$t_nots
  seq_horizon <- data$t - data$t_seq - data$t_nseq

  # extract summary parameters of interest and join
  sfit <- fv_posterior(fit, probs = probs, digits = digits)

  # detect if voc is in the data
  voc_present <- any(grepl("voc", sfit$variable))

  # summarise cases with voc label
  cases <- sfit[grepl("sim_", variable)]
  cases[, type := data.table::fcase(
    grepl("_nvoc", variable), "non-VOC",
    grepl("_voc", variable), "VOC",
    rep(voc_present, .N), "Combined",
    default = "Overall"
  )]
  cases <- link_dates_with_posterior(cases, data)
  cases <- link_obs_with_posterior(
    posterior = cases, obs = data$X,
    target_types = c("Overall", "Combined")
  )
  cases <- link_obs_with_posterior(
    posterior = cases, horizon = seq_horizon,
    target_types = c("VOC", "non-VOC")
  )

  # summarise VOC if present
  voc_frac <- sfit[grepl("frac_voc", variable)]
   voc_frac[, type := "VOC"]
  if (nrow(voc_frac) > 0) {
     voc_frac <- link_dates_with_posterior(voc_frac, data)
     voc_frac <- link_obs_with_posterior(
      posterior = voc_frac, obs = data$Y / data$N,
      target_types = "VOC"
    )
  }

  # summarise Rt and label
  rt <- sfit[grepl("r\\[", variable)]
  rt[, type := fcase(
    grepl("voc_r", variable), "VOC",
    grepl("com_r", variable), "Combined",
    grepl("r\\[", variable) & voc_present, "non-VOC",
    grepl("r\\[", variable), "Overall"
  )]
  rt <- link_dates_with_posterior(rt, data, mod_end = 1)
  rt <- link_obs_with_posterior(
    posterior = rt, horizon = case_horizon,
    target_types = c("Overall", "Combined")
  )
  rt <- link_obs_with_posterior(
    posterior = rt, horizon = seq_horizon,
    target_types = c("VOC", "non-VOC")
  )

  # rescale growth rate to desired range
  cols <- c("mean", "median", paste0("q", probs * 100))
  rt[, (cols) := purrr::map(.SD, ~ . * scale_r),
    .SDcols = cols, by = "type"
  ]

  # copy into growth
  growth <- copy(rt)

  # transform growth to Rt
  rt[, (cols) := lapply(.SD, exp), .SDcols = cols, by = "type"]

  # summarised difference between variants over time
  voc_advantage <- sfit[grepl("voc_advantage", variable)]
  voc_advantage <- voc_advantage[, type := "VOC"]
  if (nrow(voc_advantage) > 0) {
    voc_advantage <- link_dates_with_posterior(voc_advantage, data)
    voc_advantage[,
      (cols) := purrr::map(.SD, ~ exp(. * scale_r)),
      .SDcols = cols, by = "type"
    ]
  }

  # summarise model parameters
  param_lookup <- data.table(
    variable = c(
      "r_init", "r_noise", "beta", "voc_mod", "avg_voc_advantage",
      "voc_noise[1]", "nvoc_noise[1]", "init_cases[1]", "init_cases[2]",
      "phi[1]", "phi[2]", "phi"
    ),
    clean_name = c(
      "Initial growth", "Growth (sd)", "Beta",
      "Initial VOC effect", "Average VOC effect",
      "VOC (sd)", "Non-VOC (sd)", "Initial cases",
      "Initial VOC cases", "Notification overdispersion",
      "Sequencing overdispersion", "Notification overdispersion"
    ),
    exponentiated = c(
      rep(FALSE, 3), rep(TRUE, 2), rep(FALSE, 2),
      rep(TRUE, 2), rep(FALSE, 3)
    )
  )
  model <- merge(param_lookup, sfit, by = "variable")
  model[exponentiated == TRUE, (cols) := lapply(.SD, exp), .SDcols = cols]

  # join output and reorganise as needed
  out <- list(
    model = model,
    cases = cases,
    voc_frac = voc_frac,
    voc_advantage = voc_advantage,
    growth = growth,
    rt = rt,
    raw = sfit
  )

  out <- rbindlist(
    out,
    use.names = TRUE, fill = TRUE, idcol = "value_type"
  )
  setcolorder(
    out,
    c(
      "value_type", "variable", "clean_name", "date", "type",
      "obs", "observed", "forecast_start"
    )
  )
  if (!(voc_label %in% "VOC")) {
    out <- update_voc_label(out, voc_label)
  }
  class(out) <- c("fv_posterior", class(out))
  return(out[])
}

#' Extract forecast dates
#'
#' Extract forecast dates based on the availability of both case
#' and sequence data.
#'
#' @param posterior A dataframe of posterior output as produced by
#'  [fv_tidy_posterior()]. For forecast dates to be extracted data with
#' `value_type == "cases"` must be present.
#'
#' @return A data.frame containing at least two vectors: Data unavailable
#' indicating the type of data that is missing, and date giving the date
#' data was last available for.
#'
#' @family postprocess
#' @export
#' @importFrom purrr map_lgl
#' @examples
#' p <- fv_example(strains = 2, type = "posterior")
#'
#' extract_forecast_dates(p)
extract_forecast_dates <- function(posterior) {
  cases <- posterior[value_type == "cases"][, value_type := NULL]
  non_list_cols <- names(cases)[
    purrr::map_lgl(names(cases), ~ !is.list(cases[[.]]))
  ]
  cases <- cases[, ..non_list_cols]

  if (nrow(cases) == 0 | is.null(cases$forecast_start)) {
    message(
      "Cannot extract forecast dates to plot as case data with a forecast_start variable is not present." # nolint
    )
    dates <- data.table(`Data unavailable` = list(), date = list())
  } else {
    dates <- cases[forecast_start == TRUE]
    cols <- c(
      "variable", "clean_name", "obs", "observed", "forecast_start",
      "exponentiated", "mean", "median", "sd", "mad", "rhat",
      "ess_bulk", "ess_tail", grep("^q[0-9]", names(dates), value = TRUE)
    )
    suppressWarnings(dates[, (cols) := NULL])
    dates[, type := fcase(
      type %in% c("Overall", "Combined"), "Cases",
      !type %in% c("Overall", "Combined"), "Sequences"
    )]
    dates <- unique(dates)
    setnames(dates, "type", "Data unavailable")
    setcolorder(dates, "Data unavailable")
  }
  return(dates[])
}

#' Extract forecasts from a summarised posterior
#'
#'
#' Uses the `observed` variable returned by
#' [fv_tidy_posterior()] to return posterior predictions
#' for forecast dates only.
#'
#' @return A `data.frame` of forecasts in the format returned
#' by [fv_tidy_posterior()] but with fitting variables dropped.
#'
#' @family postprocess
#' @export
#' @inheritParams extract_forecast_dates
#' @examples
#' p <- fv_example(strains = 2, type = "posterior")
#'
#' fv_extract_forecast(p)
fv_extract_forecast <- function(posterior) {
  forecast <- posterior[!(value_type %in% "model")][observed == FALSE]

  cols <- c(
    "obs", "observed", "forecast_date", "rhat", "ess_bulk", "ess_tail",
    "variable", "clean_name", "exponentiated"
  )
  forecast <- suppressWarnings(forecast[, (cols) := NULL])
  forecast <- forecast[, horizon := 1:.N, by = c("value_type", "type")]
  forecast <- setcolorder(
    forecast,
    neworder = c("value_type", "type", "date", "horizon")
  )
  return(forecast[])
}

#' Label the Variant of Concern
#'
#' Assign a custom label to the variant of concern in the
#' output from [fv_tidy_posterior()].
#'
#' @param label Character string  indicating the new label to use for the
#' variant of concern.
#'
#' @param target_label A character string defaulting to "VOC". Indicates the
#' current label for the variant of concern.
#'
#' @return A list of data frames as returned by `[fv_tidy_posterior()] but
#' with updated labels.
#'
#' @family postprocess
#' @inheritParams extract_forecast_dates
#' @export
#' @examples
#' p <- fv_example(strains = 2, type = "posterior")
#' p <- update_voc_label(p, "Delta")
#' p[value_type == "model"]
update_voc_label <- function(posterior, label, target_label = "VOC") {
  if (!missing(label)) {
    stopifnot(is.character(label))
    replace_label <- function(dt) {
      char_cols <- names(Filter(
        function(f) {
          any(class(f) %in% c("character", "factor"))
        },
        dt
      ))
      dt <- dt[,
        (char_cols) := purrr::map(
          .SD,
          ~ gsub(target_label,
            replacement = label, x = .,
            ignore.case = FALSE
          )
        ),
        .SDcols = char_cols
      ]
    }
    posterior <- replace_label(posterior)
  }
  return(posterior[])
}

#' Extract posterior draws
#'
#' @param fit A list as produced by [fv_sample()].
#'
#' @param ... Additional parameters passed to [cmdstanr::draws()]
#'
#' @return A [cmdstanr::draws()] object from the `posterior` package.
#'
#' @family postprocess
#' @examplesIf interactive()
#' obs <- filter_by_availability(
#'   germany_covid19_delta_obs,
#'   date = as.Date("2021-06-12"),
#' )
#' dt <- fv_as_data_list(obs)
#' inits <- fv_inits(dt)
#' fit <- fv_sample(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' extract_draws(fit)
extract_draws <- function(fit, ...) {
  fit$fit[[1]]$draws(...)
}

#' Convert summarised quantiles from wide to long format
#'
#' @param posterior A dataframe as output by [fv_tidy_posterior()],
#' [fv_extract_forecast()], etc.
#'
#' @return A data frame of quantiles in long format.
#'
#' @family postprocess
#' @export
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#' long_posterior <- quantiles_to_long(posterior)
#' long_posterior
quantiles_to_long <- function(posterior) {
  long <- melt(posterior,
    measure.vars = patterns("^q[0-9]"),
    value.name = "prediction", variable.name = "quantile"
  )
  long[, quantile := gsub("q", "", quantile)]
  long[, quantile := as.numeric(quantile) / 100]
  return(long[])
}

#' Convert to stanfit object
#'
#' @return The model fit as a `stanfit` object
#'
#' @family postprocess
#' @inheritParams fv_tidy_posterior
#' @importFrom rstan read_stan_csv
#' @examplesIf interactive()
#' obs <- filter_by_availability(
#'   germany_covid19_delta_obs,
#'   date = as.Date("2021-06-12"),
#' )
#' dt <- fv_as_data_list(obs)
#' inits <- fv_inits(dt)
#' fit <- fv_sample(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' convert_to_stanfit(fit)
convert_to_stanfit <- function(fit) {
  stanfit <- read_stan_csv(fit$fit[[1]]$output_files())
  return(stanfit)
}
