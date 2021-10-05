#' Link dates by time for posterior parameter estimates
#' @param posterior A data frame of summarised posterior estimates
#' as returned by `cmdstanr::summary` with  an additional type variable
#' which contains the following character string options: "non-VOC",
#' "VOC", "Combined", "Overall".
#' @param data A list of data as returned in the "data" entry of the output
#' returned by `stan_fit()`.
#' @param mod_end Integer, defaults to 0. Amount to shift the end date of
#' estimates
#' @export
#' @return A dataframe with an additional data column
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
#' @param horizon Integer indicating the horizon of unobserved forecasts. If not
#' specified will be inferred from `obs`.
#' @param target_types A character vector of types (as specified in the `type`
#' variable) to modify
#' @inheritParams link_dates_with_posterior
#' @return The input data.frame combined with `obs` and `observed` variables.
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
  setcolorder(posterior,
    neworder = intersect(
      colnames(posterior),
      c("type", "date", "obs", "observed")
    )
  )
  return(posterior)
}

#' Summarise the posterior
#'
#' @param fit List of output as returned by `stan_fit()`.
#' @param probs A vector of numeric probabilities to produce
#' quantile summaries for.
#' @export
#' @importFrom purrr reduce map walk
#' @importFrom posterior quantile2 default_convergence_measures
#' @importFrom data.table .SD .N := setcolorder
#' @examples
#' \dontrun{
#' dt <- stan_data(latest_obs(germany_covid19_delta_obs))
#' inits <- stan_inits(dt)
#' options(mc.cores = 4)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' summarise_posterior(fit)
#' }
summarise_posterior <- function(fit,
                                probs = c(
                                  0.01, 0.025,
                                  seq(0.05, 0.95, by = 0.05),
                                  0.975, 0.99
                                )) {
  check_dataframe(
    fit,
    req_vars = c("fit", "data"),
    req_types = c("list", "list"),
    rows = 1
  )

  # NULL out variables
  variable <- type <- NULL
  # extract useful model info
  data <- fit$data[[1]]
  fit <- fit$fit[[1]]
  case_horizon <- data$t - data$t_nots
  seq_horizon <- data$t - data$t_seq - data$t_nseq

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
  voc <- sfit[grepl("frac_voc", variable)]
  voc[, type := "VOC"]
  if (nrow(voc) > 0) {
    voc <- link_dates_with_posterior(voc, data)
    voc <- link_obs_with_posterior(
      posterior = voc, obs = data$Y / data$N,
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
  # copy into growth
  growth <- copy(rt)

  # transform growth to Rt
  cols <- c("mean", "median", paste0("q", probs * 100))
  rt[, (cols) := lapply(.SD, exp), .SDcols = cols, by = "type"]

  # summarise model parameters
  param_lookup <- data.table(
    variable = c(
      "r_init", "r_noise", "beta", "voc_mod", "avg_voc_mod",
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
    voc = voc,
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
      "obs", "observed"
    )
  )
  return(out[])
}

#' Combine multiple summarised posteriors
#'
#' @param posteriors_list A list of posteriors as produced by
#'  `summarise_posterior()`.
#' @param list_id A character string naming the variable used to identify
#' list parameters
#' @param ids A character vector of the same lebngth
#' @export
#' @importFrom purrr map
combine_posteriors <- function(posteriors_list, list_id = "model", ids) {
  if (!missing(ids)) {
    names(posteriors_list) <- ids
  }
  posteriors <- rbindlist(
    posteriors_list,
    use.names = TRUE, fill = TRUE, idcol = list_id
  )
  return(posteriors)
}

#' Extract forecast dates
#'
#' Extract forecast dates based on the availability of both case
#' and sequence data. Custom forecasts dates can also be defined and the
#' automated forecasts can be overridden as desired.
#'
#' @param posterior A list of posterior output as produced by
#'  `summarise_posterior()`.
#' @param forecast_dates A named vector of dates to use to identify when
#' output is a forecast vs an estimate. Defaults to empty in which case
#' forecast dates are inferred from the `posterior` list based on data
#' availability for cases and sequences. These dates can be overridden
#'  by supplying a replacement data with a duplicate name (see the examples).
#' @export
#' @return A named vector of dates.
#' @examples
#' \dontrun{
#' options(mc.cores = 4)
#' obs <- latest_obs(germany_covid19_delta_obs)
#' dt <- stan_data(obs, overdispersion = FALSE)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, max_treedepth = 15, adapt_delta = 0.9)
#' p <- summarise_posterior(fit)
#' # default
#' extract_forecast_dates(p)
#'
#' # add a custom date
#' extract_forecast_dates(p, c("custom" = "2021-08-01"))
#'
#' # overwrite a date
#' extract_forecast_dates(p, c("Cases" = "2021-08-01"))
#' }
extract_forecast_dates <- function(posterior, forecast_dates = NULL) {
  dates <- NULL
  cases <- posterior[value_type == "cases"]
  if (!is.null(cases[["observed"]])) {
    dates <- suppressWarnings(
      c(
        cases[
          observed == TRUE & type %in% c("Combined", "Overall"),
          .(date = max(date))
        ]$date[1],
        cases[
          observed == TRUE & !(type %in% c("Combined", "Overall")),
          .(date = max(date))
        ]$date[1]
      )
    )
    names(dates) <- c("Cases", "Sequences")
  }

  if (!is.null(forecast_dates)) {
    date_names <- names(forecast_dates)
    forecast_dates <- as.Date(forecast_dates)
    names(forecast_dates) <- date_names
    if (is.null(dates)) {
      dates <- forecast_dates
    } else {
      dates <- c(
        forecast_dates,
        dates[setdiff(names(dates), names(forecast_dates))]
      )
    }
  }
  return(dates)
}

#' Extract forecasts from a posterior data frame by type
#'
#' @inheritParams link_dates_with_posterior
#' @inheritParams extract_forecast
#' @return A data frame of forecasts from the posterior data frame.
extract_forecast_by_type <- function(posterior, forecast_dates) {
  posterior <- rbind(
    posterior[
      type %in% c("Overall", "Combined") & date > forecast_dates["Cases"]
    ],
    posterior[
      !(type %in% c("Overall", "Combined")) & date > forecast_dates["Sequences"]
    ]
  )
  return(posterior)
}

#' Extract forecasts from a summarised posterior
#'
#'
#' @param forecast_dates A named vector of dates to use to identify when
#' output is a forecast. Must contain a "Cases" date and a "Sequence" date.
#' Default is to infer these dates from the summarised posterior.
#' @inheritParams extract_forecast_dates
#' @return A list containing a forecast for each parameter
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_covid19_delta_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' p <- summarise_posterior(fit)
#' extract_forecast(p)
#' }
extract_forecast <- function(posterior, forecast_dates = NULL) {
  if (!is.null(forecast_dates)) {
    names(forecast_dates) <- match.arg(names(forecast_dates),
      c("Sequences", "Cases"),
      several.ok = TRUE
    )
    if (length(forecast_dates) != 2) {
      stop("forecast_dates must contain a date for both cases and sequences")
    }
  }
  forecast_dates <- extract_forecast_dates(
    posterior,
    forecast_dates = forecast_dates
  )

  forecast <- extract_forecast_by_type(
    posterior[!(value_type %in% "model")], forecast_dates
  )

  cols <- c(
    "obs", "observed", "rhat", "ess_bulk", "ess_tail",
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
#' output from `summarise_posterior()`.
#'
#' @inheritParams extract_forecast_dates
#' @param label Character string  indicating the new label to use for the
#' variant of concern.
#' @param target_label A character string defaulting to "VOC". Indicates the
#' current label for the variant of concern.
#' @return A list of data frames as returned by `summarise_posterior()` but
#' with updated labels.
#' @export
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_covid19_delta_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' p <- summarise_posterior(fit)
#' p <- update_voc_label(p, "Delta")
#' p[value_type == "model"][]
#' }
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
  return(posterior)
}
#' Extract posterior draws
#'
#' @param fit A list as produced by `stan_fit()`.
#' @param ... Additional parameters passed to `cmdstanr::draws`
#'
#' @return A `draws` object from the `posterior` package.
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_covid19_delta_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' extract_draws(fit)
#' }
extract_draws <- function(fit, ...) {
  fit$fit$draws(...)
}

#' Convert to stanfit object
#'
#' @inheritParams summarise_posterior
#' @return The model fit as a stanfit object
#' @importFrom rstan read_stan_csv
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_covid19_delta_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' convert_to_stanfit(fit)
#' }
convert_to_stanfit <- function(fit) {
  stanfit <- read_stan_csv(fit$fit$output_files())
  return(stanfit)
}

#' Launch shinystan
#'
#' Launch shinystan an interactive tool for stan model evaluation
#'
#' @inheritParams summarise_posterior
#' @return NULL
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_covid19_delta_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' bp_launch_shinystan(fit)
#' }
bp_launch_shinystan <- function(fit) {
  requireNamespace("shinystan", quietly = TRUE)
  stanfit <- convert_to_stanfit(fit)
  shinystan::launch_shinystan(stanfit)
  return(invisible(NULL))
}
