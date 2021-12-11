#' Forecast across multiple dates
#'
#' @param forecast_dates A list of dates to forecast at.
#'
#' @param ... Additional parameters passed to [forecast()].
#'
#' @return A `data.table` each row containing the output from running
#' [forecast()] on a single forecast date.
#'
#' @family forecast
#' @inheritParams forecast
#' @importFrom future.apply future_lapply
#' @export
#' @examplesIf interactive()
#' library(ggplot2)
#' options(mc.cores = 4)
#'
#' forecasts <- forecast_across_dates(
#'   germany_covid19_delta_obs,
#'   forecast_dates = c(as.Date("2021-05-01"), as.Date("2021-06-12")),
#'   horizon = 4,
#'   strains = 2,
#'   adapt_delta = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#'
#' # inspect forecasts
#' forecasts
#'
#' # unnest posteriors
#' posteriors <- unnest_posterior(forecasts)
#'
#' # plot case posterior predictions
#' plot_cases(posteriors, log = TRUE) +
#'   facet_grid(vars(forecast_date), vars(voc_scale))
forecast_across_dates <- function(obs,
                                  forecast_dates = unique(obs[!is.na(seq_available)])$date[-c(1:3)], # nolint
                                  ...) {
  fits <- future.apply::future_lapply(
    forecast_dates,
    function(date, ...) {
      forecast(obs, forecast_date = date, ...)
    },
    ...,
    future.seed = TRUE
  )
  fits <- rbindlist(fits, fill = TRUE)
  return(fits[])
}

#' Forecast across multiple scenarios and dates
#'
#' @param scenarios A `data.frame` of scenarios as produced by
#' [define_scenarios()]. If an `obs` variable is present this is
#' used as the scenario data but otherwise [generate_obs_scenario()]
#' is used to generate this data from the other variables in `scenarios`.
#'
#' @param ... Additional parameters passed to [forecast_across_dates()].
#'
#' @return A data table each rows containing the output from running
#' [forecast()] on a single scenario for a single forecast date.
#'
#' @family forecast
#' @inheritParams forecast_across_dates
#' @importFrom purrr map2
#' @importFrom future.apply future_lapply
#' @export
#' @examplesIf interactive()
#' library(ggplot2)
#' options(mc.cores = 4)
#'
#' scenarios <- define_scenarios(
#'   voc_scale = list(c(0, 0.5), c(0.5, 0.25)),
#'   seq_lag = 1,
#'   seq_samples = 1
#' )
#' scenarios
#'
#' forecasts <- forecast_across_scenarios(
#'   germany_covid19_delta_obs,
#'   scenarios,
#'   forecast_dates = c(as.Date("2021-05-01"), as.Date("2021-06-12")),
#'   horizon = 4,
#'   strains = 2,
#'   adapt_delta = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#'
#' # inspect forecasts
#' forecasts
#'
#' # unnest posteriors
#' posteriors <- unnest_posterior(forecasts)
#'
#' # plot case posterior predictions
#' plot_cases(posteriors, log = TRUE) +
#'   facet_grid(vars(forecast_date))
forecast_across_scenarios <- function(obs, scenarios, ...) {
  if (is.null(scenarios[["obs"]])) {
    scenarios$obs <- purrr::map2(
      scenarios$seq_lag, scenarios$seq_samples,
      ~ generate_obs_scenario(obs, seq_lag = .x, seq_samples = .y)
    )
  }
  scenarios <- split(scenarios, by = "id")

  forecast_scenario <- function(scenario, ...) {
    forecast_across_dates(
      obs = scenario$obs[[1]],
      voc_scale = scenario$voc_scale[[1]],
      id = scenario$id[[1]],
      ...
    )
  }
  fits <- future.apply::future_lapply(
    scenarios,
    forecast_scenario,
    ...,
    future.seed = TRUE
  )
  fits <- rbindlist(fits, fill = TRUE)
  return(fits[])
}

#' Unnest posterior estimates from a forecast data.frame
#'
#' @description Unnest posterior predictions and forecasts from output
#' produced by [forecast()] (or multiple combined calls) dropping diagnostic
#' and fitting variables in the process.
#'
#' @param forecasts A data frame of forecasts as produced by [forecast()].
#'
#' @param target A character string indicating the list of outputs to
#' unnest.
#'
#' @return An unnested `data.frame` of posterior estimates and other variables
#' produced by [forecast()].
#'
#' @family forecast
#' @export
#' @importFrom purrr map
#' @examplesIf interactive()
#' library(data.table)
#' options(mc.cores = 4)
#' dt <- forecast(
#'   germany_covid19_delta_obs,
#'   forecast_date = as.Date("2021-06-12"),
#'   max_treedepth = 15, adapt_delta = 0.99
#' )
#'
#' # unnest posterior predictions
#' posterior <- unnest_posterior(dt)
#' posterior
#'
#' # unnest forecasts
#' forecasts <- unnest_posterior(dt, target = "forecast")
#' forecasts
unnest_posterior <- function(forecasts, target = "posterior") {
  forecasts <- copy(forecasts)[, row_id := 1:.N]

  targets <- forecasts[,
    rbindlist(
      map(get(target), as.data.table), fill = TRUE
    ),
    by = row_id
  ]
  forecasts <- merge(forecasts, targets, all.x = TRUE, by = "row_id")
  forecasts <- forecasts[, c(target, "row_id") := NULL]
  dcols <- setdiff(
    c("posterior", "forecast", "fit", "data", "fit_args",
      "samples", "max_rhat", "divergent_transitions",
      "per_divergent_transitions", "max_treedepth",
      "no_at_max_treedepth", "per_at_max_treedepth", "time", "error"
    ),
    target
  )
  suppressWarnings(forecasts[, (dcols) := NULL])
  class(forecasts) <- c("fv_posterior", class(forecasts))
  return(forecasts[])
}
