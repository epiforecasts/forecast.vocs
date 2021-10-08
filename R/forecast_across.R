#' Forecast across multiple dates
#'
#' @param forecast_dates A list of dates to forecast at.
#'
#' @param ... Additional parameters passed to `forecast()`
#'
#' @return A data table each row containing the output from running
#' `forecast()` on a single forecast date.
#'
#' @concept forecast
#' @inheritParams forecast
#' @importFrom future.apply future_lapply
#' @export
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
  return(fits)
}

#' Forecast across multiple scenarios and dates
#'
#' @param scenarios A dataframe of scenarios as produced by
#' `define_scenarios()`. If missing uses the default scenarios
#' from `default_scenarios()`.
#'
#' @param ... Additional parameters passed to `forecast_across_dates()`.
#'
#' @return A data table each rows containing the output from running
#' `forecast()` on a single scenario for a single forecast date.
#'
#' @concept forecast
#' @inheritParams forecast_across_dates
#' @importFrom purrr map2
#' @importFrom future.apply future_lapply
#' @export
forecast_across_scenarios <- function(obs, scenarios, ...) {
  if (missing(scenarios)) {
    scenarios <- forecast.vocs::define_scenarios()
  }
  scenarios$obs <- purrr::map2(
    scenarios$seq_lag, scenarios$seq_samples,
    ~ generate_obs_scenario(obs, seq_lag = .x, seq_samples = .y)
  )
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
  return(fits)
}
