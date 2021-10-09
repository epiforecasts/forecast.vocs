#' Forecast across multiple dates
#'
#' @param forecast_dates A list of dates to forecast at.
#'
#' @param ... Additional parameters passed to `forecast()`
#'
#' @return A data table each row containing the output from running
#' `forecast()` on a single forecast date.
#'
#' @family forecast
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
#' @family forecast
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

#' Unnest posterior estimates from a forecast data.frame
#'
#'
#' @param forecasts A data frame of forecasts as produced by `forecast()`.
#'
#' @param target A character string indicating the list of outpuuts to
#' unnest. This can currently be either "posterior", or "forecast".
#'
#' @return An unnested data.frame of posterior estimates and other variables
#' produced by `forecast()`.
#'
#' @family forecast
#' @export
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' library(data.table)
#' options(mc.cores = 4)
#' dt <- forecast(latest_obs(germany_covid19_delta_obs), max_treedepth = 15)
#'
#' # unnest posterior predictions
#' posterior <- unnest_posterior(dt, target = "posterior")
#' posterior
#'
#' # unnest forecasts
#' forecasts <- unnest_posterior(dt, target = "forecast")
#' forecasts
#' }
unnest_posterior <- function(forecasts, target = "posterior") {
  target <- match.arg(target, choices = c("posterior", "forecast"))
  forecasts <- copy(forecasts)[, row_id := 1:.N]

  targets <- forecasts[,
    rbindlist(get(target), fill = TRUE),
    by = row_id
  ]
  forecasts <- merge(forecasts, targets, all.x = TRUE, by = "row_id")
  forecasts <- forecasts[, c(target, "row_id") := NULL]
  return(forecasts[])
}
