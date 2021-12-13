#' Evaluate forecasts using proper scoring rules
#'
#' Acts as a wrapper t [scoringutils::eval_forecasts()]. In particular,
#' handling filtering the output for various [forecast.vocs] functions and
#' linking this output to observed data. See the documentation for the
#' [scoringutils] package for more on forecast scoring and the documentation
#' and examples below for simple examples in the context of [forecast.vocs].
#'
#' @param forecast A posterior forecast or posterior prediction as returned by
#'[summary.fv_posterior()], [summary.fv_forecast()] or [fv_extract_forecast()].
#' Internally case forecasts are filtered for using the `value_type` variable
#' if present as are only overall or combined case counts (i.e as returned)
#' by the 1 and 2 strain models. If looking for more complex scoring it may be 
#' wise to implement a custom wrapper.
#'
#' @param log Logical, defaults to FALSE. Should scores be calculated on the
#' log scale (with a 0.01 shift) for both observatiosn and forecasts. Scoring in
#' this way can be thought of as a relative score vs the more usual absolute
#' measure. It may be useful when targets are on very different scales or when
#' the forecaster is more interested in good all round performance versus good
#' performance for targets with large values.
#'
#' @param round_to Integer defaults to 3. Number of digits to round scoring
#' output to.
#'
#' @param ... Additional arguments passed to [scoringutils::eval_forecasts()].
#'
#' @return A `data.table` as returned by [scoringutils::eval_forecasts()].
#'
#' @inheritParams plot_default
#' @family modelvalidation
#' @importFrom data.table copy setnames
#' @export
#' @examplesIf interactive()
#' library(data.table)
#' library(scoringutils)
#'
#' # Fit and forecast using both the one and two strain models
#' forecasts <- forecast(
#'   germany_covid19_delta_obs,
#'   forecast_date = as.Date("2021-06-12"),
#'   horizon = 4,
#'   strains = c(1, 2),
#'   adapt_delta = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#'
#' # Extract forecasts
#' forecasts <- summary(forecasts, target = "forecast", type = "cases")
#'
#' # Filter for the latest available observations
#' obs <- latest_obs(germany_covid19_delta_obs)
#'
#' # score overall
#' fv_score_forecast(forecasts, obs, summarise_by = "strain")
#'
#' # score overall on a log scale
#' fv_score_forecast(forecasts, obs, summarise_by = "strain", log = TRUE)
#'
#' # score by horizon
#' fv_score_forecast(forecasts, obs, summarise_by = c("strain", "horizon"))
#'
#' # score by horizon on a log scale
#' fv_score_forecast(
#'  forecasts, obs, summarise_by = c("strain", "horizon"), log = TRUE
#' )
fv_score_forecast <- function(forecast, obs, log = FALSE,
                              round_to = 3, ...) {
  if (!requireNamespace("scoringutils")) {
    stop("scoringutils is required for this function to work")
  }
  if (!is.null(forecast$value_type)) {
    forecast <- forecast[value_type == "cases"]
  }
  if (!is.null(forecast$type)) {
    forecast <- forecast[type %in% c("Overall", "Combined")]
  }
  long_forecast <- quantiles_to_long(forecast)
  latest_obs <- data.table::copy(obs)
  data.table::setnames(latest_obs, "cases", "true_value", skip_absent = TRUE)
  cols <- intersect(colnames(forecast), colnames(latest_obs))
  long_forecast <- merge(long_forecast, latest_obs, by = cols)

  if (log) {
    cols <- c("true_value", "prediction")
    long_forecast[, (cols) := purrr::map(.SD, ~ log(. + 0.01)), .SDcols = cols]
  }

  scores <- scoringutils::eval_forecasts(long_forecast, ...)
  numeric_cols <- colnames(scores)[sapply(scores, is.numeric)]
  scores <- scores[, (numeric_cols) := lapply(.SD, signif, digits = round_to),
    .SDcols = numeric_cols
  ]
  return(scores[])
}
