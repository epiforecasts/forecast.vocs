#' Evaluate forecasts using proper scoring rules
#'
#' Acts as a wrapper to [scoringutils::score()]. In particular,
#' handling filtering the output for various [forecast.vocs] functions and
#' linking this output to observed data. See the documentation for the
#' `scoringutils` package for more on forecast scoring and the documentation
#' and examples below for simple examples in the context of [forecast.vocs].
#' Internally name clashes between [scoringutils] variables and [forecast.vocs]
#' variables are handled.
#'
#' @param forecast A posterior forecast or posterior prediction as returned by
#' [summary.fv_posterior()], [summary.fv_forecast()] or [fv_extract_forecast()].
#' Internally case forecasts are filtered for using the `value_type` variable
#' if present as are only overall or combined case counts (i.e as returned)
#' by the 1 and 2 strain models. If looking for more complex scoring it may be
#' wise to implement a custom wrapper.
#'
#' @param log Logical, defaults to FALSE. Should scores be calculated on the
#' log scale (with a 0.01 shift) for both observations and forecasts. Scoring in
#' this way can be thought of as a relative score vs the more usual absolute
#' measure. It may be useful when targets are on very different scales or when
#' the forecaster is more interested in good all round performance versus good
#' performance for targets with large values.
#'
#' @param check Logical, defaults to FALSE. Should
#' [scoringutils::check_forecasts()] be used to check input forecasts.
#'
#' @param round_to Integer defaults to 3. Number of digits to round scoring
#' output to.
#'
#' @param ... Additional arguments passed to [scoringutils::score()].
#'
#' @return A `data.table` as returned by [scoringutils::score()].
#'
#' @inheritParams plot_default
#' @family modelvalidation
#' @importFrom data.table copy setnames
#' @export
#' @examplesIf interactive()
#' options(mc.cores = 4)
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
#' # scoreon the absolute scale
#' scores <- fv_score_forecast(forecasts, obs)
#' summarise_scores(scores, by = "strains")
#'
#' # score overall on a log scale
#' log_scores <- fv_score_forecast(forecasts, obs, log = TRUE)
#' summarise_scores(log_scores, by = "strains")
#'
#' # score by horizon
#' summarise_scores(scores, by = c("strains", "horizon"))
#'
#' # score by horizon on a log scale
#' summarise_scores(log_scores, by = c("strains", "horizon"))
fv_score_forecast <- function(forecast, obs, log = FALSE, check = TRUE,
                              round_to = 3, ...) {
  forecast <- data.table::as.data.table(forecast)
  if (!requireNamespace("scoringutils")) {
    stop("scoringutils is required for this function to work")
  }
  if (!is.null(forecast$value_type)) {
    forecast <- forecast[value_type == "cases"]
  }
  if (!is.null(forecast$type)) {
    forecast <- forecast[type %in% c("Overall", "Combined")]
  }
  if (!is.null(forecast$overdispersion)) {
    data.table::setnames(forecast, "overdispersion", "obs_overdispersion")
  }
  long_forecast <- quantiles_to_long(forecast)
  latest_obs <- data.table::as.data.table(obs)
  data.table::setnames(latest_obs, "cases", "true_value", skip_absent = TRUE)
  cols <- intersect(colnames(forecast), colnames(latest_obs))
  long_forecast <- merge(long_forecast, latest_obs, by = cols)

  if (log) {
    cols <- c("true_value", "prediction")
    long_forecast[, (cols) := purrr::map(.SD, ~ log(. + 0.01)), .SDcols = cols]
  }

  long_forecast[, model := strains]

  if (check) {
    scoringutils::check_forecasts(long_forecast)
  }

  scores <- scoringutils::score(long_forecast, ...)
  numeric_cols <- colnames(scores)[sapply(scores, is.numeric)]
  scores <- scores[, (numeric_cols) := lapply(.SD, signif, digits = round_to),
    .SDcols = numeric_cols
  ]
  return(scores[])
}
