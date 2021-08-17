
#' Forecast using branching processes at a target date returning a
#' summary data frame
#'
#' @description Designed to streamline workflows with a large number of
#' indepedent model fits this function returns a data frame summarising
#' a forecast for a single model (rather than the multiple models that may
#' be used directly with `forecast_dt()`). Unlike `forecast()` it is also
#' error tolerant and returns the error if forecasting fails.
#' @inheritParams forecast
#' @param model A model as loaded by `load_model()`.
#' @param id An integer, defaulting to 0 to identify this forecast.
#' @param keep_forecast Logical, defaults to `FALSE`. Should the `forecast()`
#' output be kept.
#' @param ... Additional arguments passed to `forecast()`
#' @importFrom purrr safely
#' @return A dataframe containing the output of `forecast()` in each row as
#' well as summary information about the forecast
#' @export
#' @examples
#' \dontrun{
#' options(mc.cores = 4)
#' dt <- forecast_dt(latest_obs(germany_obs), max_treedepth = 15)
#' print(dt)
#' }
forecast_dt <- function(obs,
                        forecast_date = max(obs$date),
                        strains = 1,
                        overdispersion = TRUE,
                        variant_relationship = "pooled",
                        model = bp.delta::load_model(strains = strains),
                        keep_forecast = FALSE, id = 0, ...) {
  if (length(strains) > 1) {
    stop("forecast_dt only supports fitting a single strain model at one time")
  }
  safe_forecast <- purrr::safely(forecast)
  obj <- do.call(
    safe_forecast,
    c(
      list(...),
      list(
        obs = obs, overdispersion = overdispersion,
        variant_relationship = variant_relationship,
        models = list(model), strains = strains
      )
    )
  )

  dt <- data.table(
    id = id,
    forecast_date = forecast_date,
    strains = strains,
    overdispersion = overdispersion,
    variant_relationship = variant_relationship,
    forecast = list(obj$result$models[[1]]$forecast),
    posterior = list(obj$result$models[[1]]$tidy_posterior),
    fit = list(obj$result$models[[1]]$fit),
    data = list(obj$result$models[[1]]$data),
    obj$result$models[[1]]$diagnostics,
    error = obj$error
  )
  if (keep_forecast) {
    dt[, forecast_obj = obj$result]
  }
  return(dt)
}

#' Combine posteriors from a data frame
#'
#' A wrapper for `combine_posteriors()` but designed for use with the output
#' from `forecast_dt()`. Can be used to target either summarised posterior
#' estimates or forecast estimates.
#'
#' @param forecasts A data frame of forecasts as produced by `forecast_dt()`
#' @param target A character string indicating the list of posteriors to
#' combine. This can be either "posterior", or "forecast".
#'
#' @return A data frame of summmarised posteriors along with summary
#' information about the forecast method used.
#' @export
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' options(mc.cores = 4)
#' dt <- forecast_dt(latest_obs(germany_obs), max_treedepth = 15)
#' dt <- combine_posteriors_dt(dt, target = "forecast")
#' }
combine_posteriors_dt <- function(forecasts, target = "forecast") {
  target <- match.arg(target, choices = c("posterior", "forecast"))
  posteriors <- copy(forecasts)[
    ,
    target := purrr::map(
      get(target),
      ~ combine_posteriors(list(.), combine_variables = TRUE, list_id = "model")
    )
  ]
  posteriors <- posteriors[,
    rbindlist(target),
    by = c(
      "id", "forecast_date", "strains", "overdispersion",
      "variant_relationship"
    )
  ]
  posteriors[, model := NULL]
  return(posteriors)
}
