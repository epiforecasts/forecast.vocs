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
