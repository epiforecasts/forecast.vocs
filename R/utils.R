#' @rawNamespace import(data.table, except = transpose)
#' @import cmdstanr
#' @import ggplot2
#' @importFrom stats median rnorm
NULL

#' Convert summarised quantiles from wide to long format
#'
#' @param posterior A dataframe as output by `summarise_posterior()`,
#' extract_forecast(), combine_posteriors_dt(), etc.
#' @export
#' @return A data frame of quantiles in long format.
#' @examples
#' \dontrun{
#' options(mc.cores = 4)
#' dt <- forecast_dt(latest_obs(germany_covid19_delta_obs), max_treedepth = 15)
#' dt <- combine_posteriors_dt(dt, target = "forecast")
#' long <- quantiles_to_long(dt)
#' print(long)
#' }
quantiles_to_long <- function(posterior) {
  long <- melt(posterior,
    measure.vars = patterns("^q[0-9]"),
    value.name = "prediction", variable.name = "quantile"
  )
  long[, quantile := gsub("q", "", quantile)]
  long[, quantile := as.numeric(quantile) / 100]
  return(long)
}
