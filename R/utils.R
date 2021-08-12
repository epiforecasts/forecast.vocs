#' @rawNamespace import(data.table, except = transpose)
#' @import cmdstanr
#' @import ggplot2
#' @importFrom stats median rnorm
NULL

#' Check Quantiles Required are Present
#' @export
check_quantiles <- function(posterior, req_probs = c(0.5, 0.95, 0.2, 0.8)) {
  cols <- colnames(posterior)
  if (sum(cols %in% c("q5", "q95", "q20", "q80")) != 4) {
    stop(
      "Following quantiles must be present (set with probs): ",
      paste(req_probs, collapse = ", ")
    )
  }
  return(invisible(NULL))
}

#' Convert summarised quantiles from wide to long format
#'
#' @param posterior A dataframe as output by `summarise_posterior()`,
#' extract_forecast(), combine_posteriors_dt(), etc.
#' @export
#' @return A data frame of quantiles in long format.
#' @examples
#' \dontrun{
#' options(mc.cores = 4)
#' dt <- forecast_dt(latest_obs(germany_obs), max_treedepth = 15)
#' dt <- combine_posteriors_dt(dt, target = "forecast")
#' long <- quantiles_to_long(dt)
#' print(long)
#' }
quantiles_to_long <- function(posterior) {
  posterior <- copy(posterior)
  long <- melt(posterior,
    measure.vars = patterns("^q[0-9]"),
    value.name = "prediction", variable.name = "quantile"
  )
  long[, quantile := gsub("q", "", quantile)]
  long[, quantile := as.numeric(quantile) / 100]
  return(long)
}
