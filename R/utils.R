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

#' Update data based on availability and forecast date
#' @param cases A data frame with the following variables:
#'  date, cases, seq_delta, and seq_total.
#' @param forecast_date Date at which to forecast. Defaults to the
#' maximum date in `cases`.
#' @param cases_lag Number of weeks that case data takes to be reported.
#' Defaults to not alter the input data.
#' @param seq_lag Number of weeks that sequence data takes to be reported.
#' Defaults to not alter the input data.
#' @export
#' @importFrom purrr map
#' @examples
#' update_data_availability(germany_cases, cases_lag = 2, seq_lag = 3)
update_data_availability <- function(cases,
                                     forecast_date = max(cases$date),
                                     cases_lag, seq_lag) {
  cases <- as.data.table(cases)
  cases <- copy(cases)
  if (!missing(cases_lag)) {
    if (!is.null(cases_lag)) {
      cases[, cases_available := date + cases_lag * 7]
    }
  }

  if (!missing(seq_lag)) {
    if (!is.null(seq_lag)) {
      cases[, seq_available := date + seq_lag * 7]
    }
  }

  # filter for target date and data availability
  cases[date <= forecast_date]
  cases[cases_available > forecast_date, cases := NA]
  cols <- c("seq_total", "seq_delta", "share_delta")
  cases[seq_available > forecast_date, (cols) := purrr::map(.SD, ~NA),
    .SDcols = cols
  ]
  return(cases)
}
