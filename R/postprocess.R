#' @export
summarise_posterior <- function(fit, cases, ...) {
  variable <- Type <- NULL
  sfit <- fit$summary(...)
  sfit <- data.table::setDT(sfit)

  start_date <- min(cases$date)

  cases <- sfit[grepl("sim_", variable)]
  cases[, date := rep(seq(start_date, by = "week", length.out = .N / 3), 3)]
  cases[, Type := data.table::fcase(
                      grepl("_ndelta", variable), "non-DELTA",
                      grepl("_delta", variable), "DELTA",
                      default = "Overall"
        )]

  delta <- sfit[grepl("frac_delta", variable)]
  delta[, date := seq(start_date, by = "week", length.out = .N)]

  rt <- sfit[grepl("r\\[", variable)]
  rt[, date := rep(seq(start_date, by = "week",
                             length.out = .N / 2 - 1), 2)]
  rt[, Type := fcase(
    grepl("delta_r", variable), "DELTA",
    grepl("r\\[", variable), "non-DELTA"
  )]
  cols <- c("mean", "median", "q5", "q95")
  rt[, (cols) := lapply(.SD, exp), .SDcols = cols, by = "Variant"]

  out <- list(cases = cases, delta = delta, rt = rt)
  return(out)
}