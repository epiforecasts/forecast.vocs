#' @export
#' @importFrom purrr reduce
#' @examples
#' \dontrun{
#' dt <- stan_data(germany_cases)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99)
#' summarise_posterior(fit)
#' }
summarise_posterior <- function(fit) {
  # extract useful model info
  data <- fit$data
  t <- data$t
  start_date <- data$start_date
  fit <- fit$fit
  variable <- Type <- NULL

  #sfit <- list(
  #  fit$summary(variables = NULL, mean, median, sd, mad),
  #  fit$summary(variables = NULL, quantile2,
  #              .args = list(probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05),
  #                                     0.975, 0.99)))
  #)
  #sfit <- purrr::reduce(sfit, merge, by = "variable")
  sfit <- fit$summary()
  sfit <- data.table::setDT(sfit)
  delta_present <- any(grepl("delta", sfit$variable))

  cases <- sfit[grepl("sim_", variable)]
  cases[, date := rep(seq(start_date, by = "week", length.out = t), .N / t)]
  cases[, Type := data.table::fcase(
                      grepl("_ndelta", variable), "non-DELTA",
                      grepl("_delta", variable), "DELTA",
                      rep(delta_present, .N), "Combined",
                      default = "Overall"
        )]

  delta <- sfit[grepl("frac_delta", variable)]
  delta[, date := seq(start_date, by = "week", length.out = .N)]

  rt <- sfit[grepl("r\\[", variable)]
  rt[, date := rep(seq(start_date, by = "week", length.out = t - 1),
                   .N / (t - 1))]
  rt[, Type := fcase(
    grepl("delta_r", variable), "DELTA",
    grepl("r\\[", variable) & delta_present, "non-DELTA",
    grepl("r\\[", variable), "Overall"
  )]
  cols <- c("mean", "median", "q5", "q95")
  rt[, (cols) := lapply(.SD, exp), .SDcols = cols, by = "Type"]

  out <- list(cases = cases, delta = delta, rt = rt)
  return(out)
}