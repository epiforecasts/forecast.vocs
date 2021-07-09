#' @rawNamespace import(data.table, except = transpose)
#' @import cmdstanr
#' @import ggplot2
#' @importFrom stats median rnorm
NULL

#' Check Quantiles Required are Present
#' @export
check_quantiles <- function(posterior, req_probs = c(0.5, 0.95, 0.2, 0.8)) {
  req_q <- paste0("q", 100 * req_probs)
  cols <- colnames(posterior)
  if (sum(cols %in% c("q5", "q95", "q20", "q80"))  != 4) {
    stop("Following quantiles must be present (set with probs): ",
         paste(req_probs, collapse = ", "))
  }
  return(invisible(NULL))
}