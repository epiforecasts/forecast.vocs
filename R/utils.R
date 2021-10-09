#' @rawNamespace import(data.table, except = transpose)
#' @import cmdstanr
#' @import ggplot2
#' @importFrom stats median rnorm
NULL

utils::globalVariables(
  c(
    ".", ".draw", "cases", "cases_available", "dates", "end", "exponentiated",
    "horizon", "id", "mad", "max_treedepth", "mean_share_voc",
    "no_at_max_treedepth", "observed", "patterns", "per_at_max_treedepth",
    "q20", "q5", "q80", "q95", "quantile", "results", "row_id", "sd",
    "seq_available", "seq_total", "seq_voc", "share_voc", "start", "type",
    "Type", "value", "value_type", "..non_list_cols", "forecast_start"
  )
)
