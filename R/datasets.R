
#' Test positive COVID-19 cases and sequences in Germany
#'
#' @description Test positive COVID-19 cases and sequences stratified by
#' DELTA variant summarised by week for Germany. Data is sourced from the RKI
#' via the Germany/Poland forecasting hub.
#' @return A `data.table` with the following variables: date, location,
#' location_name, cases, seq_total, seq_delta, and share_delta.
"germany_cases"
