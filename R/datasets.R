
#' Test positive COVID-19 cases and sequences in Germany
#'
#' @description Test positive COVID-19 cases and sequences stratified by
#' voc variant status summarised by week for Germany. Data is sourced from
#' the RKI via the Germany/Poland forecasting hub.
#'
#' @return A `data.table` with the following variables: date, location_name,
#' location, cases, seq_total, seq_voc, share_voc, cases_available,
#' and seq_available.
#'
#' @family data
"germany_covid19_delta_obs"
