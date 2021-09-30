#' Update observations based on availability
#' @param obs A data frame with the following variables:
#'  date, cases, seq_voc, and seq_total, cases_available,
#' and seq_available. seq_available and case_available must be
#' uniquely define data rows but other rows can be duplicated based
#' on data availability.
#' @param cases_lag Number of weeks that case data takes to be reported.
#' Defaults to not alter the input data.
#' @param seq_lag Number of weeks that sequence data takes to be reported.
#' Defaults to not alter the input data.
#' @export
#' @examples
#' update_obs_availability(
#'   germany_covid19_delta_obs,
#'   cases_lag = 2, seq_lag = 3
#' )
update_obs_availability <- function(obs, cases_lag, seq_lag) {
  obs <- as.data.table(obs)
  obs <- copy(obs)
  if (!missing(cases_lag)) {
    if (!is.null(cases_lag)) {
      obs[!is.na(cases_available), cases_available := date + cases_lag * 7]
    }
  }
  if (!missing(seq_lag)) {
    if (!is.null(seq_lag)) {
      obs[!is.na(seq_available), seq_available := date + seq_lag * 7]
    }
  }
  return(obs)
}

#' Filter data based on availability and forecast date
#' @param date Date at which to filter. Defaults to the
#' maximum date in `obs`.
#' @param seq_date Date from which to use available sequence data. Defaults to
#' the forecast date.
#' @param case_date Date from which to use available case data. Defaults to
#' the forecast date.
#' @export
#' @inheritParams update_obs_availability
#' @importFrom purrr map
#' @examples
#' latest_dt <- latest_obs(germany_covid19_delta_obs)
#' dt <- rbind(
#'   update_obs_availability(latest_dt, seq_lag = 3),
#'   update_obs_availability(latest_dt, seq_lag = 1)
#' )
#' # filter out duplicates and up to the present date
#' filter_by_availability(dt)
#'
#' # filter to only use sequence data up the the 12th of June
#' filter_by_availability(dt, seq_date = "2021-06-12")
#'
#' # as above but only use
#' filter_by_availability(dt,
#'   seq_date = "2021-06-12",
#'   case_date = "2021-07-01"
#' )
filter_by_availability <- function(obs, date = max(obs$date),
                                   seq_date = date,
                                   case_date = date) {
  obs <- as.data.table(obs)
  obs <- copy(obs)
  target_date <- date
  # filter by forecast date and update data based on availability
  obs <- obs[date <= target_date]
  obs[cases_available > case_date, cases := NA]
  cols <- c("seq_total", "seq_voc", "share_voc")
  obs[seq_available > seq_date, (cols) := purrr::map(.SD, ~NA),
    .SDcols = cols
  ]
  # filter to get latest non NA data for sequences
  obs_na_seq <- obs[is.na(seq_available)]
  obs <- obs[!is.na(seq_available)]
  obs_max_seq <- obs[, .SD[seq_available == max(seq_available, na.rm = TRUE)],
    by = "date"
  ]
  obs_max_pres_seq <- obs[!is.na(seq_total),
    .SD[seq_available == max(seq_available, na.rm = TRUE)],
    by = "date"
  ]
  obs <- rbind(
    obs_na_seq, obs_max_pres_seq,
    obs_max_seq[!(date %in% obs_max_pres_seq$date)]
  )
  # repeat for cases
  obs_max_cases <- obs[,
    .SD[cases_available == max(cases_available, na.rm = TRUE)],
    by = "date"
  ]
  obs_max_pres_cases <- obs[!is.na(cases),
    .SD[cases_available == max(cases_available, na.rm = TRUE)][1, ],
    by = "date"
  ]
  obs <- rbind(
    obs_max_pres_cases,
    obs_max_cases[!(date %in% obs_max_pres_cases$date)]
  )
  # make sure the data is in the correct order
  setorderv(obs, cols = c("date"))
  return(obs)
}

#' Filter for latest observations of all types
#' @export
#' @inheritParams filter_by_availability
#' @examples
#' dt <- rbind(
#'   update_obs_availability(germany_covid19_delta_obs, seq_lag = 3),
#'   update_obs_availability(germany_covid19_delta_obs, seq_lag = 1)
#' )
#' latest_obs(dt)
latest_obs <- function(obs) {
  date <- max(
    max(obs$date), max(obs$cases_available, na.rm = TRUE),
    max(obs$seq_available, na.rm = TRUE)
  )
  obs <- filter_by_availability(obs, date = date)
  return(obs)
}
