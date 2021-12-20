#' Calculate the day of the week periodicity
#'
#' This helper function allows the user to generate a vector of day of the
#' the week periods.
#'
#' @param t An integer indicating the number of dates
#'
#' @param start_date A date indicating the start date
#'
#' @param specials A vector of special dates to modify the day of the week for.
#'
#' @param special_to A character string indicating which day of the week or
#' other label to assign holidays. By default this is set to "Sunday"
#'
#' @return A vector indicating the period of the dates.
#'
#' @export
#' @importFrom data.table data.table
#' @family preprocess
#' @examples
#' fv_dow_period(t = 10, start_date = as.Date("2021-12-01"))
fv_dow_period <- function(t, start_date, specials = c(),
                          special_to = "Sunday") {
  obs <- data.table::data.table(
    date = seq(start_date, length.out = t, by = "days")
  )
  obs[, day_of_week := weekdays(date)]
  obs[, special := FALSE]

  # make holidays be sundays
  if (length(specials) != 0) {
    obs <- obs[date %in% specials, special := TRUE]
    obs <- obs[special == TRUE, day_of_week := specials_to]
  }

  # make day of week a factor
  obs[, day_of_week := factor(day_of_week)]
  obs[, period := as.numeric(day_of_week)]
  return(obs$period)
}

#' Filter data based on availability and forecast date
#'
#' @param date Date at which to filter. Defaults to the
#' maximum date in `obs`.
#'
#' @param seq_date Date from which to use available sequence data. Defaults to
#' the `date`.
#'
#' @param case_date Date from which to use available case data. Defaults to
#' the `date`.
#'
#' @return A `data.frame` of observations filter for the latest available
#' data for the specified dates of interest.
#'
#' @family preprocess
#' @export
#' @inheritParams update_obs_availability
#' @importFrom purrr map
#' @examples
#' options(mc.cores = 4)
#' obs <- filter_by_availability(
#'   germany_covid19_delta_obs,
#'   date = as.Date("2021-06-12"),
#' )
#' dt <- rbind(
#'   update_obs_availability(obs, seq_lag = 3),
#'   update_obs_availability(obs, seq_lag = 1)
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
    obs_max_cases[!(date %in% obs_max_pres_cases$date)][,
      .SD[1, ],
      by = "date"
    ]
  )
  # make sure the data is in the correct order
  setorderv(obs, cols = c("date"))
  return(obs[])
}

#' Filter for latest observations of all types
#'
#' @return A `data.frame` of observations filtered for the
#' latest available data.
#'
#' @family preprocess
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
  return(obs[])
}
