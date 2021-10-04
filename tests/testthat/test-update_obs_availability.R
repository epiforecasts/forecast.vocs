library(data.table, quietly = TRUE)

# construct test data
dt <- rbind(
  update_obs_availability(latest_obs(germany_covid19_delta_obs), seq_lag = 3),
  update_obs_availability(latest_obs(germany_covid19_delta_obs), seq_lag = 1)
)

test_that("Dates are unique", {
  expect_dates_unique <- function(dt) {
    expect_equal(nrow(dt[, .(n = .N), by = c("date")][n > 1]), 0)
  }

  expect_dates_unique(filter_by_availability(dt))
  expect_dates_unique(filter_by_availability(dt, seq_date = "2021-06-12"))
  expect_dates_unique(filter_by_availability(dt, case_date = "2021-07-01"))
})

test_filter_by_availability <- function(dt, message, tar_date = max(dt$date),
                                        case_date = tar_date,
                                        seq_date = tar_date) {
  test_that(message, {
    fdt <- filter_by_availability(dt,
      date = tar_date, seq_date = seq_date,
      case_date = case_date
    )
    # Dates are correctly ordered to avoid downstream issues
    expect_true(
      all(fdt[, ordered := date > shift(date)][!is.na(ordered)]$ordered)
    )
    # No data beyond sequence date is present
    expect_equal(nrow(fdt[seq_available > seq_date & is.na(seq_available)]), 0)
    # No data beyond case date is present
    expect_equal(
      nrow(fdt[cases_available > case_date & is.na(cases_available)]), 0
    )
    if (case_date > seq_date) {
      # If cases are available after sequences they are present
      expect_true(nrow(fdt[cases_available > seq_date]) > 0)
    }
    # If cases were available before sequences they are still present
    if (nrow(dt[date < tar_date & is.na(seq_available)]) > 0) {
      expect_true(nrow(fdt[date < tar_date & is.na(seq_available)]) > 0)
    }
    # Processed data passes observations checks
    expect_error(check_observations(fdt), NA)
  })
}

test_filter_by_availability(
  dt,
  message = "Default settings work as expected"
)

test_filter_by_availability(
  dt,
  message = "Default settings work when setting a forecast date",
  tar_date = as.Date("2021-07-24")
)

test_filter_by_availability(
  dt,
  message = "Default settings work when setting a case available date",
  case_date = as.Date("2021-07-24")
)

test_filter_by_availability(
  dt,
  message = "Default settings work when setting a sequence available date",
  seq_date = as.Date("2021-07-24")
)

test_filter_by_availability(
  dt,
  message = "Default settings work when setting all dates",
  case_date = as.Date("2021-07-24"),
  seq_date = as.Date("2021-08-07"),
  tar_date = as.Date("2021-07-31")
)
