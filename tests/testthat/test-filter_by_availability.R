suppressMessages(library(data.table, quietly = TRUE))

# construct test data
dt <- rbind(
  update_obs_availability(latest_obs(germany_covid19_delta_obs), seq_lag = 3),
  update_obs_availability(latest_obs(germany_covid19_delta_obs), seq_lag = 1)
)

test_that("Dates are unique", {
  expect_dates_unique(filter_by_availability(dt))
  expect_dates_unique(filter_by_availability(dt, seq_date = "2021-06-12"))
  expect_dates_unique(filter_by_availability(dt, case_date = "2021-07-01"))
})

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
