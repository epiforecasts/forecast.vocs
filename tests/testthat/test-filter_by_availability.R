library(data.table)

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
  expect_dates_unique(filter_by_availability(dt, seq_date = "2021-07-01"))
})

test_that("Default works as expected", {

})

test_that("Able to set a maximum sequence date", {

})

test_that("Able to set a maximum case date", {

})

test_that("Able to set both a maximum case and sequence date", {

})
