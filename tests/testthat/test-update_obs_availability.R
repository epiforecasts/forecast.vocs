
# construct test data
dt <- latest_obs(germany_covid19_delta_obs)
dt[!is.na(seq_available), seq_available := date]
dt[!is.na(cases_available), cases_available := date]

expect_duplicates <- function(dt) {
  expect_true(nrow(dt[, .(n = .N), by = c("date")][n > 1]) > 0)
}

test_that("Can correctly add lags for sequence data", {
  seq_lag <- update_obs_availability(dt, seq_lag = 3)
  expect_duplicates(
    rbind(
      update_obs_availability(dt, seq_lag = 3),
      update_obs_availability(dt, seq_lag = 1)
    )
  )
  expect_equal(
    max(dt[!is.na(seq_available)]$seq_available),
    max(seq_lag[!is.na(seq_available)]$seq_available) - 7 * 3
  )
  expect_equal(
    min(dt[!is.na(seq_available)]$seq_available),
    min(seq_lag[!is.na(seq_available)]$seq_available) - 7 * 3
  )
})

test_that("Can correctly add lags for case data", {
  cases_lag <- update_obs_availability(dt, cases_lag = 3)
  expect_duplicates(
    rbind(
      update_obs_availability(dt, cases_lag = 3),
      update_obs_availability(dt, cases_lag = 1)
    )
  )
  expect_equal(
    max(dt[!is.na(cases_available)]$cases_available),
    max(cases_lag[!is.na(cases_available)]$cases_available) - 7 * 3
  )
  expect_equal(
    min(dt[!is.na(cases_available)]$cases_available),
    min(cases_lag[!is.na(cases_available)]$cases_available) - 7 * 3
  )
})
