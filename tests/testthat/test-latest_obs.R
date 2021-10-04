
# construct test data
dt <- rbind(
  update_obs_availability(latest_obs(germany_covid19_delta_obs), seq_lag = 3),
  update_obs_availability(latest_obs(germany_covid19_delta_obs), seq_lag = 1)
)

test_that("Returns latest data in correct format", {
  fdt <- latest_obs(dt)
  max_date <- max(dt$date, na.rm = TRUE)
  expect_equal(max_date, max(fdt$date, na.rm = TRUE))
  expect_error(check_observations(fdt), NA)
})
