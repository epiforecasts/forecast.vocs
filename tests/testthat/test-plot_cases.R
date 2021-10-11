
test_that("Can plot using freshly generated forecasts", {
  expect_ggplot(plot_cases(posterior1))
  expect_ggplot(plot_cases(posterior1, log = TRUE))
  expect_ggplot(
    plot_cases(
      posterior1,
      forecast_dates = extract_forecast_dates(
        posterior1
      )[, date := as.Date("2021-05-01")]
    )
  )
  expect_ggplot(plot_cases(posterior2))
  expect_ggplot(plot_cases(posterior2, current_obs))
  expect_ggplot(plot_cases(posterior2, current_obs, all_obs = FALSE))
  expect_ggplot(plot_cases(forecast1, current_obs))
  expect_ggplot(plot_cases(forecast2, current_obs))
})
