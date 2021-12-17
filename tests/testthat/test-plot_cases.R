
test_that("Can plot using freshly generated forecasts", {
  skip_on_cran()
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
  expect_ggplot(plot_cases(posterior2, current_obs, central = TRUE))
  expect_ggplot(plot_cases(posterior2, current_obs, all_obs = FALSE))
  expect_ggplot(plot_cases(forecast1, current_obs))
  expect_ggplot(plot_cases(forecast2, current_obs))
})

test_that("Can plot using example data", {
  posterior1 <- fv_example(strains = 1, type = "posterior")
  posterior2 <- fv_example(strains = 2, type = "posterior")
  forecast1 <- fv_example(strains = 1, type = "forecast")
  forecast2 <- fv_example(strains = 2, type = "forecast")
  current_obs <- fv_example(type = "obs")
  vdiffr::expect_doppelganger(
    "Default case with single strain", plot_cases(posterior1)
  )
  vdiffr::expect_doppelganger(
    "Logged case plot with single strain",
    plot_cases(posterior1, log = TRUE)
  )
  vdiffr::expect_doppelganger(
    "Logged case plot with ss and central",
    plot_cases(posterior1, log = TRUE, central = TRUE)
  )
  vdiffr::expect_doppelganger(
    "Case plot with custom forecast date",
    plot_cases(
      posterior1,
      forecast_dates = extract_forecast_dates(
        posterior1
      )[, date := as.Date("2021-05-01")]
    )
  )
  vdiffr::expect_doppelganger(
    "Default case plot with two strain",
    plot_cases(posterior2)
  )
  vdiffr::expect_doppelganger(
    "Two strain case plot with current observations",
    plot_cases(posterior2, current_obs)
  )
  vdiffr::expect_doppelganger(
    "Two strain case plot with all observations",
    plot_cases(posterior2, current_obs, all_obs = FALSE)
  )
  vdiffr::expect_doppelganger(
    "Single strain forecast plot", plot_cases(forecast1, current_obs)
  )
  vdiffr::expect_doppelganger(
    "Two strain forecast plot", plot_cases(forecast2, current_obs)
  )
})
