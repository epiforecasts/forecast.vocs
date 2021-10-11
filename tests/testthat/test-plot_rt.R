
test_that("Can plot using freshly generated forecasts", {
  skip_on_cran()
  expect_ggplot(plot_rt(posterior1))
  expect_ggplot(
    plot_rt(
      posterior1,
      forecast_dates = extract_forecast_dates(
        posterior1
      )[, date := as.Date("2021-05-01")]
    )
  )
  expect_ggplot(plot_rt(posterior2))
})

test_that("Can plot using example data", {
  posterior1 <- load_example(strains = 1, type = "posterior")
  posterior2 <- load_example(strains = 2, type = "posterior")
  vdiffr::expect_doppelganger(
    "Default rt with single strain", plot_rt(posterior1)
  )
  vdiffr::expect_doppelganger(
    "rt plot with custom forecast date",
    plot_rt(
      posterior1,
      forecast_dates = extract_forecast_dates(
        posterior1
      )[, date := as.Date("2021-05-01")]
    )
  )
  vdiffr::expect_doppelganger(
    "Default rt plot with two strain",
    plot_rt(posterior2)
  )
})
