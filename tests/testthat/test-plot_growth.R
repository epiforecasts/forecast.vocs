
test_that("Can plot using freshly generated forecasts", {
  skip_on_cran()
  expect_ggplot(plot_growth(posterior1))
  expect_ggplot(
    plot_growth(
      posterior1,
      forecast_dates = extract_forecast_dates(
        posterior1
      )[, date := as.Date("2021-05-01")]
    )
  )
  expect_ggplot(plot_growth(posterior2))
  expect_ggplot(plot_growth(posterior2, central = TRUE))
})

test_that("Can plot using example data", {
  posterior1 <- fv_example(strains = 1, type = "posterior")
  posterior2 <- fv_example(strains = 2, type = "posterior")
  vdiffr::expect_doppelganger(
    "Default growth with single strain", plot_growth(posterior1)
  )
  vdiffr::expect_doppelganger(
    "Growth plot with custom forecast date",
    plot_growth(
      posterior1,
      forecast_dates = extract_forecast_dates(
        posterior1
      )[, date := as.Date("2021-05-01")]
    )
  )
  vdiffr::expect_doppelganger(
    "Default growth plot with two strain",
    plot_growth(posterior2)
  )
})
