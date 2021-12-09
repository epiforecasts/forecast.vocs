
test_that("Can plot using freshly generated forecasts", {
  skip_on_cran()
  expect_ggplot(
    plot_voc_advantage(
      posterior2,
      forecast_dates = extract_forecast_dates(
        posterior1
      )[, date := as.Date("2021-05-01")]
    )
  )
  expect_ggplot(plot_voc_advantage(posterior2))
})

test_that("Can plot using example data", {
  posterior2 <- fv_example(strains = 2, type = "posterior")
  vdiffr::expect_doppelganger(
    "Advantage plot with custom forecast date",
    plot_voc_advantage(
      posterior2,
      forecast_dates = extract_forecast_dates(
        posterior2
      )[, date := as.Date("2021-05-01")]
    )
  )
  vdiffr::expect_doppelganger(
    "Default Advantage plot with two strain",
    plot_voc_advantage(posterior2)
  )
})
