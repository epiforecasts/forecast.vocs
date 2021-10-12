
test_that("Can plot using freshly generated forecasts", {
  skip_on_cran()
  expect_ggplot(
    plot_voc(
      posterior2,
      forecast_dates = extract_forecast_dates(
        posterior1
      )[, date := as.Date("2021-05-01")]
    )
  )
  expect_ggplot(plot_voc(posterior2))
  expect_ggplot(plot_voc(posterior2, current_obs))
  expect_ggplot(plot_voc(posterior2, current_obs, all_obs = FALSE))
})

test_that("Can plot using example data", {
  posterior2 <- fv_example(strains = 2, type = "posterior")
  current_obs <- fv_example(type = "obs")
  vdiffr::expect_doppelganger(
    "VoC plot with custom forecast date",
    plot_voc(
      posterior2,
      forecast_dates = extract_forecast_dates(
        posterior2
      )[, date := as.Date("2021-05-01")]
    )
  )
  vdiffr::expect_doppelganger(
    "Default VoC plot with two strain",
    plot_voc(posterior2)
  )
  vdiffr::expect_doppelganger(
    "Two strain VoC plot with current observations",
    plot_voc(posterior2, current_obs)
  )
  vdiffr::expect_doppelganger(
    "Two strain VoC plot with all current observations",
    plot_voc(posterior2, current_obs, all_obs = FALSE)
  )
})
