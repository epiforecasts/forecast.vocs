
if (requireNamespace("scoringutils")) {
  test_that("Can score forecasts", {
    skip_on_cran()
    pp_forecasts <- summary(
      forecast_wrapper,
      target = "forecast", type = "cases"
    )

    expect_data_table(
      fv_score_forecast(pp_forecasts, current_obs)
    )
    expect_data_table(
      fv_score_forecast(
        pp_forecasts, current_obs,
        log = TRUE
      )
    )
    expect_data_table(
      suppressMessages(fv_score_forecast(
        pp_forecasts, current_obs,
        check = TRUE
      ))
    )
  })
}
