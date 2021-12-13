
if (requireNamespace("scoringutils")) {
  test_that("Can score forecasts", {
    skip_on_cran()
    pp_forecasts <- summary(
      forecast_wrapper, target = "forecast", type = "cases"
    )

    expect_data_table(
      fv_score_forecast(pp_forecasts, current_obs, summarise_by = "strains")
    )
    expect_data_table(
      fv_score_forecast(
        pp_forecasts, current_obs, summarise_by = "strains", log = TRUE
      )
    )
    expect_data_table(
      fv_score_forecast(
        pp_forecasts, current_obs, summarise_by = c("strains", "horizon")
      )
    )
    expect_data_table(
      fv_score_forecast(
        pp_forecasts, current_obs, summarise_by = c("strains", "horizon"),
        log = TRUE
      )
    )
  })
}
