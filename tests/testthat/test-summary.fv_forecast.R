
test_that("Can summarise forecasts", {
  skip_on_cran()
  expect_true(
    any(
      class(summary(forecast_wrapper, target = "fit")[[1]]) %in% "CmdStanMCMC"
    )
  )
  expect_true(
    any(
      class(
        summary(
          forecast_wrapper, target = "posterior", type = "all"
        )
      ) %in% "fv_posterior"
    )
  )
  expect_true(
    any(
      class(
        summary(
          forecast_wrapper, target = "forecast", type = "all"
        )
      ) %in% "fv_posterior"
    )
  )
  expect_true(
    !any(
      class(
        summary(
          forecast_wrapper, target = "forecast", type = "rt"
        )
      ) %in% "fv_posterior"
    )
  )
})
