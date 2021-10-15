
test_that("Model sampling with default settings over two strains happens
           without error", {
  expect_true(all(purrr::map_lgl(forecast_wrapper$error, is.null)))
  expect_true(!is.null(forecast_wrapper$fit))
  expect_equal(forecast_wrapper$strains, c(1, 2))
})

test_forecast("Default settings work as expected",
  obs, forecast, fit2, posterior2, forecast2,
  equal = TRUE
)

test_forecast("Can alter quantiles in posterior from forecasts wrapper",
  obs, forecast, fit2, posterior2, forecast2,
  equal = FALSE,
  probs = c(0.1, 0.2)
)
