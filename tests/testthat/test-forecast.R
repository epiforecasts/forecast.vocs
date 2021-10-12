
test_forecast("Default settings work as expected",
  obs, forecast, fit2, posterior2, forecast2,
  equal = TRUE
)

test_forecast("Can alter quantiles in posterior from forecasts wrapper",
  obs, forecast, fit2, posterior2, forecast2,
  equal = FALSE,
  probs = c(0.1, 0.2)
)
