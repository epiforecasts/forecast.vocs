
test_scenarios <- define_scenarios()[1:2]

test_forecast("Default settings work as expected for a single date",
  obs, forecast_across_scenarios, fit2, posterior2, forecast2,
  equal = TRUE,
  forecast_dates = c(as.Date("2021-06-26")), scenarios = test_scenarios,
)

test_forecast("Can alter quantiles in posterior from forecasts wrapper",
  obs, forecast_across_dates, fit2, posterior2, forecast2,
  equal = FALSE,
  probs = c(0.1, 0.2), forecast_dates = c(as.Date("2021-06-26")),
  scenarios = test_scenarios,
)

test_forecast("Can forecast across multiple dates",
  obs, forecast_across_dates, fit2, posterior2, forecast2,
  equal = FALSE,
  probs = c(0.1, 0.2),
  forecast_dates = c(as.Date("2021-06-26"), as.Date("2021-07-24")),
  scenarios = test_scenarios
)
