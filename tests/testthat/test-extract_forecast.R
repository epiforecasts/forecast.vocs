
test_extract_forecast(
  "Can extract forecasts from the posterior of the single strain model",
  strains = 1, posterior = posterior1
)

test_extract_forecast(
  "Can extract forecasts from the posterior of the two strain model",
  strains = 2, posterior = posterior2
)
