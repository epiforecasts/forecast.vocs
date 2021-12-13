
test_that("Can plot using freshly generated forecasts", {
  skip_on_cran()
  expect_ggplot(plot(forecast_wrapper))
  expect_ggplot(plot(forecast_wrapper, type = "voc_advantage"))
  expect_ggplot(plot(forecast_wrapper, type = "voc_frac"))
  expect_ggplot(plot(forecast_wrapper, type = "growth"))
  expect_ggplot(plot(forecast_wrapper, type = "rt"))
  expect_ggplot(plot(forecast_wrapper, type = "all")[[1]])
  expect_ggplot(plot(forecast_wrapper, type = "fit"))
})
