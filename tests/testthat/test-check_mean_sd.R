
test_that("Detects parameter types", {
  expect_error(forecast.vocs:::check_mean_sd(c(TRUE, TRUE), "test"))
  expect_error(forecast.vocs:::check_mean_sd(c("fi", "hi"), "test"))
  expect_error(forecast.vocs:::check_mean_sd(c(1, 2), "test"), NA)
})

test_that("Detects vectors that are not ofl length 2", {
  expect_error(forecast.vocs:::check_mean_sd(c(1), "test"))
  expect_error(forecast.vocs:::check_mean_sd(c(1, 2, 3), "test"))
  expect_error(forecast.vocs:::check_mean_sd(c(1, 2), "TRUE"), NA)
})
