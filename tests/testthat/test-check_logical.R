
test_that("Detects parameter types", {
  expect_error(forecast.vocs:::check_logical(1, "test"))
  expect_error(forecast.vocs:::check_logical("fi", "test"))
  expect_error(forecast.vocs:::check_logical(TRUE, "test"), NA)
})

test_that("Detects vectors longer than 2", {
  expect_error(forecast.vocs:::check_logical(c(TRUE, FALSE), "test"))
  expect_error(forecast.vocs:::check_logical(TRUE, "TRUE"), NA)
})
