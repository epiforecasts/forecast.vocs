test_that("Detects numericv parameter types", {
  expect_error(check_param(c(TRUE, TRUE), "test", type = "numeric", length = 2))
  expect_error(check_param(c("fi", "hi", type = "numeric", length = 2), "test"))
  expect_error(check_param(c(1, 2), "test", type = "numeric", length = 2), NA)
})

test_that("Detects numeric vectors that are not of length 2", {
  expect_error(check_param(c(1), "test", type = "numeric", length = 2))
  expect_error(check_param(c(1, 2, 3), "test", type = "numeric", length = 2))
  expect_error(check_param(c(1, 2), "TRUE", type = "numeric", length = 2), NA)
})

test_that("Detects logical parameters", {
  expect_error(check_param(1, "test", type = "logical"))
  expect_error(check_param("fi", "test", type = "logical"))
  expect_error(check_param(TRUE, "test", type = "logical"), NA)
})

test_that("Detects vectors longer than 2 that are logical", {
  expect_error(check_param(c(TRUE, FALSE), "test", type = "logical"))
  expect_error(check_param(TRUE, "TRUE", type = "logical"), NA)
})
