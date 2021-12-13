
test_that("Can summarise forecasts", {
  skip_on_cran()
  expect_error(
    p <- capture.output(print(posterior2)), NA
  )
})
