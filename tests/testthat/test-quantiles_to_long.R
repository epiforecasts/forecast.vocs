
test_that("Can convert a posterior to long format successfully", {
  skip_on_cran()
  long <- quantiles_to_long(posterior1)
  expect_true(all(c("quantile", "prediction") %in% names(long)))
  expect_type(long$quantile, "double")
  expect_equal(unique(long$quantile), c(0.05, 0.20, 0.80, 0.95))
  expect_type(long$prediction, "double")
})
