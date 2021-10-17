
test_that("Can unnest a posterior successfully", {
  pdt <- data.table::data.table(posterior = list(posterior1, posterior2))
  upost <- unnest_posterior(pdt)
  expect_equal(upost, rbind(posterior1, posterior2))
})
