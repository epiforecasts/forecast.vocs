
test_that("Can unnest a posterior successfully", {
  p1 <- fv_example(type = "posterior")
  p2 <- fv_example(type = "posterior", strains = 1)
  pdt <- data.table::data.table(posterior = list(p1, p2))
  upost <- unnest_posterior(pdt)
  expect_equal(upost, rbind(p1, p2))
})
