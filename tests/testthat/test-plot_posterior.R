
test_that("Can plot results from the single strain model", {
  p <- plot_posterior(posterior1)
  expect_ggplot(p[[1]])
  expect_named(p, c("cases", "log_cases", "growth", "rt"))
})


test_that("Can plot results from the two strain model", {
  p <- plot_posterior(posterior2)
  expect_ggplot(p[[1]])
  expect_named(p, c("cases", "log_cases", "voc", "growth", "rt"))
})
