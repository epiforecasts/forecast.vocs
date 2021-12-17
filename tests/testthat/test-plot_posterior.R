
test_that("Can plot results from the single strain model", {
  skip_on_cran()
  p <- plot_posterior(posterior1)
  expect_ggplot(p[[1]])
  expect_named(p, c("cases", "log_cases", "growth", "rt"))
})


test_that("Can plot results from the two strain model", {
  skip_on_cran()
  p <- plot_posterior(posterior2)
  expect_ggplot(p[[1]])
  expect_named(
    p, c("cases", "log_cases", "voc_frac", "voc_advantage", "growth", "rt")
  )
})

test_that("Can plot results from the two strain model with central estimates", {
  skip_on_cran()
  p <- plot_posterior(posterior2, central = TRUE)
  expect_ggplot(p[[1]])
  expect_named(
    p, c("cases", "log_cases", "voc_frac", "voc_advantage", "growth", "rt")
  )
})
