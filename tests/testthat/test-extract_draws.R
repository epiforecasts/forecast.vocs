
test_that("Can extract posterior samples from a stan_fit", {
  skip_on_cran()
  draws <- extract_draws(fit1)
  expect_true(any(class(draws) %in% "draws_array"))
})
