test_that("Can successfully covert the output of fv_sample to rstan format", {
  rstanformat <- convert_to_stanfit(fit1)
  expect_type(rstanformat, "S4")
  expect_equal(class(rstanformat)[1], "stanfit")
})
