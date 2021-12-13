
test_that("Can plot using freshly generated forecasts", {
  skip_on_cran()
  expect_ggplot(plot(posterior1))
  expect_ggplot(plot(posterior2, type = "voc_advantage"))
  expect_ggplot(plot(posterior2, type = "voc_frac"))
  expect_ggplot(plot(posterior2, type = "growth"))
  expect_ggplot(plot(posterior2, type = "rt"))
  expect_ggplot(plot(posterior2, type = "all")[[1]])
})

test_that("Can plot using example data", {
  posterior1 <- fv_example(strains = 1, type = "posterior")
  posterior2 <- fv_example(strains = 2, type = "posterior")
  expect_ggplot(plot(posterior1))
  expect_ggplot(plot(posterior2, type = "voc_advantage"))
  expect_ggplot(plot(posterior2, type = "voc_frac"))
  expect_ggplot(plot(posterior2, type = "growth"))
  expect_ggplot(plot(posterior2, type = "rt"))
  expect_ggplot(plot(posterior2, type = "all")[[1]])
})
