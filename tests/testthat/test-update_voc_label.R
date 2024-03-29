
test_that("Can replace VoC placeholders with a named label", {
  skip_on_cran()
  up_posterior <- update_voc_label(
    posterior2,
    label = "Delta", target_label = "VOC"
  )
  expect_equal(unique(up_posterior$variable), unique(posterior2$variable))
  expect_true(any(unique(up_posterior$clean_name) %in% "Average Delta effect"))
  expect_equal(
    unique(up_posterior$type), c(NA, "Combined", "Delta", "non-Delta")
  )
})

test_that("No changes are made when there is no label supplied", {
  skip_on_cran()
  expect_equal(update_voc_label(posterior2), posterior2)
})
