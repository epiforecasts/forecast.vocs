test_that("Posterior estimates can be returned as expected", {
  expect_data_table(fv_example(strains = 1, type = "p"))
  expect_data_table(fv_example(strains = 2, type = "p"))
})

test_that("Forecast estimates can be returned as expected", {
  expect_data_table(fv_example(strains = 1, type = "f"))
  expect_data_table(fv_example(strains = 2, type = "f"))
})

test_that("Observations can be returned as expected", {
  expect_error(check_observations(fv_example(type = "o")), NA)
})

test_that("Scripts can be returned as expected", {
  expect_type(fv_example(strains = 1, type = "s"), "character")
  expect_type(fv_example(strains = 2, type = "s"), "character")
  expect_error(readLines(fv_example(strains = 1, type = "s")), NA)
  expect_error(readLines(fv_example(strains = 2, type = "s")), NA)
})
