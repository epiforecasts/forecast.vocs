# define test data
obs <- latest_obs(germany_covid19_delta_obs)
t <- nrow(obs)
cases_na <- nrow(obs[is.na(cases)])
seq_na <- nrow(obs[is.na(seq_voc)])
burn_in <- 4
h <- 4

test_that("Passes internal data checks with example data", {
  expect_error(fv_as_data_list(obs), NA)
})

# make default data
nlist <- fv_as_data_list(obs, horizon = h)

# test data properties
test_that("Output has the correct list properties", {
  expect_true(is.list(nlist))
  expect_equal(length(names(nlist)), length(nlist))
})

test_that("Check for NA or NULL entries in the list output", {
  expect_true(any(purrr::map_lgl(nlist, ~ all(!is.na(.)))))
  expect_true(any(purrr::map_lgl(nlist, ~ all(!is.null(.)))))
})

test_that("Index variables are correctly defined", {
  expect_equal(nlist$t, t + h)
  expect_equal(nlist$t_nots, t - burn_in + h)
  expect_equal(nlist$t_nseq, 4)
  expect_equal(nlist$t_seqf, t - 4 + h)
  expect_equal(nlist$t_seq, t - seq_na)
})

test_that("Count variables are correctly defined", {
  expect_equal(length(nlist$X), t)
  expect_true(is.integer(nlist$X))
  expect_equal(length(nlist$N), t - seq_na)
  expect_true(is.integer(nlist$N))
  expect_equal(length(nlist$Y), t - seq_na)
  expect_true(is.integer(nlist$Y))
})

test_that("Switch variables behave as expected", {
  switches <- c("likelihood", "output_loglik", "overdisp", "debug")
  purrr::walk(switches, ~ expect_true(nlist[[.]] == 1 | nlist[[.]] == 0))
})

test_that("Start date is present and a date", {
  expect_true(inherits(nlist$start_date, "Date"))
})

test_that("Variant strain relationship can be set as expected", {
  expect_equal(fv_as_data_list(obs, variant_relationship = "scaled")$relat, 0)
  expect_equal(
    fv_as_data_list(obs, variant_relationship = "correlated"
  )$relat, 2)
  expect_equal(
    fv_as_data_list(obs, variant_relationship = "independent")$relat, 1
  )
})
