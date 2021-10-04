
test_that("Packaged data passes checks", {
  obs <- latest_obs(germany_covid19_delta_obs)
  expect_error(check_observations(obs), NA)
})

test_that("Packaged data fails when required variables are missing", {
  obs <- latest_obs(germany_covid19_delta_obs)
  expect_error(check_observations(data.table::copy(obs)[, cases := NULL]))
  expect_error(check_observations(data.table::copy(obs)[, seq_total := NULL]))
  expect_error(check_observations(data.table::copy(obs)[, seq_voc := NULL]))
})

test_that("Packaged data fails when required variables have the wrong type", {
  obs <- latest_obs(germany_covid19_delta_obs)
  expect_error(
    check_observations(data.table::copy(obs)[, cases := rep("fewfwe", .N)])
  )
  expect_error(
    check_observations(data.table::copy(obs)[, seq_total := rep("fewfwe", .N)])
  )
  expect_error(
    check_observations(data.table::copy(obs)[, seq_voc := rep("fewfwe", .N)])
  )
})
