
test_that("Two strain model can be used to generate observations", {
  skip_on_cran()
  sim_obs <- suppressMessages(
    generate_obs(obs, strains = 2, fit = silent_fv_sample)
  )
  expect_named(
    sim_obs,
    c("dataset", "parameters", "obs", "data")
  )
  expect_error(check_observations(sim_obs$obs[[1]]), NA)
  expect_error(fv_inits(sim_obs$data[[1]], strains = 2), NA)
  expect_named(
    sim_obs$parameters[[1]],
    c(".draw", ".iteration", ".chain", "parameter", "sample")
  )
})

test_that("Single strain model can be used to generate observations", {
  skip_on_cran()
  sim_obs <- suppressMessages(
    generate_obs(obs, strains = 1, fit = silent_fv_sample)
  )
  expect_named(
    sim_obs,
    c("dataset", "parameters", "obs", "data")
  )
  expect_error(check_observations(sim_obs$obs[[1]]), NA)
  expect_error(fv_inits(sim_obs$data[[1]], strains = 1), NA)
  expect_named(
    sim_obs$parameters[[1]],
    c(".draw", ".iteration", ".chain", "parameter", "sample")
  )
})
