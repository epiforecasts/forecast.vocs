test_that("Packaged data passes checks", {
  expect_error(check_dataframe(germany_covid19_delta_obs), NA)
})

test_that("Packaged data fails when required variables are missing", {
  obs <- germany_covid19_delta_obs
  expect_error(
    check_dataframe(data.table::copy(obs)[, cases := NULL],
      req_types = c("numeric"), req_vars = c("cases")
    )
  )
  expect_error(
    check_dataframe(data.table::copy(obs)[, date := NULL],
      req_types = c("Date"), req_vars = c("date")
    )
  )
})

test_that("Packaged data fails when required variables have the wrong type", {
  obs <- germany_covid19_delta_obs
  expect_error(
    check_dataframe(data.table::copy(obs)[, cases := NULL],
      req_types = c("character"), req_vars = c("cases")
    )
  )
  expect_error(
    check_dataframe(data.table::copy(obs)[, date := NULL],
      req_types = c("character"), req_vars = c("date")
    )
  )
})
