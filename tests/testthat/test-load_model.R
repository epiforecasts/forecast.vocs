
test_that("Returns paths to models", {
  single <- load_model(strains = 1, compile = FALSE)
  expect_type(single, "character")
  expect_true(grepl("stan/bp.stan", single))
  two <- load_model(strains = 2, compile = FALSE)
  expect_type(two, "character")
  expect_true(grepl("stan/twostrainbp.stan", two))
})

test_that(
  "Compilation of the single strain models is successful and syntax is valid",
  { # nolint
    single <- suppressMessages(load_model(strains = 1, compile = TRUE))
    single <- suppressMessages(cmdstanr::cmdstan_model(single))
    expect_error(suppressMessages(single$check_syntax()), NA)
  }
)

test_that(
  "Compilation of two strain model is successful and syntax is valid",
  { # nolint
    two <- suppressMessages(load_model(strains = 2, compile = TRUE))
    two <- suppressMessages(cmdstanr::cmdstan_model(two))
    expect_error(suppressMessages(two$check_syntax()), NA)
  }
)

test_that("Cannot load a model that doesn't exist", {
  expect_error(load_model(strains = 3))
})
