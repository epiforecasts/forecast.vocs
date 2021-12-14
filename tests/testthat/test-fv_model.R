
test_that("Returns paths to models", {
  single <- fv_model(strains = 1, compile = FALSE)
  expect_type(single, "character")
  expect_true(grepl("stan/bp.stan", single))
  two <- fv_model(strains = 2, compile = FALSE)
  expect_type(two, "character")
  expect_true(grepl("stan/twostrainbp.stan", two))
})

test_that(
  "Compilation of the single strain models is successful and syntax is valid",
  { # nolint
    skip_on_cran()
    single <- suppressMessages(fv_model(strains = 1, compile = TRUE))
    expect_error(
      suppressMessages(single$check_syntax(
        include_path = system.file("stan", package = "forecast.vocs")
        )
      ), NA)
  }
)

test_that(
  "Compilation of two strain model is successful and syntax is valid",
  { # nolint
    skip_on_cran()
    two <- suppressMessages(fv_model(strains = 2, compile = TRUE))
    expect_error(suppressMessages(two$check_syntax(
      include_path = system.file("stan", package = "forecast.vocs")
    )), NA)
  }
)

test_that("Cannot load a model that doesn't exist", {
  expect_error(fv_model(strains = 3))
})
