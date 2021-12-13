
test_that("Can summarise forecasts", {
  skip_on_cran()
  expect_true(
    any(
      class(
        summary(
          posterior2, type = "all"
        )
      ) %in% "fv_posterior"
    )
  )
  expect_true(
    any(
      class(
        summary(
          posterior2, type = "all"
        )
      ) %in% "fv_posterior"
    )
  )
  expect_true(
    all(
        grepl("r",
          summary(
            posterior2, target = "posterior", type = "rt"
          )$variable
        )
      )
    )
  expect_true(
    all(
      summary(posterior2, target = "forecast", type = "rt")$date
        >= as.Date("2021-03-20")
      )
  )
})
