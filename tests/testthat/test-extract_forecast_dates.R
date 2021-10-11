
test_that("Can extract forecast dates from the one strain model posterior", {
  skip_on_cran()
  dates <- extract_forecast_dates(posterior1)
  expect_data_table(dates)
  expect_named(dates, c("Data unavailable", "date"))
  expect_equal(dates$`Data unavailable`, "Cases")
  expect_equal(as.character(dates$date), "2021-06-26")
  expect_true(any(class(dates$date) %in% "Date"))
})

test_that("Can extract forecast dates from the two strain model posterior", {
  skip_on_cran()
  dates <- extract_forecast_dates(posterior2)
  expect_data_table(dates)
  expect_named(dates, c("Data unavailable", "date"))
  expect_equal(dates$`Data unavailable`, c("Cases", "Sequences"))
  expect_equal(as.character(dates$date), c("2021-06-26", "2021-06-12"))
  expect_true(any(class(dates$date) %in% "Date"))
})
