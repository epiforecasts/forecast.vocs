
test_extract_forecast <- function(message, strains, posterior) {
  test_that(message, {
    forecasts <- extract_forecast(posterior)
    expect_type(forecasts, "list")
    expect_true(data.table::is.data.table(forecasts))
    expect_named(
      forecasts,
      c(
        "value_type", "type", "date", "horizon", "forecast_start", "mean",
        "median", "sd", "mad", "q5", "q20", "q80", "q95"
      )
    )
    if (strains == 1) {
      types <- "Overall"
      value_types <- c("cases", "growth", "rt")
    } else if (strains == 2) {
      types <- c("Combined", "VOC", "non-VOC")
      value_types <- c("cases", "voc", "growth", "rt")
    }
    expect_equal(unique(forecasts$type), types)
    expect_gt(min(forecasts$horizon), 0)
    expect_gte(max(forecasts$horizon), 4)
    expect_equal(unique(forecasts$value_type), value_types)
  })
}

test_extract_forecast(
  "Can extract forecasts from the posterior of the single strain model",
  strains = 1, posterior = posterior1
)

test_extract_forecast(
  "Can extract forecasts from the posterior of the two strain model",
  strains = 2, posterior = posterior2
)
