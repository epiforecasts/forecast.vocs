expect_data_table <- function(dt) {
  expect_true(data.table::is.data.table(dt))
}
