
# Make test data
frac_voc <- seq(0, 1, by = 0.1)
seq_total <- seq(10, length.out = length(frac_voc), by = 100)

test_that("Binomial observation model can be sampled from as expected", {
  seq <- sample_sequences(frac_voc, seq_total)
  expect_type(seq, "double")
  expect_equal(length(seq), 11)
  expect_true(!any(is.na(seq)))
})

test_that("Beta binomial observation model can be sampled from as expected", {
  seq <- sample_sequences(frac_voc, seq_total, 0.1)
  expect_type(seq, "double")
  expect_equal(length(seq), 11)
  expect_true(!any(is.na(seq)))
})
