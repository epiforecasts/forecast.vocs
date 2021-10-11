
test_summarise_posterior(
  "The single strain model can have its posterior summarised as expected",
  fit1, posterior1, 1, equal = TRUE
)

test_summarise_posterior(
  "The two strain model can have its posterior summarised as expected",
  fit2, posterior2, 2, equal = TRUE
)

test_summarise_posterior(
  "The two strain model can have its posterior summarised as expected with
   a custom quantile list",
  fit2, posterior2, 2, equal = FALSE, probs = c(0.2, 0.3, 0.6, 0.8)
)
