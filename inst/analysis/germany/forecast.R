library(bp.delta)

# set number of cores
options(mc.cores = 4)

results <- forecast(tmp,
  horizon = 4,
  save_path = tempdir(),
  strains = c(1, 2),
  adapt_delta = 0.99,
  max_treedepth = 15,
  output_loglik = TRUE
)
# inspect object
names(results)

# look at plots
names(results$plots)
results$plots$cases
results$plots$log_cases
results$plots$delta
results$plots$rt
