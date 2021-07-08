library(bp.delta)

# set number of cores
options(mc.cores = 4)

results <- forecast(germany_cases,
                    horizon = 4,
                    save_path = "inst/output",
                    strains = c(2, 1),
                    max_treedepth = 15,
                    adapt_delta = 0.99,
                    output_loglik = TRUE)
# inspect object
names(results)

# look at plots
names(results$plots)
results$plots$cases
results$plots$log_cases
results$plots$delta
results$plots$rt