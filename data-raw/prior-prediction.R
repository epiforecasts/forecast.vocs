library(bp.delta)

# set number of cores
options(mc.cores = 4)

results <- forecast(germany_cases,
                    horizon = 4,
                    save_path = "inst/output",
                    max_treedepth = 15,
                    adapt_delta = 0.99,
                    output_loglik = TRUE)
