library(bp.delta)
library(data.table)

# set number of cores
options(mc.cores = 4)

prior <- forecast(germany_obs,
  horizon = 4,
  save_path = "inst/output/prior-check",
  max_treedepth = 15,
  adapt_delta = 0.99,
  likelihood = FALSE
)
