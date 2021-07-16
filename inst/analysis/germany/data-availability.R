library(bp.delta)
library(purrr)
library(future)

# set number of cores
plan("multicore")

# Fit scenarios
fits <- forecast_across_scenarios(
  latest_obs(germany_obs),
  horizon = 4,
  models = c(
    load_model(strains = 2),
    load_model(strains = 1)
  ),
  save_path = "inst/output/germany/data-availability",
  strains = c(2, 1),
  max_treedepth = 15, adapt_delta = 0.99,
  refresh = 0, show_messages = FALSE,
  parallel_chains = 1
)
