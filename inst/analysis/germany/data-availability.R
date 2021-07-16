library(bp.delta)
library(data.table)
library(purrr)

# set number of cores
options(mc.cores = 4)

obs <- latest_obs(germany_obs)
dates <- obs$date[-c(1:3)]
scenarios <- define_scenarios()
scenarios_obs <- purrr::map2(
  scenarios$seq_lag, scenarios$seq_samples,
  ~ generate_obs_scenario(obs, seq_lag = .x, seq_samples = .y)
)
