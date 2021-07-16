library(bp.delta)
library(purrr)
library(future)

# set number of cores
options(mc.cores = 1)
plan("multicore")

# Fit scenarios
fits <- forecast_accross_scenarios(latest_obs(germany_obs))
forecast_accross_scenarios <- function(obs, scenarios, save_path = tempdir(),
                                       ...) {
  if (missing(scenarios)) {
    scenarios <- bp.delta::define_scenarios()
  }
  scenarios$obs <- purrr::map2(
    scenarios$seq_lag, scenarios$seq_samples,
    ~ generate_obs_scenario(obs, seq_lag = .x, seq_samples = .y)
  )
  scenarios <- split(scenarios, by = "id")

  fits <- future.apply::future_lapply(
    scenarios,
    function(scenario, ...) {
      forecast_accross_dates(
        scenario$obs,
        delta = scenario$delta,
        save_path = file.path(save_path, scenario$id),
        ...
      )
    }
  )
  scenarios$fits <- fits
  return(scenarios)
}
