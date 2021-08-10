# Targets converting observed data
obs_targets <- list(
  tar_target(
    obs,
    load_obs(source)
  ),
  tar_target(
    current_obs,
    latest_obs(obs)
  ),
  tar_target(
    forecast_dates,
    current_obs[!is.na(seq_available), ]$date[-c(1:3)]
  ),
  tar_target(
    retro_obs,
    filter_by_availability(obs, date = forecast_dates),
    map(forecast_dates),
    deployment = "worker"
  ),
  tar_target(
    scenario_obs,
    scenarios[
      ,
      obs := list(generate_obs_scenario(
        current_obs,
        seq_lag = seq_lag, seq_samples = seq_samples
      ))
    ],
    map(scenarios),
    deployment = "worker"
  ),
  tar_target(
    avail_scenario_obs,
    scenario_obs[, `:=`(
      forecast_date = forecast_dates,
      avail_obs = list(filter_by_availability(obs, forecast_dates))
    )],
    cross(forecast_dates, scenario_obs),
    deployment = "worker"
  )
)
