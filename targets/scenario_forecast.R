scenario_forecast_targets <- list(
  tar_target(
    single_scenario_forecasts,
    do.call(
      forecast_dt,
      c(
        forecast_args,
        list(
          obs = avail_scenario_obs$avail_obs[[1]],
          strains = 1, overdispersion = overdispersion,
          model = single_model,
          id = avail_scenario_obs$id[[1]],
          delta = avail_scenario_obs$delta[[1]]
        )
      )
    ),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
    cross(avail_scenario_obs, overdispersion)
  ),
  tar_target(
    two_scenario_forecasts,
    do.call(
      forecast_dt,
      c(
        forecast_args,
        list(
          obs = avail_scenario_obs$avail_obs[[1]],
          strains = 2, overdispersion = overdispersion,
          variant_relationship = variant_relationship,
          model = two_model,
          id = avail_scenario_obs$id[[1]],
          delta = avail_scenario_obs$delta[[1]]
        )
      )
    ),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
    cross(avail_scenario_obs, variant_relationship, overdispersion)
  )
)
