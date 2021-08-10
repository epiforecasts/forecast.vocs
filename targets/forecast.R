# Targets producing forecasts
forecast_targets <- list(
  tar_target(
    single_retrospective_forecasts,
    do.call(
      forecast_dt,
      c(
        forecast_args,
        list(
          obs = retro_obs, strains = 1, overdispersion = overdispersion,
          model = single_model
        )
      )
    ),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
    cross(retro_obs, overdispersion)
  ),
  tar_target(
    two_retrospective_forecasts,
    do.call(
      forecast_dt,
      c(
        forecast_args,
        list(
          obs = retro_obs, strains = 2, overdispersion = overdispersion,
          variant_relationship = variant_relationship,
          model = two_model
        )
      )
    ),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
    cross(retro_obs, variant_relationship, overdispersion)
  )
)
