library(targets)
library(tarchetypes)
library(stantargets)
library(future)
library(future.callr)
plan(callr)

tar_option_set(
  packages = c("bp.delta", "purrr", "data.table"),
  imports = "bp.delta", deployment = "main",
  debug = "two_retrospective_forecasts"
)

# Input and control targets
meta_targets <- list(
  tar_target(
    obs,
    germany_obs
  ),
  tar_target(
    single_model,
    bp.delta::load_model(strains = 1),
    format = "file"
  ),
  tar_target(
    two_model,
    bp.delta::load_model(strains = 2),
    format = "file"
  ),
  tar_target(
    forecast_args,
    list(
      horizon = 4, adapt_delta = 0.8, max_treedepth = 12,
      parallel_chains = 1, plot = FALSE,
    )
  )
)

# Targets defining the scenarios to evaluate
scenario_targets <- list(
  tar_target(
    variant_relationship,
    c("scaled", "pooled", "independent")
  ),
  tar_target(
    overdispersion,
    c(TRUE, FALSE)
  ),
  tar_target(
    scenarios,
    head(define_scenarios(), n = 5)
  )
)

# Targets converting observed data
obs_targets <- list(
  tar_target(
    latest_obs,
    latest_obs(obs)
  ),
  tar_target(
    forecast_dates,
    latest_obs[!is.na(seq_available), ]$date[-c(1:3)]
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
        latest_obs,
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

c(
  meta_targets,
  obs_targets,
  scenario_targets,
  forecast_targets,
  scenario_forecast_targets
)
