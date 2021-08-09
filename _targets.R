library(targets)
library(tarchetypes)
library(stantargets)
library(future)
library(future.callr)
plan(callr)

tar_option_set(packages = c("bp.delta", "purrr", "data.table"))

# Input and control targets
meta_targets <- list(
  tar_target(
    obs,
    germany_obs
  ),
  tar_target(
    single_model,
    bp.delta::load_model(strains = 1),
    format = "file", deployment = "main"
  ),
  tar_target(
    two_model,
    bp.delta::load_model(strains = 2),
    format = "file", deployment = "main"
  ),
  tar_target(
    forecast_args,
    list(
      horizon = 4, adapt_delta = 0.99, max_treedepth = 15,
      parallel_chains = 1, plot = FALSE
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
    define_scenarios()
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
    deployment = "main"
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
    deployment = "main"
  ),
  tar_target(
    avail_scenario_obs,
    scenario_obs[, `:=`(
      forecast_date = forecast_dates,
      avail_obs = list(filter_by_availability(obs, forecast_dates))
    )],
    cross(forecast_dates, scenario_obs),
    deployment = "main"
  )
)

# Targets producing forecasts
forecast_dt <- function(obs, overdispersion, variant_relationship,
                        id, forecast_args, ...) {
  dt <- data.table(
    date = max(obs$date),
    overdispersion = overdispersion
  )
  if (!missing(variant_relationship)) {
    dt[, variant_relationship := variant_relationship]
  }
  if (!missing(id)) {
    dt[, id := id]
  }
  dt[
    ,
    forecast := list(
      do.call(
        forecast,
        c(
          forecast_args,
          list(...),
          list(
            obs = obs, overdispersion = overdispersion
          )
        )
      )
    )
  ]
  return(dt)
}

forecast_targets <- list(
  tar_target(
    single_retrospective_forecasts,
    forecast_dt(retro_obs, overdispersion,
      forecast_args = forecast_args,
      strains = 1, models = list(single_model)
    ),
    cross(retro_obs, overdispersion)
  ),
  tar_target(
    two_retrospective_forecasts,
    forecast_dt(retro_obs, overdispersion,
      variant_relationship = variant_relationship,
      forecast_args = forecast_args,
      strains = 2, models = list(two_model)
    ),
    cross(retro_obs, variant_relationship, overdispersion)
  )
)

scenario_forecast_targets <- list(
  tar_target(
    single_scenario_forecasts,
    forecast_dt(avail_scenario_obs$avail_obs[[1]],
      overdispersion = overdispersion,
      forecast_args = forecast_args,
      strains = 1, models = list(single_model),
      id = avail_scenario_obs$id[[1]],
      delta = avail_scenario_obs$delta[[1]]
    ),
    cross(avail_scenario_obs, overdispersion)
  ),
  tar_target(
    two_scenario_forecasts,
    forecast_dt(avail_scenario_obs$avail_obs[[1]],
      overdispersion = overdispersion,
      variant_relationship = variant_relationship,
      forecast_args = forecast_args,
      strains = 2, models = list(two_model),
      id = avail_scenario_obs$id[[1]],
      delta = avail_scenario_obs$delta[[1]]
    ),
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
