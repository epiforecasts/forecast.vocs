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
    map(forecast_dates)
  ),
  tar_target(
    scenario_obs,
    purrr::map2(
      scenarios$seq_lag, scenarios$seq_samples,
      ~ generate_obs_scenario(latest_obs, seq_lag = .x, seq_samples = .y)
    )
  )
)

# Targets producing forecasts

forecast_dt <- function(obs, overdispersion, id, forecast_args, ...) {
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
          ...,
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
    single_retrospective_forecast,
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

c(
  meta_targets,
  obs_targets,
  scenario_targets,
  forecast_targets
)
