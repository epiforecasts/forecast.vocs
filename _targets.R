library(targets)
library(tarchetypes)
library(stantargets)
library(future)
library(future.callr)
plan(callr)

tar_option_set(packages = c("bp.delta", "purrr", "data.table"))
list(
  tar_target(
    obs,
    germany_obs
  ),
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
    single_model,
    bp.delta::load_model(strains = 1)
  ),
  tar_target(
    two_model,
    bp.delta::load_model(strains = 2)
  ),
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
  ),
  tar_target(
    scenario_obs,
    purrr::map2(
      scenarios$seq_lag, scenarios$seq_samples,
      ~ generate_obs_scenario(latest_obs, seq_lag = .x, seq_samples = .y)
    )
  ),
  tar_target(
    forecast_args,
    list(
      horizon = 4, adapt_delta = 0.99, max_treedepth = 15,
      parallel_chains = 1
    )
  ),
  tar_target(
    single_retro,
    data.table(
      date = max(retro_obs$date),
      overdispersion = overdispersion,
      forecast = list(do.call(
        forecast,
        c(
          forecast_args,
          list(
            obs = retro_obs, strains = 1, models = list(single_model),
            overdispersion = overdispersion
          )
        )
      ))
    ),
    cross(retro_obs, overdispersion)
  ),
  tar_target(
    two_retro,
    data.table(
      date = max(retro_obs$date),
      variant_relationship = variant_relationship,
      overdispersion = overdispersion,
      forecast = list(do.call(
        forecast,
        c(
          forecast_args,
          list(
            obs = retro_obs, strains = 2, models = list(two_model),
            variant_relationship = variant_relationship,
            overdispersion = overdispersion
          )
        )
      ))
    ),
    cross(retro_obs, variant_relationship, overdispersion)
  )
)
