summarise_forecast_targets <- list(
  tar_target(
    fit_summary,
    rbindlist(
      map(list(
        single_retrospective_forecasts,
        two_retrospective_forecasts,
        single_scenario_forecasts,
        two_scenario_forecasts
      ), ~ .[, .(
        id, forecast_date, strains, overdispersion, variant_relationship,
        samples, max_rhat, divergent_transitions,
        per_divergent_transitons, max_treedepth
      )])
    ),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
  ),
  tar_target(
    forecast_single_retro,
    combine_posteriors_dt(single_retrospective_forecasts, target = "forecast"),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
  ),
  tar_target(
    forecast_two_retro,
    combine_posteriors_dt(two_retrospective_forecasts, target = "forecast"),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
  ),
  tar_target(
    forecast_single_scenario,
    combine_posteriors_dt(single_scenario_forecasts, target = "forecast"),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
  ),
  tar_target(
    forecast_two_scenario,
    combine_posteriors_dt(two_scenario_forecasts, target = "forecast"),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
  ),
  tar_target(
    forecast,
    rbindlist(list(
      forecast_single_retro,
      forecast_two_retro,
      forecast_single_scenario,
      forecast_two_scenario
    ))[, location := source],
    deployment = "worker",
    memory = "transient"
  ),
  tar_target(
    forecast_cases,
    merge(
      forecast[value_type == "cases"][type %in% c("Overall", "Combined")],
      current_obs[, .(date, true_value = cases)],
      all.x = TRUE, by = "date"
    ),
    deployment = "worker"
  )
)
