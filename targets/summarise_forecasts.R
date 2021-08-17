# Targets summarising forecasts
summarise_forecast_targets <- list(
  # Summarise the forecast models fit
  tar_target(
    fit_summary,
    rbindlist(
      map(list(
        single_retrospective_forecasts,
        two_retrospective_forecasts,
        two_scenario_forecasts
      ), ~ .[, .(
        id, forecast_date, strains, overdispersion, variant_relationship,
        samples, max_rhat, divergent_transitions,
        per_divergent_transitons, max_treedepth
      )])
    ),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
  ),
  # Combine forecasts into a single data frame
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
    forecast_two_scenario,
    combine_posteriors_dt(two_scenario_forecasts, target = "forecast"),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
  ),
  # Combine all separate forecasts into a single data frame
  tar_target(
    forecast,
    merge(
      rbindlist(
        list(
          forecast_single_retro,
          forecast_two_retro,
          forecast_two_scenario
        )
      )[, location := source],
      data_availability_scenarios[
        ,
        delta := map_chr(delta, paste, collapse = ", ")
      ],
      by = "id", all.x = TRUE
    ),
    deployment = "worker",
    memory = "transient"
  ),
  # Extract forecasts for cases only and link to current observations
  tar_target(
    forecast_cases,
    merge(
      forecast[value_type == "cases"][type %in% c("Overall", "Combined")][
        ,
        type := NULL
      ],
      current_obs[, .(date,
        true_value = cases,
        share_delta, seq_delta, seq_total
      )],
      all.x = TRUE, by = "date"
    ),
    deployment = "worker"
  )
)
