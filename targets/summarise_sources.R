summarise_source_targets <- list(
  tar_target(
    fit_summary,
    fit_summary_germany,
    deployment = "worker"
  ),
  tar_target(
    forecast_cases,
    forecast_cases_germany,
    deployment = "worker"
  ),
  tar_target(
    forecast_cases_long,
    quantiles_to_long(forecast_cases),
    deployment = "worker"
  ),
  tar_target(
    forecast_scores,
    eval_forecasts(forecast_cases_long[!is.na(true_value) & id == 0]),
    deployment = "worker"
  ),
  tar_target(
    scenario_forecast_scores,
    eval_forecasts(forecast_cases_long[!is.na(true_value) & id != 0]),
    deployment = "worker"
  )
)
