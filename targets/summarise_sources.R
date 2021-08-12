summarise_source_targets <- list(
  tar_target(
    fit_summary,
    fit_summary_germany
  ),
  tar_target(
    forecast_cases,
    forecast_cases_germany
  ),
  tar_target(
    forecast_cases_long,
    quantiles_to_long(forecast_cases)
  )
)
