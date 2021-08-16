summarise_source_targets <- list(
  # gather summaries and forecasts
  tar_target(
    fit_summary,
    fit_summary_germany
  ),
  tar_target(
    forecast_cases,
    forecast_cases_germany
  ),
  tar_target(
    forecast_scores,
    forecast_scores_germany
  ),
  tar_target(
    rwis,
    rwis_germany
  )
)
