score_forecast_targets <- list(
  # map forecasts to long format
  tar_target(
    forecast_cases_long,
    quantiles_to_long(forecast_cases)
  ),
  # score every available observation with all metrics
  tar_target(
    forecast_scores,
    eval_forecasts(forecast_cases_long[!is.na(true_value)])
  ),
  # calculate relative performance vs the single strain baseline
  tar_target(
    baseline_wis,
    eval_forecasts(
      forecast_cases_long[!is.na(true_value) & id == 0 & strains == 1],
      metrics = "interval_score"
    )[order(forecast_date, horizon)][
      ,
      .(forecast_date, date, horizon, overdispersion, location, interval_score)
    ]
  ),
  tar_target(
    rwis,
    merge(
      eval_forecasts(
        forecast_cases_long[!is.na(true_value) & strains == 2],
        metrics = "interval_score"
      )[order(id, forecast_date, date)],
      baseline_wis[, baseline := interval_score][, interval_score := NULL],
      by = c("forecast_date", "date", "horizon", "overdispersion", "location")
    )[, c("rwis") := interval_score / baseline]
  )
)
