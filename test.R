library(brms)
library(data.table)
library(targets)

tar_load("rwis")
rwis_retro <- rwis[id == 0]
rwis_scenario <- rwis[id != 0]

fit_data <- copy(rwis_retro)
fit_data <- fit_data[, log_baseline := log(baseline)][!is.na(share_delta)]
fit_data[, forecast_date := as.factor(forecast_date)]
fit_data[, date := as.factor(date)]
fit_data[, horizon_minus_one := horizon]

fit_data[,
  as.list(summary(rwis)),
  by = .(overdispersion, variant_relationship, horizon)
][order(Median)]

retro_fit <- brm(
  bf(
    interval_score ~ overdispersion + variant_relationship +
      s(horizon_minus_one, k = 4) + s(share_delta, k = 5) + (1 | date) +
      offset(log_baseline),
    sigma ~ 1 + offset(log_baseline)
  ),
  family = student("log", "log"),
  data = fit_data,
  backend = "cmdstanr",
  chains = 4,
  cores = 4
)
