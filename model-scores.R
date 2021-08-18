library(brms)
library(data.table)
library(targets)

tar_load("rwis")
rwis_retro <- rwis[id == 0]
rwis_scenario <- rwis[id != 0]

process_rwis <- function(rwis) {
  fit_data <- copy(rwis)
  fit_data <- fit_data[, log_baseline := log(baseline)][!is.na(share_delta)]
  fit_data[, forecast_date := as.factor(forecast_date)]
  fit_data[, date := as.factor(date)]
  fit_data[, horizon_minus_one := horizon]
  fit_data[
    ,
    variant_relationship := factor(
      variant_relationship,
      levels = c("pooled", "scaled", "independent")
    )
  ]
  fit_data[
    ,
    overdispersion := factor(
      fcase(
        overdispersion == TRUE, "yes",
        overdispersion == FALSE, "no"
      ),
      levels = c("yes", "no")
    )
  ]
  return(fit_data)
}
summarise_rwis <- function(rwis, by = c(
                             "overdispersion",
                             "variant_relationship",
                             "horizon"
                           )) {
  rwis[,
    as.list(summary(rwis)),
    by = by
  ][order(Median)]
}

rwis_retro <- process_rwis(rwis_retro)
rwis_scenario <- process_rwis(rwis_scenario)

summarise_rwis(rwis_retro)
summarise_rwis(rwis_scenario)

retro_fit <- brm(
  bf(
    rwis ~ overdispersion + variant_relationship +
      s(horizon_minus_one, k = 4) + s(share_delta, k = 5) + (1 | date)
  ),
  family = lognormal(),
  data = rwis_retro,
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  adapt_delta = 0.99,
  max_treedepth = 15
)

scenario_fit <- brm(
  bf(
    rwis ~ overdispersion + variant_relationship +
      s(horizon_minus_one, k = 4) + s(share_delta, k = 5) + (1 | date) +
      seq_samples + delta
  ),
  family = lognormal(),
  data = rwis_scenario,
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  adapt_delta = 0.95,
  max_treedepth = 15
)
