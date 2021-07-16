library(bp.delta)


# set number of cores
options(mc.cores = 4)

fits <- forecast_accross_dates(
  germany_obs,
  horizon = 4,
  models = c(
    load_model(strains = 2),
    load_model(strains = 1)
  ),
  save_path = "inst/output/germany/retrospective",
  strains = c(2, 1),
  max_treedepth = 15, adapt_delta = 0.99
)
