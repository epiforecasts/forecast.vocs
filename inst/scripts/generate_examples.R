source("inst/scripts/1_strains_example.R")

saveRDS(
  posterior, "inst/extdata/posterior_1_strain_example.rds"
)

saveRDS(
  forecast, "inst/extdata/forecast_1_strain_example.rds"
)

saveRDS(
  current_obs, "inst/extdata/observations_example.rds"
)

source("inst/scripts/2_strains_example.R")

saveRDS(
  posterior, "inst/extdata/posterior_2_strain_example.rds"
)

saveRDS(
  forecast, "inst/extdata/forecast_2_strain_example.rds"
)
