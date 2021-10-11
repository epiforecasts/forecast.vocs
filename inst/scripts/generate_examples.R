source("inst/scripts/1_strains_example.R")

data.table::fwrite(
  posterior, "inst/extdata/posterior_1_strains_example.csv"
)

data.table::fwrite(
  forecast, "inst/extdata/forecast_1_strains_example.csv"
)

data.table::fwrite(
  current_obs, "inst/extdata/observations_example.csv"
)

source("inst/scripts/2_strains_example.R")

data.table::fwrite(
  posterior, "inst/extdata/posterior_2_strains_example.csv"
)

data.table::fwrite(
  forecast, "inst/extdata/forecast_2_strains_example.csv"
)
