library(data.table)
# load example data from eval-delta-forecasting
germany_covid19_delta_obs <- fread(
  "https://raw.githubusercontent.com/epiforecasts/evaluate-delta-for-forecasting/main/data/obs_germany.csv" # nolint
)
# save all observations
usethis::use_data(germany_covid19_delta_obs, overwrite = TRUE)
