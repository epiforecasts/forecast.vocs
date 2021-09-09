library(data.table)
# load example data from eval-delta-forecasting
germany_covid19_delta_obs <- fread(
  "https://raw.githubusercontent.com/epiforecasts/evaluate-delta-for-forecasting/main/data/obs_germany.csv" # nolint
)

setnames(germany_covid19_delta_obs,
  old = c("seq_delta", "share_delta"),
  new = c("seq_voc", "share_voc")
)
# save all observations
usethis::use_data(germany_covid19_delta_obs, overwrite = TRUE)
