library(bp.delta)
library(purrr)

# set number of cores
options(mc.cores = 4)

dates <- germany_cases$date[-c(1:3)]
fits <- map(
  dates,
  ~ forecast(germany_cases,
    forecast_date = .,
    horizon = 4,
    models = c(
      load_model(strains = 2),
      load_model(strains = 1)
    ),
    save_path = "inst/output/retrospective",
    strains = c(2, 1),
    max_treedepth = 15, adapt_delta = 0.99
  )
)
names(fits) <- dates
