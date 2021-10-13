library(forecast.vocs)
options(mc.cores = 4)

strains <- 2
dt <- fv_data(latest_obs(germany_covid19_delta_obs),
  horizon = 4, overdispersion = TRUE,
  variant_relationship = "independent"
)
mod <- fv_model(strains = strains)
inits <- fv_inits(dt, strains = strains)
fit <- fv_sample(dt,
  model = mod, init = inits, adapt_delta = 0.99,
  max_treedepth = 15, save_warmup = TRUE
)

p <- fv_tidy_posterior(fit)

plot_rt(p)

plot_cases(p, latest_obs(germany_covid19_delta_obs), log = TRUE)

plot_pairs(fit)

bp_launch_shinystan(fit)
