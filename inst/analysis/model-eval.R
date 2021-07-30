library(bp.delta)
library(shinystan)
library(rstan)

dt <- stan_data(latest_obs(germany_obs), horizon = 0)
mod <- load_model(strains = 1)
inits <- stan_inits(dt, strains = 1)
fit <- stan_fit(dt,
  model = mod, init = inits, adapt_delta = 0.9,
  max_treedepth = 15, save_warmup = TRUE
)

p <- summarise_posterior(fit)
plot_rt(p)
plot_cases(p, latest_obs(germany_obs), log = TRUE)

stanfit <- read_stan_csv(fit$fit$output_files())
launch_shinystan(stanfit)
