library(bp.delta)
library(shinystan)
library(rstan)
library(bayesplot)

dt <- stan_data(latest_obs(germany_obs), horizon = 4)
mod <- load_model(strains = 2)
inits <- stan_inits(dt, strains = 2)
fit <- stan_fit(dt,
  model = mod, init = inits, adapt_delta = 0.8,
  max_treedepth = 15, save_warmup = TRUE
)

p <- summarise_posterior(fit)
plot_rt(p)
plot_cases(p, latest_obs(germany_obs), log = TRUE)

draws <- fit$fit$draws()
stanfit <- read_stan_csv(fit$fit$output_files())

np <- nuts_params(stanfit)

dts <- mcmc_parcoord(draws,
  np = np,
  pars = c("r_init", "r_noise", "init_cases[1]", "eta[1]", "phi[1]")
)
dts

pairs <- mcmc_pairs(draws,
  np = np,
  pars = c("r_init", "r_noise", "init_cases[1]", "eta[1]", "phi[1]")
)
pairs

launch_shinystan(stanfit)
