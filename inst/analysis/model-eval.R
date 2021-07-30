library(bp.delta)

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

plot_pairs(fit)

bp_launch_shinystan(fit)
