library(bp.delta)
options(mc.cores = 4)

strains <- 2
dt <- stan_data(latest_obs(germany_obs), horizon = 4, overdispersion = TRUE,
                variant_relationship = "pooled")
mod <- load_model(strains = strains)
inits <- stan_inits(dt, strains = strains)
fit <- stan_fit(dt,
  model = mod, init = inits, adapt_delta = 0.99,
  max_treedepth = 15, save_warmup = TRUE
)

p <- summarise_posterior(fit)
plot_rt(p)
plot_cases(p, latest_obs(germany_obs), log = TRUE)

plot_pairs(fit)

bp_launch_shinystan(fit)
