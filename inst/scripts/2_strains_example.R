options(mc.cores = 2)
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = "2021-06-26"
)

current_obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = "2021-08-26"
)

dt <- stan_data(
  obs,
  overdispersion = TRUE,
  variant_relationship = "scaled",
  voc_scale = c(0.4, 0.2)
)

inits <- stan_inits(dt, strains = 2)

model <- suppressMessages(load_model(strains = 2))

fit <- stan_fit(
  data = dt, model = model, init = inits,
  adapt_delta = 0.99, max_treedepth = 15, chains = 2
)

# summarise posterior assuming a mean generation time of  5.5 days.
posterior <- summarise_posterior(fit, scale_r = 5.5 / 7)
forecast <- extract_forecast(posterior)