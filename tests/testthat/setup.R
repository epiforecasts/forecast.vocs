if (not_on_cran()) {
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

  inits1 <- stan_inits(dt, strains = 1)
  inits2 <- stan_inits(dt, strains = 2)

  model1 <- suppressMessages(load_model(strains = 1))
  model2 <- suppressMessages(load_model(strains = 2))

  fit1 <- silent_stan_fit(
    data = dt, model = model1, init = inits1,
    adapt_delta = 0.9, max_treedepth = 15, chains = 2
  )
  fit2 <- silent_stan_fit(
    data = dt, model = model2, init = inits2,
    adapt_delta = 0.9, max_treedepth = 15, chains = 2
  )

  posterior1 <- summarise_posterior(fit1, scale_r = 5.5 / 7)
  posterior2 <- summarise_posterior(fit2, scale_r = 5.5 / 7)
  forecast1 <- extract_forecast(posterior1)
  forecast2 <- extract_forecast(posterior2)
}
