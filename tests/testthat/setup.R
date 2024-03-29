if (not_on_cran()) {
  message("Running tests setup")
  options(mc.cores = 2)
  obs <- filter_by_availability(
    germany_covid19_delta_obs,
    date = "2021-06-26"
  )
  current_obs <- filter_by_availability(
    germany_covid19_delta_obs,
    date = "2021-08-26"
  )
  dt <- fv_as_data_list(
    obs,
    overdispersion = TRUE,
    variant_relationship = "scaled",
    voc_scale = c(0.4, 0.2)
  )

  inits1 <- fv_inits(dt, strains = 1)
  inits2 <- fv_inits(dt, strains = 2)

  model1 <- suppressMessages(fv_model(strains = 1))
  model2 <- suppressMessages(fv_model(strains = 2))

  fit1 <- silent_fv_sample(
    data = dt, model = model1, init = inits1,
    adapt_delta = 0.98, max_treedepth = 15, chains = 2
  )
  fit2 <- silent_fv_sample(
    data = dt, model = model2, init = inits2,
    adapt_delta = 0.98, max_treedepth = 15, chains = 2
  )

  posterior1 <- fv_tidy_posterior(fit1)
  posterior2 <- fv_tidy_posterior(fit2)
  forecast1 <- fv_extract_forecast(posterior1)
  forecast2 <- fv_extract_forecast(posterior2)

  forecast_wrapper <- forecast(
    obs,
    fit = silent_fv_sample,
    strains = c(1, 2), likelihood = FALSE,
    adapt_delta = 0.99, max_treedepth = 15, chains = 2,
    refresh = 0, show_messages = FALSE
  )
}
