test_sample <- TRUE
if (!is.null(getOption("test_sample"))) {
  test_sample <- getOption("test_sample")
}
if (test_sample) {
  if (not_on_cran()) {
    options(mc.cores = 2)
    obs <- filter_by_availability(
      germany_covid19_delta_obs,
      date = "2021-06-26"
    )

    overdisp_dt <- fv_as_data_list(
      obs,
      overdispersion = TRUE, variant_relationship = "correlated",
      voc_scale = c(0.4, 0.2)
    )
    overdisp_dt_step <- fv_as_data_list(
      obs,
      overdispersion = TRUE, variant_relationship = "correlated",
      voc_scale = c(0.4, 0.2), r_step = 7
    )
    nooverdisp_dt <- fv_as_data_list(obs, overdispersion = FALSE)
    scaled_dt <- fv_as_data_list(
      obs,
      overdispersion = TRUE, variant_relationship = "scaled",
      voc_scale = c(0.4, 0.2)
    )
    independent_dt <- fv_as_data_list(
      obs,
      overdispersion = TRUE, variant_relationship = "independent",
      voc_scale = c(0.4, 0.2)
    )
    ndisp_scaled_dt <- fv_as_data_list(
      obs,
      overdispersion = FALSE, variant_relationship = "scaled",
      voc_scale = c(0.4, 0.2)
    )
    ndisp_independent_dt <- fv_as_data_list(
      obs,
      overdispersion = FALSE, variant_relationship = "independent",
      voc_scale = c(0.4, 0.2)
    )
    inits1 <- fv_inits(overdisp_dt, strains = 1)
    inits2 <- fv_inits(overdisp_dt, strains = 2)
    inits2_step <- fv_inits(overdisp_dt_step, strains = 2)
    one_model <- suppressMessages(fv_model(strains = 1))
    two_model <- suppressMessages(fv_model(strains = 2))
  }

  test_fv_sample(
    "The single strain model with overdispersion can be fit as expected",
    overdisp_dt, one_model, inits1
  )

  test_fv_sample(
    "The single strain model without overdispersion can be fit as expected",
    nooverdisp_dt, one_model, inits1,
    convergence = FALSE
  )

  test_fv_sample(
    "The two strain model with pooling and overdispersion can be fit as
    expected",
    overdisp_dt, two_model, inits2
  )

  test_fv_sample(
    "The two strain model with pooling, a weekly random walk and overdispersion
    can be fit as expected",
    overdisp_dt_step, two_model, inits2_step
  )

  test_fv_sample(
    "The two strain model with scaling and overdispersion can be fit as
    expected",
    scaled_dt, two_model, inits2
  )

  test_fv_sample(
    "The two strain model with independence and overdispersion can be fit as
    expected",
    independent_dt, two_model, inits2
  )
}
