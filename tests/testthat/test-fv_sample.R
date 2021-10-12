if (not_on_cran()) {
  options(mc.cores = 2)
  obs <- filter_by_availability(
    germany_covid19_delta_obs,
    date = "2021-06-26"
  )

  overdisp_dt <- fv_data(
    obs,
    overdispersion = TRUE, variant_relationship = "pooled",
    voc_scale = c(0.4, 0.2)
  )
  nooverdisp_dt <- fv_data(obs, overdispersion = FALSE)
  scaled_dt <- fv_data(
    obs,
    overdispersion = TRUE, variant_relationship = "scaled",
    voc_scale = c(0.4, 0.2)
  )
  independent_dt <- fv_data(
    obs,
    overdispersion = TRUE, variant_relationship = "independent",
    voc_scale = c(0.4, 0.2)
  )
  ndisp_scaled_dt <- fv_data(
    obs,
    overdispersion = FALSE, variant_relationship = "scaled",
    voc_scale = c(0.4, 0.2)
  )
  ndisp_independent_dt <- fv_data(
    obs,
    overdispersion = FALSE, variant_relationship = "independent",
    voc_scale = c(0.4, 0.2)
  )
  inits1 <- fv_inits(overdisp_dt, strains = 1)
  inits2 <- fv_inits(overdisp_dt, strains = 2)
  one_model <- suppressMessages(fv_model(strains = 1))
  two_model <- suppressMessages(fv_model(strains = 2))
}

test_fv_sample(
  "The single strain model with overdispersion can be fit as expected",
  overdisp_dt, one_model, inits1
)

test_fv_sample(
  "The single strain model without overdispersion can be fit as expected",
  nooverdisp_dt, one_model, inits1
)

test_fv_sample(
  "The two strain model with pooling and overdispersion can be fit as
   expected",
  overdisp_dt, two_model, inits2
)

test_fv_sample(
  "The two strain model with pooling and without overdispersion can be fit as
   expected",
  nooverdisp_dt, two_model, inits2
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