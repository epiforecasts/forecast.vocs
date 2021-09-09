sample_sequences <- function(frac_voc, seq_total, phi) {
  if (missing(phi)) {
    phi <- NULL
  }
  if (!is.null(phi)) {
    shape1 <- frac_voc * phi
    shape2 <- (1 - frac_voc) * phi
    frac_voc <- suppressWarnings(
      purrr::map2_dbl(shape1, shape2, ~ rbeta(1, .x, .y))
    )
  }

  seq_voc <- suppressWarnings(
    purrr::map2_dbl(seq_total, frac_voc, ~ rbinom(1, .x, .y))
  )
  return(seq_voc)
}

generate_data <- function(obs, strains = 2,
                          model = forecast.vocs::load_model(strains = strains),
                          type = "prior", datasets = 10, ...) {
  type <- match.arg(type, choices = c("prior", "posterior"))
  dt <- stan_data(obs,
    likelihood = type %in% "posterior",
    output_loglik = FALSE, horizon = 0, ...
  )

  inits <- stan_inits(dt, strains = strains)

  fit <- stan_fit(
    data = dt, model = model, init = inits,
    adapt_delta = 0.99, max_treedepth = 15,
    refresh = 0, show_messages = FALSE,
    chains = 2, iter_sampling = ceiling(datasets * 5 / 2),
    thin = 5
  )

  draws <- extract_draws(fit)
  draws <- setDT(posterior::as_draws_df(draws))
  draws <- draws[.draw <= datasets]
  vars <- colnames(draws)

  gen_cases <- suppressWarnings(
    data.table::copy(draws)[, grepl("sim_cases", vars), with = FALSE]
  )

  gen_frac_voc <- suppressWarnings(
    data.table::copy(draws)[, grepl("frac_voc", vars), with = FALSE]
  )

  seq_phi <- draws[["phi[2]"]]

  gen_data <- data.table(
    obs = purrr::map(seq_len(datasets), function(i) {
      copy(obs)[
        ,
        `:=`(
          cases = as.vector(t(gen_cases[i, ])),
          mean_share_voc = c(
            rep(NA, dt$t_nseq),
            as.vector(t(gen_frac_voc[i, ]))
          )
        )
      ][
        ,
        seq_voc := sample_sequences(mean_share_voc, seq_total, seq_phi[i])
      ][
        ,
        share_voc := seq_voc / seq_total
      ]
    })
  )

  gen_data[, stan_data := purrr::map(obs, stan_data, horizon = 0, ...)]
  return(gen_data)
}
