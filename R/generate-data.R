#' Sample Sequence Observation Model
#'
#' @param frac_voc A numeric vector of expected proportions positive for the
#' variant of concern.
#'
#' @param seq_total An integer vector of total sequences available.
#'
#' @param phi The overdispersion of the sampling process. If not supplied
#' then no overdispersion is used (i.e a binomial observation model vs a
#' beta binomial observation model).
#'
#' @return A vector of observed sequences positive for the variant of
#' concern.
#'
#' @family generatedata
#' @importFrom purrr map2_dbl
#' @export
#' @examples
#' # dummy sequence data
#' frac_voc <- seq(0, 1, by = 0.1)
#' seq_total <- seq(10, length.out = length(frac_voc), by = 100)
#'
#' # binomial observation model
#' sample_sequences(frac_voc, seq_total)
#'
#' # beta binomial observation model
#' sample_sequences(frac_voc, seq_total, 0.5)
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


#' Generate Simulated Observations
#'
#' Generate simulated observations from the prior or posterior
#' distributions of a `forecast.vocs` model.
#'
#' @param obs Observed data to use to parameterise the model and used for
#' fitting when the posterior is required.
#'
#'
#' @param type A character string indicating the type of data to generate.
#' Supported options are data based on the "prior" or data based on  the
#' "posterior" with the default being the prior.
#'
#' @param datasets Numeric, defaults to 10. Number of datasets to generate.
#'
#' @param ... Additional arguments to pass `fv_data()`.
#'
#' @return A dataframe with a sampled dataset on each row with the following
#' variables: parameters (prior/posterior parameters used to generate the data),
#' obs (simulated observed data), fv_data, (the simulated data formatted
#' using `fv_data()` using the same arguments as specified  for simulation.)
#'
#' @family generatedata
#' @inheritParams forecast
#' @inheritParams fv_sample
#' @export
#' @importFrom posterior as_draws_df
#' @importFrom purrr map
#' @examplesIf interactive()
#' options(mc.cores = 4)
#' obs <- latest_obs(germany_covid19_delta_obs)
#'
#' sim_obs <- generate_obs(obs, voc_scale = c(0.8, 0.1), r_init = c(-0.1, 0.05))
#'
#' # fit a simulated dataset
#' sim_dt <- sim_obs$fv_data[[1]]
#' inits <- fv_inits(sim_dt)
#' fit <- fv_sample(
#'   sim_dt,
#'   init = inits, adapt_delta = 0.95, max_treedepth = 15
#' )
#'
#' # summarise and plot simualated fit
#' posterior <- fv_tidy_posterior(fit)
#'
#' plot_cases(posterior, log = TRUE)
#'
#' plot_voc(posterior)
#'
#' plot_rt(posterior)
generate_obs <- function(obs, strains = 2,
                         model = forecast.vocs::fv_model(strains = strains),
                         type = "prior", datasets = 10, ...) {
  type <- match.arg(type, choices = c("prior", "posterior"))
  dt <- fv_data(obs,
    likelihood = type %in% "posterior",
    output_loglik = FALSE, horizon = 0, ...
  )

  inits <- fv_inits(dt, strains = strains)

  fit <- fv_sample(
    data = dt, model = model, init = inits,
    adapt_delta = 0.99, max_treedepth = 15,
    refresh = 0, show_messages = FALSE,
    chains = 2, iter_sampling = ceiling(datasets / 2)
  )

  draws <- extract_draws(fit)
  draws <- setDT(posterior::as_draws_df(draws))
  draws <- draws[.draw <= datasets]
  vars <- colnames(draws)
  melt_draws <- melt(draws,
    id.vars = c(".draw", ".iteration", ".chain"),
    variable.name = "parameter", value.name = "sample"
  )

  gen_cases <- suppressWarnings(
    data.table::copy(draws)[, grepl("sim_cases", vars), with = FALSE]
  )

  gen_frac_voc <- suppressWarnings(
    data.table::copy(draws)[, grepl("frac_voc", vars), with = FALSE]
  )

  seq_phi <- draws[["phi[2]"]]

  gen_data <- data.table(
    parameters = purrr::map(seq_len(datasets), ~ melt_draws[.draw == .]),
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

  gen_data[, fv_data := purrr::map(obs, fv_data, horizon = 0, ...)]
  return(gen_data)
}
