#' @export
stan_data <- function(cases, horizon = 4) {
  cases <- data.table::as.data.table(cases)
  data <- list(
    # time indices
    t = nrow(cases) + 4,
    t_nots = nrow(cases),
    t_seq = nrow(cases[!is.na(seq_B.1.1617.2)]),
    # weekly incidences
    X  = cases$inc7,
    # total number of sequenced samples
    N = cases[!is.na(seq_total)]$seq_total,
    # number of sequenced samples with delta variant
    Y = cases[!is.na(seq_total)]$seq_B.1.1617.2
  )
  return(data)
}

#' @export
stan_inits <- function(data) {
  init_fn <- function() {
    inits <- list(
      init_cases = purrr::map_dbl(
        c(data$X[1] * data$Y[1] / data$N[1], data$X[1]),
        ~ abs(rnorm(1, ., . * 0.01))),
      r = rnorm(1, 0, 0.25),
      r_noise = abs(rnorm(1, 0, 0.01)),
      delta_mod = rnorm(1, 0.25, 0.05),
      delta_noise = abs(rnorm(1, 0, 0.01)),
      sqrt_phi = abs(rnorm(2, 0, 0.01))
    )
    return(inits)
  }
  return(init_fn)
}

#' @export
stan_fit <- function(data,
                    model = system.file("stan/model.stan",
                                        package = "bp.delta"),
                     save_path, diagnostics = TRUE) {
  mod <- cmdstanr::cmdstan_model(model)

  fit <- mod$sample(data = data, ...)

  if (!missing(save_path)) {
    fit$save_object(file = file.path(save_path, "fit.rds"))
  }

  if (diagnostics) {
    fit$cmdstan_diagnose()
  }

  sfit <- fit$summary()
  sfit <- data.table::setDT(sfit)

  if (!missing(save_path)) {
    data.table::fwrite(sfit, file.path(save_path, "summarised_posterior.csv"))
  }

  out <- list(
    fit = fit,
    posterior = summarised_posterior
  )
  return(out)
}