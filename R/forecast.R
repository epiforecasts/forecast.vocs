
#' Forecast using branching processes at a target date
#' @param forecast_date Date at which to forecast. Defaults to the
#' maximum date in `obs`.
#' @inheritParams filter_by_availability
#' @export
#' @importFrom purrr map transpose reduce
forecast <- function(obs,
                     plot_obs = latest_obs(obs),
                     forecast_date = max(obs$date),
                     seq_date = forecast_date, case_date = forecast_date,
                     save_path = tempdir(), horizon = 4,
                     delta = c(0.2, 0.2), strains = 2,
                     models = NULL, likelihood = TRUE, output_loglik = FALSE,
                     probs = c(
                       0.01, 0.025, seq(0.05, 0.95, by = 0.05),
                       0.975, 0.99
                     ),
                     ...) {
  # resolve  data availability
  target_obs <- filter_by_availability(
    obs,
    date = forecast_date,
    seq_date = seq_date, case_date = case_date
  )

  # add date to saving paths
  date_path <- file.path(save_path, forecast_date)

  # format data and fit models
  data <- stan_data(target_obs,
    horizon = horizon, delta = delta,
    likelihood = likelihood, output_loglik = output_loglik
  )

  # forecast required strain models
  strain_fits <- purrr::map(
    seq_along(strains),
    function(strain, ...) {
      forecast_n_strain(
        model = models[[strain]],
        strains = strains[strain], data = data,
        probs = probs,
        save_path = date_path, ...
      )
    },
    ...
  )
  names(strain_fits) <- paste0(strains, "_strains")
  posteriors <- purrr::transpose(strain_fits)
  posteriors <- combine_posteriors(posteriors$tidy_posterior)

  save_posterior(posteriors, save_path = date_path)

  plots <- plot_posterior(posteriors, plot_obs,
    forecast_date = forecast_date,
    save_path = date_path
  )
  out <- list(posteriors = posteriors, plots = plots, models = strain_fits)
  return(out)
}
#' Forecast using a single branching process
#' @export
#' @importFrom purrr map
forecast_n_strain <- function(data, model = NULL, strains = 2,
                              save_path = tempdir(),
                              probs = c(
                                0.01, 0.025, seq(0.05, 0.95, by = 0.05),
                                0.975, 0.99
                              ),
                              ...) {
  save_path <- file.path(save_path, paste0(strains, "_strains"))
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  inits <- stan_inits(data, strains = strains)

  if (is.null(model)) {
    model <- load_model(strains = strains)
  }

  # fit and summarise variant model
  fit <- stan_fit(
    model = model, data = data,
    init = inits, save_path = save_path,
    ...
  )

  fit$tidy_posterior <- summarise_posterior(fit, probs = probs)
  return(fit)
}
