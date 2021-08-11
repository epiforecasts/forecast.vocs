
#' Forecast using branching processes at a target date
#' @param forecast_date Date at which to forecast. Defaults to the
#' maximum date in `obs`.
#' @param plot Logical, should posterior plots be produced and saved.
#' Defaults to `TRUE`.
#' @inheritParams filter_by_availability
#' @inheritParams stan_data
#' @inheritParams forecast_n_strain
#' @export
#' @importFrom purrr map transpose reduce
#' @examples
#' \dontrun{
#' options(mc.cores = 4)
#' results <- forecast(
#'   latest_obs(germany_obs),
#'   horizon = 4,
#'   save_path = tempdir(),
#'   strains = c(1, 2),
#'   adapt_delta = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#' # inspect object
#' names(results)
#'
#' # look at plots
#' names(results$plots)
#' results$plots$cases
#' results$plots$log_cases
#' results$plots$delta
#' results$plots$rt
#' }
forecast <- function(obs,
                     plot_obs = bp.delta::latest_obs(obs),
                     forecast_date = max(obs$date),
                     seq_date = forecast_date, case_date = forecast_date,
                     save_path = NULL, horizon = 4,
                     delta = c(0.2, 0.2), strains = 2,
                     variant_relationship = "pooled", overdispersion = TRUE,
                     models = NULL, likelihood = TRUE, output_loglik = FALSE,
                     probs = c(
                       0.01, 0.025, seq(0.05, 0.95, by = 0.05),
                       0.975, 0.99
                     ), plot = TRUE,
                     ...) {
  # resolve  data availability
  target_obs <- filter_by_availability(
    obs,
    date = forecast_date,
    seq_date = seq_date, case_date = case_date
  )

  # add date to saving paths
  if (!is.null(save_path)) {
    date_path <- file.path(save_path, forecast_date)
  } else {
    date_path <- NULL
  }

  # format data and fit models
  data <- stan_data(target_obs,
    horizon = horizon, delta = delta,
    variant_relationship = variant_relationship,
    overdispersion = overdispersion,
    likelihood = likelihood,
    output_loglik = output_loglik
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
  tfits <- purrr::transpose(strain_fits)
  posteriors <- combine_posteriors(tfits$tidy_posterior)
  forecasts <- combine_posteriors(tfits$forecast)

  save_posterior(posteriors, save_path = date_path)
  out <- list(
    posteriors = posteriors,
    forecasts = forecasts,
    models = strain_fits
  )
  if (plot) {
    out$plots <- plot_posterior(posteriors, plot_obs, save_path = date_path)
  }
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
  if (!is.null(save_path)) {
    save_path <- file.path(save_path, paste0(strains, "_strains"))
    dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
  }
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
  fit$forecast <- extract_forecast(fit$tidy_posterior)
  return(fit)
}

#' Forecast across multiple dates
#'
#' @param forecast_dates A list of dates to forecast at.
#' @param ... Additional parameters passed to `forecast()`
#' @inheritParams forecast
#' @importFrom purrr map
#' @export
#' @return A list each containing the output from running `forecast()`
#'  on a single forecast date
forecast_across_dates <- function(obs,
                                  forecast_dates = unique(obs[!is.na(seq_available)])$date[-c(1:3)], # nolint
                                  ...) {
  fits <- purrr::map(
    forecast_dates,
    function(date, ...) {
      forecast(obs, forecast_date = date, ...)
    },
    ...
  )
  names(fits) <- forecast_dates
  return(fits)
}

#' Forecast across multiple scenarios and dates
#'
#' @param scenarios A dataframe of scenarios as produced by
#' `define_scenarios()`. If missing uses the default scenarios
#' from `default_scenarios()`.
#' @param save_path Character string indicating the path to save results
#' to. Defaults to `tempdir()`. Each scenario will be saved in a sub folder.
#' @param ... Additional parameters passed to `forecast_across_dates()`
#' @inheritParams forecast_across_dates
#' @importFrom purrr map2
#' @importFrom future.apply future_lapply
#' @export
#' @return A list each containing the output from running
#' `forecast_across_dates()` on a single scenario.
forecast_across_scenarios <- function(obs, scenarios, save_path = tempdir(),
                                      ...) {
  if (missing(scenarios)) {
    scenarios <- bp.delta::define_scenarios()
  }
  scenarios$obs <- purrr::map2(
    scenarios$seq_lag, scenarios$seq_samples,
    ~ generate_obs_scenario(obs, seq_lag = .x, seq_samples = .y)
  )
  scenarios <- split(scenarios, by = "id")

  forecast_scenario <- function(scenario, ...) {
    forecast_across_dates(
      obs = scenario$obs[[1]],
      delta = scenario$delta[[1]],
      save_path = file.path(save_path, scenario$id[[1]]),
      ...
    )
  }
  fits <- future.apply::future_lapply(
    scenarios,
    forecast_scenario,
    ...
  )
  scenarios$fits <- fits
  return(scenarios)
}
