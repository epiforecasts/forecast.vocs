
#' Forecast using branching processes at a target date
#'
#' @param models A list of models to use named by strain. If not supplied
#' uses the package default model for that strain.
#' @param forecast_date Date at which to forecast. Defaults to the
#' maximum date in `obs`.
#' @param keep_fit Logical, defaults to `TRUE`. Should the stan model fit be
#' kept and returned. Dropping this can substantially reduce memory usage in
#' situtations where multiple models are being fit.
#' @param ... Additional parameters passed to `stan_fit()`.
#' @inheritParams filter_by_availability
#' @inheritParams stan_data
#' @inheritParams forecast_n_strain
#' @inheritParams stan_fit
#' @inheritParams summarise_posterior
#' @export
#' @importFrom purrr map transpose reduce
#' @examples
#' \dontrun{
#' options(mc.cores = 4)
#' results <- forecast(
#'   latest_obs(germany_covid19_delta_obs),
#'   horizon = 4,
#'   save_path = tempdir(),
#'   strains = c(1, 2),
#'   adapt_voc = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#' # inspect object
#' names(results)
#' }
forecast <- function(obs,
                     forecast_date = max(obs$date),
                     seq_date = forecast_date, case_date = forecast_date,
                     save_path = NULL, horizon = 4,
                     voc_scale = c(0, 0.2), strains = 2,
                     variant_relationship = "pooled", overdispersion = TRUE,
                     models = NULL, likelihood = TRUE,
                     output_loglik = FALSE, keep_fit = TRUE,
                     probs = c(
                       0.01, 0.025, seq(0.05, 0.95, by = 0.05),
                       0.975, 0.99
                     ), ...) {
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
    horizon = horizon, voc_scale = voc_scale,
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
        strains = strains[strain],
        data = data,
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

  if (!keep_fit) {
    strain_fits <- purrr::map(strain_fits, function(.) {
      .$fit <- NULL
      return(.)
    })
  }

  out <- list(
    posteriors = posteriors,
    forecasts = forecasts,
    models = strain_fits
  )
  return(out)
}
#' Forecast using a single branching process
#' @export
#' @inheritParams stan_inits
#' @inheritParams forecast
#' @inheritParams stan_fit
#' @inheritParams summarise_posterior
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
