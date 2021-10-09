#' Forecast using branching processes at a target date
#'
#' @param models A model as supplied by `load_model()`. If not supplied the
#' default for that strain is used. If multiple strain models are being forecast
#' then `models` should be a list models.
#'
#' @param forecast_date Date at which to forecast. Defaults to the
#' maximum date in `obs`.
#'
#' @param strains Integer number of strains to use. Defaults to 2. Current
#' maximum is 2. A numeric vector can be passed if forecasts from multiple
#' strain models are desired.
#'
#' @param keep_fit Logical, defaults to `TRUE`. Should the stan model fit be
#' kept and returned. Dropping this can substantially reduce memory usage in
#' situtations where multiple models are being fit.
#'
#' @param id ID to assign to this forecast. Defaults to 0.
#'
#' @param ... Additional parameters passed to `stan_fit()`.
#'
#' @return A dataframe containing the output of `stan_fit()` in each row as
#' well as the summarised posterior, forecast and information about the
#' parameters specified.
#'
#' @family forecast
#' @inheritParams filter_by_availability
#' @inheritParams stan_data
#' @inheritParams stan_fit
#' @inheritParams summarise_posterior
#' @export
#' @importFrom purrr map transpose reduce map_chr safely
#'
#' @examples
#' \dontrun{
#' options(mc.cores = 4)
#' results <- forecast(
#'   latest_obs(germany_covid19_delta_obs),
#'   horizon = 4,
#'   strains = c(1, 2),
#'   adapt_delta = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#' # inspect results
#' results
#'
#' # unnest posteriors
#' results <- unnest_posterior(results)
#'
#' # plot case posterior predictions
#' plot_cases(results, log = TRUE)
#' }
forecast <- function(obs,
                     forecast_date = max(obs$date),
                     seq_date = forecast_date, case_date = forecast_date,
                     horizon = 4, r_init = c(0, 0.25), voc_scale = c(0, 0.2),
                     voc_label = "VOC", strains = 2,
                     variant_relationship = "pooled", overdispersion = TRUE,
                     models = NULL, likelihood = TRUE, output_loglik = FALSE,
                     debug = FALSE, keep_fit = TRUE, scale_r = 1,
                     probs = c(0.05, 0.2, 0.8, 0.95), id = 0, ...) {
  if (!is.null(models)) {
    if (length(models) == 1 & length(strains) == 1) {
      models <- list(models)
    }
    stopifnot(
      "Number of models supplied must be equal to the numer of strain
       forecasts specified." = length(models) == length(strains)
    )
  }

  # resolve  data availability
  target_obs <- filter_by_availability(
    obs,
    date = forecast_date,
    seq_date = seq_date,
    case_date = case_date
  )

  # format data and fit models
  data <- stan_data(target_obs,
    horizon = horizon,
    r_init = r_init,
    voc_scale = voc_scale,
    variant_relationship = variant_relationship,
    overdispersion = overdispersion,
    likelihood = likelihood,
    output_loglik = output_loglik,
    debug = debug
  )

  out <- data.table(
    id = id,
    forecast_date = forecast_date,
    strains = strains,
    overdispersion = overdispersion,
    variant_relationship = variant_relationship,
    r_init = list(purrr::map_chr(r_init, paste, collapse = ", ")),
    voc_scale = list(purrr::map_chr(voc_scale, paste, collapse = ", "))
  )

  # forecast required strain models
  safe_n_forecast <- purrr::safely(forecast_n_strain)
  forecasts <- purrr::map(
    seq_along(strains),
    function(strain, ...) {
      out <- out[strain, ]
      fit <-
        safe_n_forecast(
          model = models[[strain]],
          strains = strains[strain],
          data = data,
          probs = probs,
          scale_r = scale_r,
          ...
        )
      out <- out[, `:=`(results = list(fit$result), error = list(fit$error))]
      return(out)
    },
    ...
  )

  forecasts <- rbindlist(forecasts, fill = TRUE)
  cols <- setdiff(colnames(forecasts), "results")
  forecasts <- cbind(forecasts, rbindlist(forecasts$results, fill = TRUE))
  forecasts[, results := NULL]
  if (!keep_fit) {
    forecasts[, c("fit", "data", "fit_args") := NULL]
  }
  return(forecasts[])
}

#' Forecast for a single model and summarise
#' @inheritParams stan_inits
#' @inheritParams forecast
#' @inheritParams stan_fit
#' @inheritParams summarise_posterior
forecast_n_strain <- function(data, model = NULL, strains = 2,
                              voc_label = "VOC",
                              probs = c(0.05, 0.2, 0.8, 0.95),
                              scale_r = 1, ...) {
  inits <- stan_inits(data, strains = strains)

  if (is.null(model)) {
    model <- load_model(strains = strains)
  }

  # fit and summarise
  fit <- stan_fit(
    model = model, data = data, init = inits, ...
  )
  fit$posterior <- list(summarise_posterior(
    fit,
    probs = probs, voc_label = voc_label, scale_r = scale_r
  ))
  fit$forecast <- list(extract_forecast(fit$posterior[[1]]))
  return(fit)
}
