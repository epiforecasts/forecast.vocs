#' Forecast using branching processes at a target date
#'
#' @param models A model as supplied by [fv_model()]. If not supplied the
#' default for that strain is used. If multiple strain models are being forecast
#' then `models` should be a list models.
#'
#' @param data_list A function that returns a list of data as ingested by the
#' `inits` and `fit` function. Must use arguments as defined in
#' [fv_as_data_list()]. If not supplied the package default [fv_as_data_list()]
#' is used.
#'
#' @param inits A function that returns a function to samples initial
#' conditions with the same arguments as [fv_inits()]. If not supplied the
#' package default [fv_inits()] is used.
#'
#' @param fit A function that fits the supplied model with the same arguments
#' and return values as [fv_sample()]. If not supplied the
#' package default [fv_sample()] is used which performs MCMC sampling using
#' [cmdstanr].
#'
#' @param posterior A function that summarises the output from the supplied
#' fitting function with the same arguments and return values (depending on
#' the requirement for downstream package functionality to function) as
#' [fv_tidy_posterior()]. If not supplied the package default
#' [fv_tidy_posterior()] is used.
#'
#' @param extract_forecast A function that extracts the forecast from
#' the summarised `posterior`. If not supplied the package default
#' [fv_extract_forecast()] is used.
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
#' situations where multiple models are being fit.
#'
#' @param id ID to assign to this forecast. Defaults to 0.
#'
#' @param ... Additional parameters passed to [fv_sample()].
#'
#' @return A `data.frame` containing the output of [fv_sample()] in each row as
#' well as the summarised posterior, forecast and information about the
#' parameters specified.
#'
#' @family forecast
#' @inheritParams filter_by_availability
#' @inheritParams fv_as_data_list
#' @inheritParams fv_sample
#' @inheritParams fv_tidy_posterior
#' @export
#' @importFrom purrr map transpose reduce map_chr safely
#' @examplesIf interactive()
#' options(mc.cores = 4)
#'
#' forecasts <- forecast(
#'   germany_covid19_delta_obs,
#'   forecast_date = as.Date("2021-06-12"),
#'   horizon = 4,
#'   strains = c(1, 2),
#'   adapt_delta = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#' # inspect forecasts
#' forecasts
#'
#' # unnest posteriors
#' forecasts <- unnest_posterior(forecasts)
#'
#' # plot case posterior predictions
#' plot_cases(forecasts, log = TRUE)
#'
#' # plot voc posterior predictions
#' plot_voc_advantage(forecasts)
forecast <- function(obs,
                     forecast_date = max(obs$date),
                     seq_date = forecast_date, case_date = forecast_date,
                     data_list = forecast.vocs::fv_as_data_list,
                     inits = forecast.vocs::fv_inits,
                     fit = forecast.vocs::fv_sample,
                     posterior = forecast.vocs::fv_tidy_posterior,
                     extract_forecast = forecast.vocs::fv_extract_forecast,
                     horizon = 4, r_init = c(0, 0.25), voc_scale = c(0, 0.2),
                     voc_label = "VOC", strains = 2,
                     variant_relationship = "pooled", overdispersion = TRUE,
                     models = NULL, likelihood = TRUE, output_loglik = FALSE,
                     debug = FALSE, keep_fit = TRUE, scale_r = 1, digits = 3,
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
  stopifnot(
    "Strains is not a unique vector" =
      length(strains) == length(unique(strains))
  )

  # resolve  data availability
  target_obs <- filter_by_availability(
    obs,
    date = forecast_date,
    seq_date = seq_date,
    case_date = case_date
  )

  # format data and fit models
  data <- data_list(target_obs,
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
          inits = inits,
          fit = fit,
          posterior = posterior,
          extract_forecast = extract_forecast,
          strains = strains[strain],
          data = data,
          probs = probs,
          scale_r = scale_r,
          digits = digits,
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
    suppressWarnings(forecasts[, c("fit", "data", "fit_args") := NULL])
  }
  class(forecasts) <- c("fv_forecast", class(forecasts))
  return(forecasts[])
}

#' Forecast for a single model and summarise
#'
#' @family forecast
#' @inheritParams fv_inits
#' @inheritParams forecast
#' @inheritParams fv_sample
#' @inheritParams fv_tidy_posterior
forecast_n_strain <- function(data, model = NULL,
                              inits = forecast.vocs::fv_inits,
                              fit = forecast.vocs::fv_sample,
                              posterior = forecast.vocs::fv_tidy_posterior,
                              extract_forecast = forecast.vocs::fv_extract_forecast, # nolint
                              strains = 2, voc_label = "VOC",
                              probs = c(0.05, 0.2, 0.8, 0.95),
                              digits = 3, scale_r = 1, ...) {
  inits <- inits(data, strains = strains)

  if (is.null(model)) {
    model <- fv_model(strains = strains)
  }

  # fit and summarise
  fit <- fit(
    model = model, data = data, init = inits, ...
  )

  fit$posterior <- list(posterior(
    fit,
    probs = probs, voc_label = voc_label, scale_r = scale_r,
    digits = digits
  ))
  fit$forecast <- list(extract_forecast(fit$posterior[[1]]))
  return(fit)
}
