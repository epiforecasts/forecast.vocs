
#' @export
#' @importFrom purrr map transpose reduce
forecast <- function(cases, target_date = max(cases$date),
                     save_path = tempdir(), horizon = 4, strains = 2,
                     models = NULL, likelihood = TRUE, output_loglik = FALSE,
                     ...) {

  if (target_date != max(cases$date)) {
    target_cases <- cases[date <= target_date]
    cols <- c("seq_total", "seq_B.1.1617.2", "share_B.1.1617.2")
    target_cases <- target_cases[,
      (cols) := purrr::map(.SD, ~ c(.[1:(.N - 2)], NA, NA)), .SDcols = cols]
  }else{
    target_cases <- copy(cases)
  }

  # add date to saving paths
  date_path <- file.path(save_path, target_date)

  # format data and fit models
  data <- stan_data(target_cases, horizon = horizon,
                    likelihood = likelihood, output_loglik = output_loglik)

  # forecast required strain models
  strain_fits <- purrr::map(
    seq_along(strains),
    ~ forecast_n_strain(model = models[.], strains = .),
    data = data, save_path = save_path, ...)

  names(strain_fits) <- paste0(strains, "_strains")
  posteriors <- purrr::transpose(strain_fits)
  posteriors <- purrr::reduce(posteriors$tidy_posterior, combine_posteriors)

  save_posterior(posteriors, save_path = date_path)

  plots <- plot_posterior(posteriors, cases,
                          forecast_date = target_date,
                          save_path = date_path)
  out <- list(posteriors = posteriors, plots = plots, models = strain_fits)
  return(out)
}
#' @export
#' @importFrom purrr map
forecast_n_strain <- function(data, model = NULL, strains = 2,
                              save_path = tempdir(),
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

  fit$tidy_posterior <- summarise_posterior(fit)
  return(fit)
}