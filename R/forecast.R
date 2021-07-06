#' @export
#' @importFrom purrr map
forecast <- function(cases, target_date = max(cases$date),
                     save_path = tempdir(), horizon = 4, ...) {

  if (target_date != max(cases$date)) {
    target_cases <- cases[date <= target_date]
    cols <- c("seq_total", "seq_B.1.1617.2", "share_B.1.1617.2")
    target_cases <- target_cases[,
      (cols) := purrr::map(.SD, ~ c(.[1:(.N - 2)], NA, NA)), .SDcols = cols]
  }else{
    target_cases <- copy(cases)
  }

  # saving paths
  date_path <- file.path(path, target_date)
  variant_path <- file.path(date_path, "variant")
  baseline_path <- file.path(date_path, "baseline")
  dir.create(date_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(baseline_path, showWarnings = FALSE)
  dir.create(variant_path, showWarnings = FALSE)

  # format data and fit models
  data <- stan_data(target_cases, horizon = horizon)
  inits <- stan_inits(data)

  # fit and summarise variant model
  fit <- stan_fit(
    data = data, init = inits,
    save_path = variant_path, ...
  )

  posterior <- summarise_posterior(fit, target_cases)

  plots <- plot_posterior(posterior, cases, forecast_date = target_date,
                          save_path = variant_path)

  out <- list(fit = fit, posterior = posterior, plots = plots)
  return(out)
}
