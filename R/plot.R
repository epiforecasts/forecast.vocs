#' Default plot
#' @export
plot_default <- function(data, ...) {
  check_quantiles(data, req_probs = c(0.05, 0.2, 0.8, 0.95))
  plot <- ggplot(data) +
    aes(...) +
    geom_line(aes(y = median), size = 1, alpha = 0.6) +
    geom_line(aes(y = mean), linetype = 2) +
    geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.2, size = 0.2) +
    geom_ribbon(aes(ymin = q20, ymax = q80, col = NULL), alpha = 0.2)
  return(plot)
}

#' Add the default plot theme
#' @export
plot_theme <- function(plot) {
  plot <- plot +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

#' Add the forecast date to a plot
#' @export
add_forecast_dates <- function(plot, forecast_dates) {
  if (!is.null(forecast_dates)) {
    forecast_dates <- data.table(
      "Data unavailable" = names(forecast_dates),
      dates = as.Date(forecast_dates)
    )
    plot <- plot +
      geom_vline(
        data = forecast_dates,
        aes(
          xintercept = dates,
          linetype = .data[["Data unavailable"]]
        ),
        size = 1.1, alpha = 0.9
      ) +
      scale_linetype_manual(values = 2:6)
  }
  return(plot)
}

#' Plot the posterior prediction for cases
#' @export
#' @importFrom scales comma log_trans
plot_cases <- function(posterior, obs, forecast_dates = NULL,
                       log = TRUE) {
  setnames(posterior$cases, "type", "Type", skip_absent = TRUE)
  plot <- plot_default(posterior$cases, x = date, col = Type, fill = Type)

  if (!missing(obs)) {
    plot <- plot +
      geom_point(data = obs, aes(y = cases, col = NULL, fill = NULL))
  }

  if (log) {
    plot <- plot +
      scale_y_continuous(labels = scales::comma, trans = scales::log_trans()) +
      labs(y = "Weekly test postive cases (log scale)", x = "Date")
  } else {
    plot <- plot +
      scale_y_continuous(labels = scales::comma) +
      labs(y = "Weekly test postive cases", x = "Date")
  }

  plot <- plot +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")

  plot <- plot_theme(plot)
  plot <- add_forecast_dates(plot, forecast_dates)
  return(plot)
}

#' Plot the posterior prediction for the fraction of samples with the DELTA
#' variant
#' @export
#' @importFrom scales percent
plot_delta <- function(posterior, obs, forecast_dates = NULL) {
  plot <- plot_default(posterior$delta, x = date)

  if (!missing(obs)) {
    obs <- obs[!is.na(seq_available)]
    plot <- plot +
      geom_point(data = obs, aes(y = share_delta))
  }

  plot <- plot +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = "Percentage of overall cases with the DELTA variant",
      x = "Date"
    )

  plot <- plot_theme(plot)
  plot <- add_forecast_dates(plot, forecast_dates)
  return(plot)
}

#' Plot the posterior prediction for the reproduction number
#' @export
plot_rt <- function(posterior, forecast_dates = NULL) {
  setnames(posterior$rt, "type", "Type", skip_absent = TRUE)
  plot <- plot_default(posterior$rt, x = date, col = Type, fill = Type)
  plot <- plot +
    geom_hline(yintercept = 1, linetype = 3, col = "black")

  plot <- plot +
    scale_y_continuous() +
    labs(
      y = "Effective reproduction number of observed cases",
      x = "Date"
    )
  plot <- plot_theme(plot)
  plot <- add_forecast_dates(plot, forecast_dates)
  return(plot)
}
plot_model <- function(posterior) {
  dt <- posterior$model
}
#' Plot posterior predictions
#' @export
#' @importFrom purrr walk2
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' posterior <- summarise_posterior(fit)
#' plot_posterior(posterior, obs)
#' }
plot_posterior <- function(posterior, obs, forecast_dates = NULL,
                           save_path, type = "png") {
  plots <- list()
  plots$cases <- plot_cases(posterior, obs, forecast_dates, log = FALSE)
  plots$log_cases <- plot_cases(posterior, obs, forecast_dates,
    log = TRUE
  )
  plots$delta <- plot_delta(posterior, obs, forecast_dates)
  plots$rt <- plot_rt(posterior, forecast_dates)

  if (!missing(save_path)) {
    walk2(
      plots, names(plots),
      ~ ggsave(file.path(save_path, paste0(.y, ".", type)), .x,
        height = 6, width = 9
      )
    )
  }
  return(plots)
}

#' Pairs plot of parameters of interest and fitting diagnostics
#'
#' @param pars Character vector of parameters to try and include
#' in the plot. Will only be included if present in the fitted model.
#' @param ... Additional parameters passed to `mcmc_pairs()`.
#' @inheritParams stan_fit
#' @inheritParams summarise_posterior
#' @importFrom bayesplot nuts_params mcmc_pairs
#' @return  A ggplot2 based pairs plot of parameters of interest
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' plot_pairs(fit)
#' }
plot_pairs <- function(fit,
                       pars = c(
                         "r_init", "r_noise", "beta", "delta_noise[1]",
                         "ndelta_noise[1]", "init_cases",
                         "init_cases[1]", "init_cases[2]",
                         "eta[1]", "delta_eta[1]", "ndelta_eta[1]",
                         "sqrt_phi[1]", "sqrt_phi[2]", "sqrt_phi"
                       ),
                       diagnostics = TRUE, ...) {
  draws <- extract_draws(fit)
  stanfit <- convert_to_stanfit(fit)
  vars <- names(stanfit)
  present_pars <- intersect(vars, pars)
  np <- NULL
  if (diagnostics) {
    np <- nuts_params(stanfit)
  }

  pairs <- mcmc_pairs(draws,
    np = np,
    pars = present_pars,
    ...
  )
  return(pairs)
}
