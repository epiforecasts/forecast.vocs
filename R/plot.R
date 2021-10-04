#' Add the default plot theme
#' @param plot `ggplot2` object
#' @export
plot_theme <- function(plot) {
  plot <- plot +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

#' Add the forecast dates to a plot
#' @inheritParams plot_theme
#' @inheritParams plot_default
#' @export
add_forecast_dates <- function(plot, forecast_dates = NULL) {
  if (!is.null(forecast_dates)) {
    forecast_dates <- data.table(
      "Date unavailable" = names(forecast_dates),
      dates = as.Date(forecast_dates)
    )
    plot <- plot +
      geom_vline(
        data = forecast_dates,
        aes(
          xintercept = dates,
          linetype = .data[["Date unavailable"]]
        ),
        size = 1.1, alpha = 0.9
      ) +
      scale_linetype_manual(values = 2:6)
  }
  return(plot)
}

#' Default posterior plot
#'
#' @param obs A data frame of observed data as produced by `latest_obs()`.
#' @param target A character string indicating which variable to extract
#' from the posterior list.
#' @param all_obs Logical, defaults to `FALSE`. Should all observations be plot
#' or just those in the date range of the estimates being plot.
#' @param ... Additional arguments passed to `ggplot2::aes()`
#' @inheritParams extract_forecast_dates
#' @export
plot_default <- function(posterior, target, obs = NULL, forecast_dates = NULL,
                         all_obs = FALSE, ...) {
  data <- posterior[value_type %in% target]
  setnames(data, "type", "Type", skip_absent = TRUE)

  forecast_dates <- extract_forecast_dates(posterior, forecast_dates)

  check_quantiles(data, req_probs = c(0.05, 0.2, 0.8, 0.95))
  plot <- ggplot(data) +
    aes(...)

  plot <- add_forecast_dates(plot, forecast_dates)

  plot <- plot +
    geom_line(aes(y = median), size = 1, alpha = 0.6) +
    geom_line(aes(y = mean), linetype = 2) +
    geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.2, size = 0.2) +
    geom_ribbon(aes(ymin = q20, ymax = q80, col = NULL), alpha = 0.2)

  if (is.null(obs)) {
    if (is.null(data[["obs"]])) {
      data[, obs := NA_real_]
    }
    obs <- data[, .(date, value = obs)]
  }
  obs <- unique(obs)
  obs <- obs[!is.na(value)]

  if (nrow(obs) > 0) {
    if (!all_obs) {
      obs <- obs[date <= max(data$date) & date >= min(data$date)]
    }
    plot <- plot +
      geom_point(data = obs, aes(y = value, col = NULL, fill = NULL))
  }
  return(plot)
}

#' Plot the posterior prediction for cases
#' @param log Logical, defaults to `TRUE`. Should cases be plot on
#' a log scale?
#' @inheritParams plot_default
#' @export
#' @importFrom scales comma log_trans
plot_cases <- function(posterior, obs = NULL, forecast_dates = NULL,
                       log = TRUE, all_obs = FALSE) {
  if (!is.null(obs)) {
    obs <- copy(obs)[, .(date, value = cases)]
  }
  plot <- plot_default(posterior, "cases", obs, forecast_dates,
    all_obs = all_obs, x = date, col = Type, fill = Type
  )

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
  return(plot)
}

#' Plot the posterior prediction for the fraction of samples with the variant
#' of concern
#' @param voc_label Character string giving the name to assign to the variant
#' of concern. Defaults to  "variant of concern".
#' @inheritParams plot_default
#' @export
#' @importFrom scales percent
plot_voc <- function(posterior, obs = NULL, forecast_dates = NULL,
                     all_obs = FALSE, voc_label = "variant of concern") {
  if (!is.null(obs)) {
    obs <- copy(obs)[, .(date, value = share_voc)]
  }
  plot <- plot_default(posterior, "voc", obs, forecast_dates,
    all_obs = FALSE, x = date
  )

  plot <- plot +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = paste0("Percentage of overall cases with the ", voc_label),
      x = "Date"
    )

  plot <- plot_theme(plot)
  return(plot)
}

#' Plot the posterior prediction for the reproduction number
#' @inheritParams plot_default
#' @export
plot_rt <- function(posterior, forecast_dates = NULL) {
  plot <- plot_default(posterior, "rt",
    obs = NULL,
    forecast_dates, x = date, col = Type,
    fill = Type
  )
  plot <- plot +
    geom_hline(yintercept = 1, linetype = 3, col = "black")

  plot <- plot +
    scale_y_continuous() +
    labs(
      y = "Effective reproduction number of observed cases",
      x = "Date"
    )
  plot <- plot_theme(plot)
  return(plot)
}

#' Plot posterior predictions
#'
#' @param save_path A character string indicating where to save plots
#' if required.
#' @param type A character string indicating the format to use to save plots.
#' @export
#' @inheritParams plot_cases
#' @inheritParams plot_voc
#' @importFrom purrr walk2
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_covid19_delta_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' posterior <- summarise_posterior(fit)
#' plot_posterior(posterior)
#' }
plot_posterior <- function(posterior, obs = NULL, forecast_dates = NULL,
                           all_obs = FALSE, save_path = NULL, type = "png",
                           voc_label = "variant of concern") {
  plots <- list()
  plots$cases <- plot_cases(posterior, obs, forecast_dates,
    log = FALSE,
    all_obs = all_obs
  )
  plots$log_cases <- plot_cases(posterior, obs, forecast_dates,
    log = TRUE, all_obs = all_obs
  )
  if (nrow(posterior[value_type %in% "voc"]) > 0) {
    plots$voc <- plot_voc(posterior, obs, forecast_dates,
      all_obs = all_obs,
      voc_label = voc_label
    )
  }
  plots$rt <- plot_rt(posterior, forecast_dates)

  if (!is.null(save_path)) {
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
#' obs <- latest_obs(germany_covid19_voc_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_voc = 0.99, max_treedepth = 15)
#' plot_pairs(fit)
#' }
plot_pairs <- function(fit,
                       pars = c(
                         "r_init", "r_noise", "beta", "voc_noise[1]",
                         "nvoc_noise[1]", "init_cases",
                         "init_cases[1]", "init_cases[2]",
                         "eta[1]", "voc_eta[1]", "nvoc_eta[1]",
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
