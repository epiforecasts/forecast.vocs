#' Add the default plot theme
#'
#' @param plot `ggplot2` object
#'
#' @return A `ggplot2` plot with the package theme applied.
#' @family plot
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
#'
#' @param forecast_dates A data.frame in the format produced by
#' [extract_forecast_dates()] (with at least a date variable and a
#' Data unavailable variable)). Specifies when date availability should be
#' add to plots. May contain faceting variables.
#'
#' @return A `ggplot2` plot with dates of data unavailability added.
#'
#' @family plot
#' @inheritParams plot_theme
#' @export
add_forecast_dates <- function(plot, forecast_dates = NULL) {
  if (!is.null(forecast_dates)) {
    if (!nrow(forecast_dates) == 0) {
      plot <- plot +
        geom_vline(
          data = forecast_dates,
          aes(
            xintercept = date,
            linetype = .data[["Data unavailable"]]
          ),
          size = 1.1, alpha = 0.9
        ) +
        scale_linetype_manual(values = 2:6)
    }
  }
  return(plot)
}

#' Default posterior plot
#'
#' @param obs A data frame of observed data as produced by [latest_obs()].
#'
#' @param target A character string indicating which variable to extract
#' from the posterior list.
#'
#' @param all_obs Logical, defaults to `FALSE`. Should all observations be plot
#' or just those in the date range of the estimates being plot.
#'
#' @param ... Additional arguments passed to [ggplot2::aes()]
#'
#' @return A `ggplot2` plot.
#'
#' @family plot
#' @inheritParams add_forecast_dates
#' @inheritParams extract_forecast_dates
#' @importFrom purrr map_lgl
#' @export
plot_default <- function(posterior, target, obs = NULL, forecast_dates = NULL,
                         all_obs = FALSE, ...) {
  data <- posterior[value_type %in% target]
  setnames(data, "type", "Type", skip_absent = TRUE)

  check_quantiles(data, req_probs = c(0.05, 0.2, 0.8, 0.95))

  plot <- ggplot(data) +
    aes(...)

  if (is.null(forecast_dates)) {
    forecast_dates <- extract_forecast_dates(posterior)
  }

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
    obs <- data[, value := obs]
  }
  non_list_cols <- names(obs)[purrr::map_lgl(names(obs), ~ !is.list(obs[[.]]))]
  obs <- unique(obs[, ..non_list_cols])
  obs <- obs[!is.na(value)]

  if (nrow(obs) > 0) {
    if (!all_obs) {
      obs <- obs[date <= max(data$date, na.rm = TRUE) &
        date >= min(data$date, na.rm = TRUE)]
    }
    plot <- plot +
      geom_point(data = obs, aes(y = value, col = NULL, fill = NULL))
  }
  return(plot)
}

#' Plot the posterior prediction for cases
#'
#' @param log Logical, defaults to `TRUE`. Should cases be plot on
#' a log scale?
#'
#' @param col A character string denoting the variable to use to
#' stratify the ribbon plot. Defaults to "type" which indicates the
#' data stream.
#'
#' @return A `ggplot2` plot.
#'
#' @family plot
#' @inheritParams plot_default
#' @export
#' @importFrom scales comma log_trans
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#'
#' # default with log transform
#' plot_cases(posterior)
#'
#' # without log transform
#' plot_cases(posterior)
plot_cases <- function(posterior, obs = NULL, forecast_dates = NULL,
                       all_obs = FALSE, col = NULL, log = TRUE) {
  if (!is.null(obs)) {
    obs <- copy(obs)[, value := cases]
  }
  if (is.null(col)) {
    col <- "Type"
  }
  plot <- plot_default(posterior, "cases", obs, forecast_dates,
    all_obs = all_obs, x = date, col = .data[[col]], fill = .data[[col]]
  )

  if (log) {
    plot <- plot +
      scale_y_continuous(labels = scales::comma, trans = scales::log_trans()) +
      labs(y = "Weekly test positive cases (log scale)", x = "Date")
  } else {
    plot <- plot +
      scale_y_continuous(labels = scales::comma) +
      labs(y = "Weekly test positive cases", x = "Date")
  }

  plot <- plot +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")

  plot <- plot_theme(plot)
  return(plot)
}

#' Plot the posterior prediction for the fraction of samples with the variant
#' of concern
#'
#' @param voc_label Character string giving the name to assign to the variant
#' of concern. Defaults to  "variant of concern".
#'
#' @param ... Additional parameters passed to [plot_default()].
#'
#' @return A `ggplot2` plot.
#'
#' @family plot
#' @inheritParams plot_default
#' @export
#' @importFrom scales percent
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#' plot_voc(posterior)
plot_voc <- function(posterior, obs = NULL, forecast_dates = NULL,
                     all_obs = FALSE, voc_label = "variant of concern", ...) {
  if (!is.null(obs)) {
    obs <- copy(obs)[, value := share_voc]
  }
  plot <- plot_default(posterior, "voc", obs, forecast_dates,
    all_obs = all_obs, x = date, ...
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
#'
#' @return A `ggplot2` plot.
#'
#' @family plot
#' @inheritParams plot_default
#' @inheritParams plot_cases
#' @export
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#' plot_rt(posterior)
plot_rt <- function(posterior, forecast_dates = NULL, col = NULL) {
  if (is.null(col)) {
    col <- "Type"
  }
  plot <- plot_default(
    posterior, "rt",
    obs = NULL,
    forecast_dates, x = date, col = .data[[col]], fill = .data[[col]]
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

#' Plot the posterior prediction for the growth rate
#'
#' @return A `ggplot2` plot.
#'
#' @family plot
#' @export
#' @inheritParams plot_default
#' @inheritParams plot_cases
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#' plot_growth(posterior)
plot_growth <- function(posterior, forecast_dates = NULL, col = NULL) {
  if (is.null(col)) {
    col <- "Type"
  }
  plot <- plot_default(
    posterior, "growth",
    obs = NULL,
    forecast_dates, x = date, col = .data[[col]],
    fill = .data[[col]]
  )
  plot <- plot +
    geom_hline(yintercept = 0, linetype = 3, col = "black")

  plot <- plot +
    scale_y_continuous() +
    labs(
      y = "Growth rate of observed cases",
      x = "Date"
    )
  plot <- plot_theme(plot)
  return(plot)
}


#' Plot posterior predictions
#'
#' @param save_path A character string indicating where to save plots
#' if required.
#'
#' @param type A character string indicating the format to use to save plots.
#'
#' @return A named list of all supported package plots with sensible defaults.
#'
#' @family plot
#' @export
#' @inheritParams plot_cases
#' @inheritParams plot_voc
#' @importFrom purrr walk2
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#' plot_posterior(posterior)
plot_posterior <- function(posterior, obs = NULL, forecast_dates = NULL,
                           save_path = NULL, type = "png",
                           all_obs = FALSE, voc_label = "variant of concern") {
  plots <- list()
  plots$cases <- plot_cases(
    posterior, obs, forecast_dates,
    log = FALSE, all_obs = all_obs
  )
  plots$log_cases <- plot_cases(
    posterior, obs, forecast_dates,
    log = TRUE, all_obs = all_obs
  )
  if (nrow(posterior[value_type %in% "voc"]) > 0) {
    plots$voc <- plot_voc(
      posterior, obs, forecast_dates,
      voc_label = voc_label, all_obs = all_obs
    )
  }
  plots$growth <- plot_growth(posterior, forecast_dates)
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
#'
#' @param ... Additional parameters passed to [bayesplot::mcmc_pairs()].
#'
#' @return  A `ggplot2` based pairs plot of parameters of interest
#'
#' @family plot
#' @family modelvalidation
#' @inheritParams fv_sample
#' @inheritParams fv_posterior
#' @importFrom bayesplot nuts_params mcmc_pairs
#' @examplesIf interactive()
#' obs <- filter_by_availability(
#'   germany_covid19_delta_obs,
#'   date = as.Date("2021-06-12"),
#' )
#' dt <- fv_data(obs)
#' inits <- fv_inits(dt)
#' fit <- fv_sample(dt, init = inits, adapt_voc = 0.99, max_treedepth = 15)
#' plot_pairs(fit)
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
