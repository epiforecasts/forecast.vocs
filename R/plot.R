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
    theme(legend.position = "bottom") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

#' Add the forecast date to a plot
#' @export
add_forecast_date <- function(plot, forecast_date) {
  if (!is.null(forecast_date)) {
    plot <- plot +
      geom_vline(xintercept = as.Date(forecast_date), linetype = 3, size = 1.1)
  }
  return(plot)
}

#' Plot the posterior prediction for cases
#' @export
#' @importFrom scales comma log_trans
plot_cases <- function(posterior, cases, forecast_date = NULL,
                       log = TRUE) {
  setnames(posterior$cases, "type", "Type", skip_absent = TRUE)
  plot <- plot_default(posterior$cases, x = date, col = Type, fill = Type)

  if (!missing(cases)) {
    plot <- plot +
      geom_point(data = cases, aes(y = inc7, col = NULL, fill = NULL))
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
  plot <- add_forecast_date(plot, forecast_date)
  return(plot)
}

#' Plot the posterior prediction for the fraction of samples with the DELTA
#' variant
#' @export
#' @importFrom scales percent
plot_delta <- function(posterior, obs_delta, forecast_date = NULL) {
  plot <- plot_default(posterior$delta, x = date)

  if (!missing(obs_delta)) {
    plot <- plot +
      geom_point(data = obs_delta, aes(y = share_delta))
  }

  plot <- plot +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = "Percentage of overall cases with the DELTA variant",
      x = "Date"
    )

  plot <- plot_theme(plot)
  plot <- add_forecast_date(plot, forecast_date)
  return(plot)
}

#' Plot the posterior prediction for the reproduction number
#' @export
plot_rt <- function(posterior, forecast_date = NULL) {
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
  plot <- add_forecast_date(plot, forecast_date)
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
#' dt <- stan_data(germany_cases)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' posterior <- summarise_posterior(fit)
#' plot_posterior(posterior, germany_cases)
#' }
plot_posterior <- function(posterior, cases, forecast_date = NULL,
                           save_path, type = "png") {
  plots <- list()
  plots$cases <- plot_cases(posterior, cases, forecast_date, log = FALSE)
  plots$log_cases <- plot_cases(posterior, cases, forecast_date,
    log = TRUE
  )
  plots$delta <- plot_delta(posterior, cases, forecast_date)
  plots$rt <- plot_rt(posterior, forecast_date)

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
