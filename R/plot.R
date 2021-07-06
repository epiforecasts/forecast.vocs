#' @export
plot_cases <- function(posterior_cases, cases, log = TRUE) {
  plot <- ggplot(posterior_cases) +
  aes(x = date, y = median, col = Type, fill = Type) +
  geom_line(size = 1.1, alpha = 0.6) +
  geom_line(aes(y = mean), linetype = 2) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3, size = 0.4) +
  geom_point(data = cases, aes(y = inc7, col = NULL, fill = NULL)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90))

  if (log) {
    plot <- plot +
      scale_y_continuous(labels = scales::comma, trans = log_trans()) +
      labs(y = "Weekly test postive cases (log scale)", x = "Date")
  }else{
    plot <- plot +
      scale_y_continuous(labels = scales::comma) +
      labs(y = "Weekly test postive cases", x = "Date")
  }
  return(plot)
}
#' @export
plot_delta <- function(posterior_delta, obs_delta) {
  ggplot(posterior_delta) +
    aes(x = date, y = median) +
    geom_line(size = 1.1, alpha = 0.6) +
    geom_line(aes(y = mean), linetype = 2) +
    geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3, size = 0.4) +
    geom_point(data = obs_delta, aes(y = share_B.1.1617.2)) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    labs(y = "Percentage of overall cases with the DELTA variant",
        x = "Date") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme(axis.text.x = element_text(angle = 90))
}
#' @export
plot_rt <- function(posterior_rt) {
  ggplot(posterior_rt) +
    aes(x = date, y = median, col = Type, fill = Type) +
    geom_hline(yintercept = 1, linetype = 3, col = "black") +

    geom_line(size = 1.1, alpha = 0.6) +
    geom_line(aes(y = mean), linetype = 2) +
    geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3, size = 0.4) +
    scale_y_continuous() +
    theme_bw() +
    labs(y = "Effective reproduction number of observed cases",
        x = "Date") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(legend.position = "bottom")
}
