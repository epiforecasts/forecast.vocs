# Dual-strain branching process model
# Sam Abbott sam.abbott@lshtm.ac.uk
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# deps
library(data.table)
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos"))) 
library(cmdstanr)
library(ggplot2)
library(scales)
library(purrr)

# get stan setup
# install_cmdstan()

# set number of cores
options(mc.cores = 4)

# load data
cases_sat <- fread("data/cases.csv")

# extract variables for model:
data <- list(
  # time indices
  t = nrow(cases_sat) + 4,
  t_nots = nrow(cases_sat),
  t_seq = nrow(cases_sat[!is.na(seq_B.1.1617.2)]),
  # weekly incidences
  X  = cases_sat$inc7,
  # total number of sequenced samples
  N = cases_sat[!is.na(seq_total)]$seq_total,
  # number of sequenced samples with delta variant
  Y = cases_sat[!is.na(seq_total)]$seq_B.1.1617.2
)

# compile model
mod <- cmdstan_model("model.stan")

# set up initialisation
init_fn <- function() {
  inits <- list(
    init_cases = map_dbl(c(data$X[1] * data$Y[1] / data$N[1], data$X[1]),
                         ~ abs(rnorm(1, ., . * 0.01))),
    beta = rnorm(1, 0, 0.25),
    beta_noise = abs(rnorm(1, 0, 0.01)),
    delta_mod = rnorm(1, 0.25, 0.05),
    delta_noise = abs(rnorm(1, 0, 0.01)),
    sqrt_phi = abs(rnorm(2, 0, 0.01))
  )
  return(inits)
}

# fit model using NUTS
fit <- mod$sample(data = data, adapt_delta = 0.99, max_treedepth = 15,
                  init = init_fn)

# assess fit
fit$cmdstan_diagnose()

# summarise posterior
sfit <- fit$summary()

# save fit
fit$save_object(file = "results/stan-fit.rds")

# save summarised posterior
sfit <- setDT(sfit)
fwrite(sfit, "results/stan-posterior.csv")

# extract cases posterior predictions
posterior_case_preds <- sfit[grepl("sim_", variable)]
posterior_case_preds[, date := rep(seq(min(cases_sat$dat),
                                   by = "week", length.out = data$t), 3)]
posterior_case_preds[, Variant := fcase(
  grepl("_beta", variable), "BETA",
  grepl("_delta", variable), "DELTA",
  default = "Overall"
)]

plot_case_posterior <- function(dt, log = TRUE) {
  axis_label <- "Weekly test postive cases"
  plot <- ggplot(dt) +
  aes(x = date, y = median, col = Variant, fill = Variant) +
  geom_line(size = 1.1, alpha = 0.6) +
  geom_line(aes(y = mean), linetype = 2) +
  geom_ribbon(aes(ymin = q5, ymax = q95, ), alpha = 0.3, size = 0.4) +
  geom_point(data = cases_sat, aes(y = inc7, col = NULL, fill = NULL)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90))

  if (log) {
    plot <- plot +
      scale_y_continuous(labels = comma, trans = log_trans()) +
      labs(y = "Weekly test postive cases (log scale)", x = "Date")
  }else{
    plot <- plot +
      scale_y_continuous(labels = comma) +
      labs(y = "Weekly test postive cases", x = "Date")
  }
  return(plot)
}
# plot case posterior
plot_case_post <- plot_case_posterior(posterior_case_preds, log = FALSE)
plot_case_post

ggsave("plots/stan-posterior-cases.pdf", plot_case_post,
       height = 6, width = 9)

plot_log_case_post <- plot_case_posterior(posterior_case_preds, log = TRUE)
plot_log_case_post

ggsave("plots/stan-posterior-cases.pdf", plot_log_case_post,
       height = 6, width = 9)


# extract fraction DELTA
delta_frac <- sfit[grepl("frac_delta", variable)]
delta_frac[, date := seq(min(cases_sat$dat), by = "week", length.out = data$t)]

# plot DELTA fraction
plot_delta_frac <- ggplot(delta_frac) +
  aes(x = date, y = median) +
  geom_line(size = 1.1, alpha = 0.6) +
  geom_line(aes(y = mean), linetype = 2) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3, size = 0.4) +
  geom_point(data = cases_sat, aes(y = share_B.1.1617.2)) +
  scale_y_continuous(labels = percent) +
  theme_bw() +
  labs(y = "Percentage of overall cases with the DELTA variant",
       x = "Date") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90))
plot_delta_frac

ggsave("plots/stan-posterior-delta-frac.pdf", plot_delta_frac,
       height = 6, width = 9)

# extract Rt estimates
posterior_rt <- sfit[grepl("_r", variable)]
posterior_rt[, date := rep(seq(min(cases_sat$dat),
                               by = "week", length.out = data$t - 1), 2)]
posterior_rt[, Variant := fcase(
  grepl("beta", variable), "BETA",
  grepl("delta", variable), "DELTA"
)]
cols <- c("mean", "median", "q5", "q95")
posterior_rt[, (cols) := lapply(.SD, exp), .SDcols = cols, by = "Variant"]

# plot Rt estimates
plot_rt <- ggplot(posterior_rt) +
  aes(x = date, y = median, col = Variant, fill = Variant) +
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
plot_rt

ggsave("plots/stan-posterior-rt.pdf", plot_rt,
       height = 6, width = 9)
