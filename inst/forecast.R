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
# plot case posterior
plot_cases <- plot_cases(posterior_case_preds, log = FALSE)
plot_cases

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

plot_delta_frac

ggsave("plots/stan-posterior-delta-frac.pdf", plot_delta_frac,
       height = 6, width = 9)

# plot Rt estimates
plot_rt

ggsave("plots/stan-posterior-rt.pdf", plot_rt,
       height = 6, width = 9)
