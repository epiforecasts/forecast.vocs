# Dual-strain branching process model
# Sam Abbott sam.abbott@lshtm.ac.uk
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# deps
library(data.table)
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos"))) 
library(cmdstanr)

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

# fit model using NUTS
fit <- mod$sample(data = data, adapt_delta = 0.9)

# summarise posterior
sfit <- fit$summary()

# extract point estimators and quantiles:
# summary(results, vars = "r_delta") # not currently experted
summary_X_delta <- summary(results, vars = "X_delta")
# summary(results, vars = "r_other") # not currently experted
summary_X_other <- summary(results, vars = "X_other")
summary_X_proj <- summary(results, vars = "X_proj")

# arrange into data.frame:
summary_all <- data.frame(wk = seq(from = cases_sat$wk[1], length.out = nrow(summary_X_delta)),
                      date = seq(from = cases_sat$date[1], length.out = nrow(summary_X_delta), by = 7),
                      X_total_Lower95 = summary_X_proj[, "Lower95"],
                      X_total_Median = summary_X_proj[, "Median"],
                      X_total_Upper95 = summary_X_proj[, "Upper95"],
                      X_delta_Lower95 = summary_X_delta[, "Lower95"],
                      X_delta_Median = summary_X_delta[, "Median"],
                      X_delta_Upper95 = summary_X_delta[, "Upper95"],
                      X_other_Lower95 = summary_X_other[, "Lower95"],
                      X_other_Median = summary_X_other[, "Median"],
                      X_other_Upper95 = summary_X_other[, "Upper95"])

dir.create("results")
write.csv(summary_all, file = paste("results/summary_", data_source, today, ".csv"), row.names = FALSE)

# helper function: 
# modify the alpha value of a given color (to generate transparent versions for prediction bands)
modify_alpha <- function(col, alpha){
  x <- col2rgb(col)/255
  rgb(x[1], x[2], x[3], alpha = alpha)
}

# helper function: add dots and bands to plot
add_result <- function(results, tag, col, line = FALSE){
  transparent_col <- modify_alpha(col, 0.3)
  polygon(c(results$date, rev(results$date)),
          c(results[, paste0("X_", tag, "_Lower95")],
            rev(results[, paste0("X_", tag, "_Upper95")])),
          col = transparent_col, border = NA)
  lines(results$date, results[, paste0("X_", tag, "_Median")], type = ifelse(line, "b", "p"),
        pch = 1, col = col, lty = "dotted", cex = 0.7)
}

# plot:
pdf(paste0("plots/plot_", data_source, "_", today, ".pdf"), width = 9, height = 6)
# aggregated incidence data
plot(summary_all$date, summary_all$X_total_Median, pch = 1, cex = 0.7, ylim = c(0, 20000),
     xlab = "calendar week", ylab = "confirmed cases", 
     main = paste0("7-day absolute case numbers, Germany (", data_source, " data)" ))
lines(cases$date, cases$inc7)
points(cases_sat$date, cases_sat$inc7, pch = 15)
# prediction at aggregate level:
add_result(summary_all, "total", "black")
# non-delta:
add_result(summary_all, "other", "darkgreen")
# delta:
add_result(summary_all, "delta", "red")

# vertical line at current date:
abline(v = today, lty = "dashed")
text(x = today, y = 17000, "today \n", srt = 90, cex = 0.65)

date_last_incidence_data <- max(cases_sat$date)
abline(v = date_last_incidence_data, lty = "dotted")
text(x = date_last_incidence_data, y = 17000, "last incidence \n data used", cex = 0.65, srt = 90)

date_last_sequencing_data <- max(cases_sat$date[!is.na(cases_sat$seq_B.1.1617.2)])
abline(v = date_last_sequencing_data, lty = "dotted")
text(x = date_last_sequencing_data,
     y = 17000, "last sequencing \n data used", cex = 0.65, srt = 90)


legend("topleft", legend = c("total", "non-delta", "delta", "(points mark Saturdays)"),
       col = c("black", "darkgreen", "red", NA), pch = 15, lty = c(1, NA, NA, NA), bty = "n")

legend("left", pch = c(15, 1, 15), col = c("black", "black", "lightgrey"), 
       legend = c("observed", "unobserved or predicted", "95% uncertainty intervals"), 
       bty = "n")
dev.off()
