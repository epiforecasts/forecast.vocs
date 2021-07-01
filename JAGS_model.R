# Dual-strain branching process model
# Johannes Bracher johannes.bracher@kit.edu

setwd("/home/johannes/Documents/Ideas/branching_process_delta")

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

library(runjags)

today <- as.Date("2021-07-01")
# the script is sort of tailored to the data availability on Thursday 1 July
# (with one week for which incidence data are already available while sequencing data are not)

data_source <- "RKI"

# Truth data (aggregate confirmed cases):
if(data_source == "RKI"){
  cases <- read.csv("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Incident%20Cases_Germany.csv")
}
if(data_source == "JHU"){
  cases <- read.csv("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/JHU/truth_JHU-Incident%20Cases_Germany.csv")
}

# some formatting...
cases <- subset(cases, location == "GM")
cases$date <- as.Date(cases$date)
# generate 7-day moving averages
cases$inc7 <- NA; for(i in 7:nrow(cases)) cases$inc7[i] <- sum(cases$value[i - (0:6)])
# subset to Saturdays for comparability to Forecast Hub
cases_sat <- subset(cases, weekdays(cases$date) == "Saturday")
# add calendar weeks:
cases_sat$wk <- as.numeric(format(cases_sat$date, "%W"))
# restrict to most recent weeks
cases_sat <- subset(cases_sat, date >= as.Date("2021-04-24"))

# get data on variants from RKI:
sampling <- read.csv("VOC_VOI_Tabelle.csv")
# re-format calendar week:
sampling$KW <- as.numeric(gsub("KW", "", sampling$KW))
# compute total number of tests performed and some formatting
sampling$B.1.617.2_Anteil.... <- sampling$B.1.617.2_Anteil..../100
sampling$total <- round(sampling$B.1.617.2_Anzahl/sampling$B.1.617.2_Anteil....)
sampling <- sampling[, c("KW", "total", "B.1.617.2_Anzahl", "B.1.617.2_Anteil....")]
colnames(sampling) <- c("wk", "seq_total", "seq_B.1.1617.2", "share_B.1.1617.2")

# merge into case data set
cases_sat <- merge(cases_sat, sampling, by = "wk", all.x = TRUE)

# extract variables for JAGS:
X <- cases_sat$inc7 # weekly incidences
N <- cases_sat$seq_total # total number of sequenced samples
Y <- cases_sat$seq_B.1.1617.2 # number of sequenced samples with delta variant
T1 <- length(Y[!is.na(Y)]) # number of time points for sequencing data
T2 <- T1 + 1 # number of observations for total incidence
T3 <- T2 + 3 # number of time points incl weeks to predict

# Model definition in JAGS language:
model <- "
    model {
      # flat prior on overdispersion parameters (could be refined)
      psi_delta ~ dunif(0, 100)
      psi_other ~ dunif(0, 100)
      # flat prior on precision of random walk (could be refined)
      prec ~ dunif(5, 200)
    
      # initial values of weekly rowth rates
      r_other[1] ~ dunif(0.5, 1.5)
      r_delta[1] ~ dunif(0.5, 1.5)
      
      # vague priors on initial values of incidence per variant
      X_delta[1] ~ dunif(0, 1000)
      X_other[1] ~ dunif(X[1] - 1000, X[1])
      X_proj[1] = X[1]
    
      # run through time points where both incidence and sequencing data are available
      for(t in 2 : T1) { #data# T1
          # multiplicative random walk on weekly growth rates for delta variant and others
          r_other[t] = r_other[t - 1]*exp(epsilon_other[t])
          r_delta[t] = r_delta[t - 1]*exp(epsilon_delta[t])
          epsilon_delta[t] ~ dnorm(0, prec)
          epsilon_other[t] ~ dnorm(0, prec)

          # multiplication factors to express NegBin as Poisson-Gamma mixture
          omega_delta[t] ~ dgamma(psi_delta, psi_delta)
          omega_other[t] ~ dgamma(psi_other, psi_other)
        
          # conditional expectations given the multiplication factor for overdispersion
          lambda_delta[t] = omega_delta[t]*(r_delta[t - 1]*X_delta[t - 1])
          lambda_other[t] = omega_other[t]*(r_other[t - 1]*X_other[t - 1])
        
          # for the branching process a little detour is necessary: X[t] cannot be written as
          # X[t] = X_delta[t] + X_other[t]
          # as observed nodes cannot be logical in JAGS
          # can be circumvented using Poisson convolution and thinning properties
          # sample X[t] first:
          lambda_total[t] = lambda_delta[t] + lambda_other[t]
          X[t] ~ dpois(lambda_total[t])  #data# X
          # then obtain delta share via thinning
          X_delta[t] ~ dbinom(lambda_delta[t]/lambda_total[t], X[t])
          # and others as the resulting difference
          X_other[t] = X[t] - X_delta[t]
        
          # add observation process for sequencing:
          # probabilty that a randomly chosen sample is delta
          p[t] = X_delta[t]/X[t]
          # binomial observation process
          Y[t] ~ dbin(p[t], N[t])  #data# Y, N
          
          # for t = 1, ..., T2 X_proj is just a place holder identical to X[t]
          X_proj[t] = X[t]
      }
      
      # run through time points where incidence data is still available, but not sequencing data:
      for(t in (T1 + 1) : T2){ #data# T2
          # same as above: random walk on r
          r_other[t] = r_other[t - 1]*exp(epsilon_other[t])
          r_delta[t] = r_delta[t - 1]*exp(epsilon_delta[t])
          epsilon_delta[t] ~ dnorm(0, prec)
          epsilon_other[t] ~ dnorm(0, prec)
        
          # multiplication factors for overdispersion
          omega_delta[t] ~ dgamma(psi_delta, psi_delta)
          omega_other[t] ~ dgamma(psi_other, psi_other)
        
          # branching process:
          lambda_delta[t] = omega_delta[t]*(r_delta[t - 1]*X_delta[t - 1])
          lambda_other[t] = omega_other[t]*(r_other[t - 1]*X_other[t - 1])
          lambda_total[t] = lambda_delta[t] + lambda_other[t]
          X[t] ~ dpois(lambda_total[t])
          X_delta[t] ~ dbinom(lambda_delta[t]/lambda_total[t], X[t])
          X_other[t] = X[t] - X_delta[t]
          # X_proj is still a place holder, becomes relevant in next step
          X_proj[t] = X[t]
      }
      
      # run through timepoints for which to predict:
      for(t in (T2 + 1):(T2 + 3)){
          # continue random walk on r
          r_other[t] = r_other[t - 1]*exp(epsilon_other[t])
          r_delta[t] = r_delta[t - 1]*exp(epsilon_delta[t])
          epsilon_delta[t] ~ dnorm(0, prec)
          epsilon_other[t] ~ dnorm(0, prec)
          
          # overdispersion
          omega_delta[t] ~ dgamma(psi_delta, psi_delta)
          omega_other[t] ~ dgamma(psi_other, psi_other)
          
          # branching:
          lambda_delta[t] = omega_delta[t]*(r_delta[t - 1]*X_delta[t - 1])
          lambda_other[t] = omega_other[t]*(r_other[t - 1]*X_other[t - 1])
  
          lambda_total[t] = lambda_delta[t] + lambda_other[t]
          
          # IMPORTANT: Now use X_proj here rather than X as no data is available
          X_proj[t] ~ dpois(lambda_total[t])
          X_delta[t] ~ dbinom(lambda_delta[t]/lambda_total[t], X_proj[t])
          X_other[t] = X_proj[t] - X_delta[t]
      }
    # which quantities to write out?
    #monitor# X_delta, X_other, X_proj, prec
    }"

# run sampling:
results <- run.jags(model, n.chains = 5, sample = 50000)


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
