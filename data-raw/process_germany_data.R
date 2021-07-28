# Dual-strain branching process model
# Johannes Bracher johannes.bracher@kit.edu
# Adapted by Sam Abbott
library(readxl)
library(data.table)

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# the script is sort of tailored to the data availability on Thursday 1 July
# (with one week for which incidence data are already available while
# sequencing data are not)

data_source <- "RKI"

# Truth data (aggregate confirmed cases):
if (data_source == "RKI") {
  cases <- read.csv("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Incident%20Cases_Germany.csv") # nolint
}
if (data_source == "JHU") {
  cases <- read.csv("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/JHU/truth_JHU-Incident%20Cases_Germany.csv") # nolint
}

# some formatting...
cases <- subset(cases, location == "GM")
cases$date <- as.Date(cases$date)

# generate 7-day moving averages
cases$inc7 <- NA
for (i in 7:nrow(cases)) cases$inc7[i] <- sum(cases$value[i - (0:6)])
# subset to Saturdays for comparability to Forecast Hub
cases_sat <- subset(cases, weekdays(cases$date) == "Saturday")
# add calendar weeks:
cases_sat$wk <- as.numeric(format(cases_sat$date, "%W"))
# restrict to most recent weeks
cases_sat <- subset(cases_sat, date >= as.Date("2021-03-20"))

# get data on variants from RKI:
download.file(
  "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile", # nolint
  destfile = "VOC_VOI_Tabelle.xlsx"
)
sampling <- readxl::read_excel("VOC_VOI_Tabelle.xlsx")
file.remove("VOC_VOI_Tabelle.xlsx")

# re-format calendar week:
sampling$KW <- as.numeric(gsub("KW", "", sampling$KW)) # nolint
# compute total number of tests performed and some formatting
sampling$`B.1.617.2_Anteil (%)` <- sampling$`B.1.617.2_Anteil (%)` / 100 # nolint
sampling$total <-
  round(sampling$B.1.617.2_Anzahl / sampling$`B.1.617.2_Anteil (%)`) # nolint
sampling <- sampling[
  ,
  c("KW", "total", "B.1.617.2_Anzahl", "B.1.617.2_Anteil (%)")
]
colnames(sampling) <- c("wk", "seq_total", "seq_B.1.1617.2", "share_B.1.1617.2")
sampling <- setDT(sampling)[share_B.1.1617.2 >= 1e-3 & seq_B.1.1617.2 > 5]

# merge into case data set
germany_obs <- merge(cases_sat, sampling, by = "wk", all.x = TRUE)
germany_obs <- setDT(germany_obs)
setnames(
  germany_obs,
  old = c("inc7", "seq_B.1.1617.2", "share_B.1.1617.2"),
  new = c("cases", "seq_delta", "share_delta")
)
set(germany_obs, j = c("value", "wk"), value = NULL)

# Add availability indicators
germany_obs[, `:=`(cases_available = date, seq_available = date)]
# add that some sequences will never be available
germany_obs[date <= "2021-04-10", seq_available := NA]
# assume sequence availability lag based on final NA number
avail_lag <- nrow(germany_obs[is.na(seq_total) & !is.na(seq_available)])
germany_obs[, seq_available := seq_available + 7 * avail_lag]

# save all observations
usethis::use_data(germany_obs, overwrite = TRUE)
