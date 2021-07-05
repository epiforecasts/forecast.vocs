# Dual-strain branching process model
# Johannes Bracher johannes.bracher@kit.edu

library(readxl)
library(data.table)

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

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
download.file("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile", destfile="VOC_VOI_Tabelle.xlsx")
sampling <- readxl::read_excel("VOC_VOI_Tabelle.xlsx")


# re-format calendar week:
sampling$KW <- as.numeric(gsub("KW", "", sampling$KW))
# compute total number of tests performed and some formatting
sampling$`B.1.617.2_Anteil (%)` <- sampling$`B.1.617.2_Anteil (%)`/100
sampling$total <- round(sampling$B.1.617.2_Anzahl/sampling$`B.1.617.2_Anteil (%)`)
sampling <- sampling[, c("KW", "total", "B.1.617.2_Anzahl", "B.1.617.2_Anteil (%)")]
colnames(sampling) <- c("wk", "seq_total", "seq_B.1.1617.2", "share_B.1.1617.2")

# merge into case data set
cases_sat <- merge(cases_sat, sampling, by = "wk", all.x = TRUE)

fwrite(cases_sat, "data/cases.csv")