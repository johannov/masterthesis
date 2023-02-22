library(readr)
library(tidyverse)
library(ARDL)
library(xts)
library(car)

# Load heating degree days / weather  data
weather <- read_csv("./data/IEAhdd18_deu.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))[c(4,5)]
weather_th <- read_csv("./data/hddth18.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))
weather_16 <- read_csv("./data/hdd16.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))
weather_wind <- read_csv("./data/hddwind.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))
weather_wind10 <- read_csv("./data/wind.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))
weather_evap <- read_csv("./data/evap.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))
weather_humid <- read_csv("./data/humid.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))
weather_cloud <- read_csv("./data/cloud.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))

# Adding infinitesimal to heating degree days to prevent zero values
# weather$HDD18 <- weather$HDD18 + 0.00000000000001
# weather_th$HDDThold18 <- weather_th$HDDThold18 + 0.00000000000001
# weather$HDD16 <- weather$HDD16 + 0.00000000000001

# Load natural gas consumption data
demand <- read_csv("./data/gastag.csv", 
                   col_types = cols(Gastag = col_date(format = "%d.%m.%Y"), 
                                    ...3 = col_skip(), ...4 = col_skip()))

# Load TTF prices
ttfclose <- read_csv("data/ttfclose.csv", 
                     col_types = cols(Timestamp = col_date(format = "%d/%m/%Y")))

# Load EEX prices
eex <- read_csv("data/eex.csv", 
                     col_types = cols(Datum = col_date(format = "%d.%m.%Y")))

# Merging dataset
data <- merge(weather, demand, by = 1)
# data <- merge(data, ttfclose, by = 1)
data <- merge(data, eex[c(1, 3)], by = 1)
data <- merge(data, weather_th, by = 1)
data <- merge(data, weather_16, by = 1)
data <- merge(data, weather_wind, by = 1)
data <- merge(data, weather_wind10, by = 1)
data <- merge(data, weather_evap, by = 1)
data <- merge(data, weather_humid, by = 1)
data <- merge(data, weather_cloud, by = 1)
colnames(data) <- c("date", "hdd", "demand", "eex", "hddth", "hdd16", "hddwind", "wind", "evap", "humid", "cloud")

# Convert demand to MWh
data$demand <- data$demand * 0.001

# Weekly dataset
library(ISOweek)

# Convert date column to date format
data$date <- as.Date(data$date)

# Create new columns for iso-week and year
data <- mutate(data, yearweek = ISOweek(date), year = format(date, format="%Y"))
data$month <- format(data$date, format = "%Y-%m")
data$week <- substr(as.character(data$yearweek), 7, 8)

# Group by iso-year and year, and calculate the sum of hdd, demand, and the mean of ttf
data_weekly <- group_by(data, yearweek) %>% summarize(hdd = sum(hdd),
                                                      demand = sum(demand),
                                                     # ttf = mean(ttf)
                                                     )

data_monthly <- group_by(data, yearweek) %>% summarize(hdd = sum(hdd),
                                                       demand = sum(demand),
                                                       #ttf = mean(ttf)
                                                       )
# Demeaned, seasonal adjusted
demdata <- data %>% 
  group_by(week) %>% 
  mutate(demand = demand - mean(demand), hdd = hdd - mean(hdd), hddth = hddth - mean(hddth), hdd16 = hdd16 - mean(hdd16), hddwind = hddwind - mean(hddwind), wind = wind - mean(wind), evap = evap - mean(evap), humid = humid - mean(humid), cloud = cloud - mean(cloud))

# demdata$ttf_logret <- log(demdata$ttf) - log(dplyr::lag(demdata$ttf))
demdata$eex_lag <- dplyr::lag(demdata$eex)
demdata$eex_lagsq <- demdata$eex_lag * demdata$eex_lag
demdata$eex_laglog <- log(demdata$eex_lag)
demdata$eex_cap <- ifelse(demdata$eex > 60, 60, demdata$eex)
demdata$eex_peak <- ifelse(demdata$eex > 60, demdata$eex - 60, 0)
demdata$eex_cap_lag <- ifelse(demdata$eex_lag > 60, 60, demdata$eex_lag)
demdata$eex_peak_lag <- ifelse(demdata$eex_lag > 60, demdata$eex_lag - 60, 0)
demdata$eex_prewar <- ifelse(demdata$date < as.Date("2022-02-24"), demdata$eex_lag, 0)
demdata$eex_postwar <- ifelse(demdata$date >= as.Date("2022-02-24"), demdata$eex_lag, 0)
demdata <- drop_na(demdata)


## Month dummies ##

# Add a new column for month
demdata <- demdata %>%
  mutate(monthoy = format(date, "%m"))

# Create month dummies using model.matrix()
month_dummies <- model.matrix(~factor(monthoy)-1, data = demdata)

# Add the dummies to the dataset
demdata <- cbind(demdata, month_dummies)

# Summer dummy
demdata$summer <- demdata$`factor(monthoy)06` + demdata$`factor(monthoy)07` + demdata$`factor(monthoy)08`
demdata$winter <- demdata$`factor(monthoy)12` + demdata$`factor(monthoy)01` + demdata$`factor(monthoy)02`

demdata$humsum <- demdata$summer * demdata$humid
demdata$humwin <- demdata$winter * demdata$humid

demdata$hddwind <- demdata$hdd16 * demdata$hddwind


# ARDL model demand
model <- auto_ardl(demand ~ cloud + wind + hdd16 + eex_lag, data = demdata, max_order = 5, selection = "BIC")$best_model
summary(model)
multipliers(model)
multipliers(model, type = "sr")

# Simple linear regression
lm <- lm(demand ~ eex_postwar + wind + hdd16 + cloud + dplyr::lag(hdd16), data = demdata)
# Checking linear relationship
crPlot(lm, variable = "eex_postwar")
summary(lm)

# Elasticity estimates
avg_price <- mean(subset(data, date > as.Date("2022-02-24"))$eex)
avg_demand <- mean(subset(data, date > as.Date("2022-02-24"))$demand) # data or dem dama?

# elasticity <- multipliers(model)$estimate[3] * (avg_price / avg_demand)
# "Long run elasticity:" 
# elasticity

# Plot fit
# if (!exists("plot_demand")) {
#   source("plotdemand.R")
# }

