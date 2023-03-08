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
demand <- read_csv("./data/230222_Restlast.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))
demand <- subset(demand, date != as.Date("2019-01-01"))

# Load TTF prices
ttfclose <- read_csv("data/ttfclose.csv", 
                     col_types = cols(Timestamp = col_date(format = "%d/%m/%Y")))

# Load EEX prices
eex <- read_csv("data/eex.csv", 
                     col_types = cols(Datum = col_date(format = "%d.%m.%Y")))

# Merging dataset
data <- merge(weather, demand, all.y = TRUE, by = 1)
# data <- merge(data, ttfclose, by = 1)
data <- merge(data, eex[c(1, 3)], all.x = TRUE, by = 1)
data <- merge(data, weather_th, all.x = TRUE, by = 1)
data <- merge(data, weather_16, all.x = TRUE, by = 1)
data <- merge(data, weather_wind, all.x = TRUE, by = 1)
data <- merge(data, weather_wind10, all.x = TRUE, by = 1)
data <- merge(data, weather_evap, all.x = TRUE, by = 1)
data <- merge(data, weather_humid, all.x = TRUE, by = 1)
data <- merge(data, weather_cloud, all.x = TRUE, by = 1)
data <- merge(data, wetter_wa, all = TRUE, by = 1)
colnames(data)[1:11] <- c("date", "hdd", "demand", "eex", "hddth", "iea_hdd16", "hddwind", "wind", "evap", "humid", "cloud")

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
  mutate(demand = demand - mean(demand), 
         hdd = hdd - mean(hdd), 
         hddth = hddth - mean(hddth), 
         hdd16 = hdd16 - mean(hdd16),
         iea_hdd16 = iea_hdd16 - mean(iea_hdd16),
         hddwind = hddwind - mean(hddwind), 
         wind = wind - mean(wind), 
         evap = evap - mean(evap), 
         humid = humid - mean(humid), 
         cloud = cloud - mean(cloud), 
         wind_speed_sum = wind_speed_sum - mean(wind_speed_sum),
         temperature_air_mean_200_sum = temperature_air_mean_200_sum - mean(temperature_air_mean_200_sum),
         temperature_air_min_200_sum = temperature_air_min_200_sum - mean(temperature_air_min_200_sum),
         temperature_air_max_200_sum = temperature_air_max_200_sum - mean(temperature_air_max_200_sum),
         hdd16_sum = hdd16_sum - mean(hdd16_sum),
         hdd17_sum = hdd17_sum - mean(hdd17_sum),
         hdd15_sum = hdd15_sum - mean(hdd15_sum),
         celctemp = celctemp - mean(celctemp))

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
# demdata <- drop_na(demdata)
# 
# 
# ## Month dummies ##
# 
# # Add a new column for month
# demdata <- demdata %>%
#   mutate(monthoy = format(date, "%m"))
# 
# # Create month dummies using model.matrix()
# month_dummies <- model.matrix(~factor(monthoy)-1, data = demdata)
# 
# # Add the dummies to the dataset
# demdata <- cbind(demdata, month_dummies)
# 
# # Summer dummy
# demdata$summer <- demdata$`factor(monthoy)06` + demdata$`factor(monthoy)07` + demdata$`factor(monthoy)08`
# demdata$winter <- demdata$`factor(monthoy)12` + demdata$`factor(monthoy)01` + demdata$`factor(monthoy)02`
# 
# demdata$humsum <- demdata$summer * demdata$humid
# demdata$humwin <- demdata$winter * demdata$humid
# 
# demdata$hddwind <- demdata$hdd16 * demdata$hddwind
# 

# 
# ARDL model demand
model <- auto_ardl(demand ~ hdd16 + dplyr::lag(hdd16) + eex_lag + wind, data = comp_dwd, max_order = 5, selection = "BIC")$best_model
summary(model)
longm <- multipliers(model)
shortm <- multipliers(model, type = "sr")
longm
shortm



plot(data$iea_hdd16, data$demand)
# Simple linear regression
lm <- lm(demand ~ hdd16_sum + dplyr::lag(hdd16_sum), data =)

# Checking linear relationship
crPlot(lm, variable = "hdd16")
summary(lm)
plot(lm$residuals)

# 
# # Elasticity estimates
# avg_price <- mean(data$eex)
# avg_demand <- mean(data$demand) # data or dem dama?
# avg_cloud <- mean(data$cloud)
# avg_wind <- mean(data$wind)
# avg_hdd16 <- mean(data$hdd16)
# 
# "Cloud elasticity"
# longm$Estimate[2] * (avg_cloud / avg_demand)
# 
# "Wind elasticity"
# longm$Estimate[3] * (avg_wind / avg_demand)
# 
# "Temperature elasticity"
# longm$Estimate[4] * (avg_hdd16 / avg_demand)
# 
# "Price elasticity"
# longm$Estimate[5] * (avg_price / avg_demand)
# 
# 
# 
# "Cloud elasticity, short run"
# shortm$Estimate[2] * (avg_cloud / avg_demand)
# 
# "Wind elasticity, short run"
# shortm$Estimate[3] * (avg_wind / avg_demand)
# 
# "Temperature elasticity, short run"
# shortm$Estimate[4] * (avg_hdd16 / avg_demand)
# 
# "Price elasticity, short run"
# shortm$Estimate[5] * (avg_price / avg_demand)
# 
# # elasticity <- multipliers(model)$estimate[3] * (avg_price / avg_demand)
# # "Long run elasticity:" 
# # elasticity
# 
# # Plot fit
# # if (!exists("plot_demand")) {
# #   source("plotdemand.R")
# # }

