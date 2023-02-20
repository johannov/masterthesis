library(readr)
library(tidyverse)
library(ARDL)
library(xts)
library(car)

# Load heating degree days / weather  data
weather <- read_csv("./data/IEAhdd18_deu.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))[c(4,5)]
# Adding infinitesimal to heating degree days to prevent zero values
weather$HDD18 <- weather$HDD18 + 0.00000001

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
data <- merge(data, ttfclose, by = 1)
data <- merge(data, eex[c(1, 3)], by = 1)
colnames(data) <- c("date", "hdd", "demand", "ttf", "eex")

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
                                                      ttf = mean(ttf))

data_monthly <- group_by(data, yearweek) %>% summarize(hdd = sum(hdd),
                                                       demand = sum(demand),
                                                       ttf = mean(ttf))
# Demeaned, seasonal adjusted
demdata <- data %>% 
  group_by(week) %>% 
  mutate(demand = demand - mean(demand), hdd = hdd - mean(hdd))

demdata$ttf_logret <- log(demdata$ttf) - log(dplyr::lag(demdata$ttf))
demdata$eex_lag <- dplyr::lag(demdata$eex)

demdata <- drop_na(demdata)



# ARDL model demand
model <- auto_ardl(demand ~ hdd + eex_lag, data = demdata, max_order = 5, selection = "BIC")$best_model
summary(model)
multipliers(model)
multipliers(model, type = "sr")

# Simple linear regression
lm <- lm(demand ~ hdd + dplyr::lag(hdd) + eex, data = demdata)
# Checking linear relationship
crPlot(lm, variable = "eex")
summary(lm)

predicted_values <- predict(lm)

# Calculate the residuals
residuals <- demdata$demand - c(NA, predicted_values)

plot(demdata$date, residuals)

# Elasticity estimates
avg_price <- mean(demdata$eex_lag)
avg_demand <- mean(data$demand) # data or dem dama?

elasticity <- multipliers(model)$estimate[3] * (avg_price / avg_demand)
elasticity

# Plot fit
if (!exists("plot_demand")) {
  source("plotdemand.R")
}

plot_demand(model, demdata)
