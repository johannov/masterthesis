library(tidyverse)
library(ISOweek)
library(ARDL)
library(car)
if (exists("data")) {rm(data)}

# Load natural gas consumption data
demand <- read_csv("./data/230222_Restlast.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))
demand <- subset(demand, date != as.Date("2019-01-01")) # Outlier

# Load weather data
weather <- read_csv("./data/weather_geoweighted.csv", col_names = TRUE, cols("date" = col_date(format = "%Y-%m-%d")))

# Create month dummies for weather data
weather$month <- as.factor(weather$month)

# Create week dummies for weather data
weather$week <- as.factor(weather$week)

# Deseason weather data
week_lm <- lm(hdd16 ~ week, data = weather)
summary(week_lm)

# Save residuals with dates from weather in data frame
week_data <- data.frame(date = weather$date, hdd16 = week_lm$residuals, week = weather$week)
head(week_data)

# Regression of residuals on week dummies
week_lm <- lm(month_data$hdd16 ~ month_data$week)
summary(week_lm)
