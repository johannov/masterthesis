library(tidyverse)
if (exists("df")) rm(df)


prices <- read_csv("data/prices_last_day_close.csv")

# Create a complete date sequence
date <- seq(as.Date("2017-01-03"), as.Date("2023-03-02"), by = "days")
df <- as.data.frame(date)

# Find day of week
df$weekday <- weekdays(df$date, abbreviate = TRUE)

# Outer join with prices
df <- merge(df, prices, by = "date", all = TRUE)

# Join weekday prices and weekend prices