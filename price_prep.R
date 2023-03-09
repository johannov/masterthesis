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
df <- df[c(1:4)]
# Join weekday prices and weekend prices


# Replace missing values in day_ahead with weekend_ahead value from Monday after
df$combined <- df$day_ahead

for (i in 1:length(df$combined)) {
  
  if (df$weekday[i] == "Sat") {df$combined[i] <- df$weekend_ahead[i+2]}
  if (df$weekday[i] == "Sun") {df$combined[i] <- df$weekend_ahead[i+1]}
}

# Write to file
write.csv(df, "bloomberg_prices_combined.csv")


# Save and remove temp files
prices <- df[c("date", "combined")]
colnames(prices)[2] <- "last_close"
rm(df)
rm(date)
