library(readr)
library(tidyverse)
library(tseries)
library(lmtest)
library(fUnitRoots)

# Load prices data
verivox_prices <- read_csv("C:/Users/dja/Downloads/verivox_prices.csv", 
                           col_types = cols(Wert = col_date(format = "%d.%m.%Y")))

# Plot prices data
ggplot(verivox_prices, aes(Wert, Zahl)) +
  geom_line() +
  ggtitle("Prices")

# Summary statistics of prices data
summary(verivox_prices$Zahl)

# Unit root test on prices data
adf.test(verivox_prices$Zahl)

# Load natural gas consumption data
demand <- read_csv("C:/Users/dja/Downloads/gastag.csv", 
                   col_types = cols(Gastag = col_date(format = "%d.%m.%Y"), 
                                    ...3 = col_skip(), ...4 = col_skip()))

# Plot natural gas consumption data
ggplot(demand, aes(Gastag, `Restlast[kWh]*`)) +
  geom_line() +
  ggtitle("Natural Gas Consumption")

# Summary statistics of natural gas consumption data
summary(demand$`Restlast[kWh]*`)

# Unit root test on natural gas consumption data
adf.test(demand$`Restlast[kWh]*`)

# Breusch-Pagan test on consumption data
model <- lm(`Restlast[kWh]*` ~ as.numeric(as.Date(Gastag)), data = demand)
bptest(model)

# Load heating degree days data
weather <- read_csv("C:/Users/dja/Downloads/IEA_CMCC_HDD18dailyworldbypopalldays.csv", col_names = TRUE, skip = 9, cols("Date" = col_date(format = "%Y-%m-%d")))
weather <-  subset(weather, Territory == "Germany" & Date >= as.Date("2018-01-01"))

# Plot heating degree days data
ggplot(weather, aes(Date, HDD18)) +
  geom_line() +
  ggtitle("Heating Degree Days")

# Summary statistics of heating degree days data
summary(weather$HDD18)

# Unit root test on heating degree days data
adf.test(weather$HDD18)
