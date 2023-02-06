library(readr)
library(tidyverse)
library(tseries)
library(lmtest)
library(fUnitRoots)
library(zoo)


# Load natural gas consumption data
demand <- read_csv("./data/gastag.csv", 
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


# Load prices data
verivox_prices <- read_csv("./data/verivox_prices.csv", 
                           col_types = cols(Wert = col_date(format = "%d.%m.%Y")))

# Plot prices data
ggplot(verivox_prices, aes(Wert, Zahl)) +
  geom_line() +
  ggtitle("Prices")

# Summary statistics of prices data
summary(verivox_prices$Zahl)

# Unit root test on prices data
adf.test(verivox_prices$Zahl)

# Breusch-Pagan test on price data
model_price <- lm(Zahl ~ as.numeric(as.Date(Wert)), data = verivox_prices_2018)
bptest(model_price)

# Load heating degree days / weather  data
weather <- read_csv("./data/IEA_CMCC_HDD18dailyworldbypopalldays.csv", col_names = TRUE, skip = 9, cols("Date" = col_date(format = "%Y-%m-%d")))
weather <-  subset(weather, Territory == "Germany" & Date >= as.Date("2018-01-01"))

# Plot heating degree days data
ggplot(weather, aes(Date, HDD18)) +
  geom_line() +
  ggtitle("Heating Degree Days")

# Summary statistics of heating degree days data
summary(weather$HDD18)

# Unit root test on heating degree days data
adf.test(weather$HDD18)

# Breusch-Pagan test on weather data
model_weather <- lm(HDD18 ~ as.numeric(as.Date(Date)), data = weather)
bptest(model_weather)


# Load income data
income <- read_csv("./data/income.csv")
income_wages <- income[c(1,2,7)]
income_wages <- income_wages %>%
  mutate(year_quarter = paste0(Year, " Q", quarter))
income_wages$year_quarter <- as.yearqtr(income_wages$year_quarter, format = "%Y Q%q")
income_wages$date <- as.Date(income_wages$year_quarter)


# Plot wages data
ggplot(data = income_wages, aes(x = year_quarter, y = `Compensation of employees`)) +
  geom_line() +
  xlab("Year Quarter") +
  ylab("Compensation of employees") +
  ggtitle("Compensation of employees over time")

# Summary statistics of wages data
summary(income_wages$`Compensation of employees`)

# Unit root test on heating degree days data
adf.test(income_wages$`Compensation of employees`)

# Breusch-Pagan test on weather data
model_wage <- lm(`Compensation of employees` ~ as.numeric(as.Date(date)), data = income_wages)
bptest(model_wage)
