library(readr)
library(tidyverse)
library(ISOweek)
library(ARDL)

### Natural gas consumption ###

gas_week <- read_delim("C:/Users/dja/Downloads/weeklygas.csv", ";")

gas_week <- pivot_longer(data = gas_week[1:4], cols = 2:4, names_to = "year", values_to = "demand")

colnames(gas_week)[1] <- "week"

gas_week$week <- as.integer(gas_week$week)
gas_week$year <- as.integer(gas_week$year)

gas_week$time <- sprintf("%d-W%02d", gas_week$year, gas_week$week)


### Heating degree days ###

weather <- read_csv("C:/Users/dja/Downloads/IEA_CMCC_HDD18dailyworldbypopalldays.csv", col_names = TRUE, skip = 9, cols("Date" = col_date(format = "%Y-%m-%d")))
weather <-  subset(weather, Territory == "Germany" & Date >= as.Date("2018-01-01"))


weather_week <- weather %>% 
  mutate(week = ISOweek(Date), year = format(Date, format="%Y")) %>% 
  group_by(year, week) %>% 
  summarize(sum_HDD18 = sum(HDD18))

### Merging ###

data <- merge(gas_week, weather_week, by.x = "time", by.y = "week")

data$hdd2 <- data$sum_HDD18 * data$sum_HDD18

data$hddlag <- dplyr::lag(data$sum_HDD18)

data$hddlag2 <- dplyr::lag(data$hddlag)


### Savings campaign ###

data$date <- ISOweek2date(paste(data$time, '1', sep = '-'))
data$dummy <- 0
data$dummy[data$date >= as.Date("2022-06-10")] <- 1



### Model ###

model <- lm(demand ~ sum_HDD18 + hddlag + hddlag2 + hdd2 + dummy, data = data)
summary(model)



#### Plot ####

predictions <- predict(model, data)
data$predictions <- predictions

ggplot(data, aes(x=date, y=demand)) + 
  geom_line(color="blue", size=1) +
  geom_line(aes(x=date, y=predictions), color="red", size=1) +
  ggtitle("Comparison of Real Values and Predicted Values") +
  xlab("Date") +
  ylab("Gas Consumption")

