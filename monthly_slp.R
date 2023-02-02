library(readr)
library(ARDL)

### Prices ###
verivox_prices <- read_csv("C:/Users/dja/Downloads/verivox_prices.csv", 
                           col_types = cols(Wert = col_date(format = "%d.%m.%Y")))

verivox_prices$month <- format(verivox_prices$Wert, format="%Y-%m")

### Heating degree days ###
weather <- read_csv("C:/Users/dja/Downloads/IEA_CMCC_HDD18dailyworldbypopalldays.csv", col_names = TRUE, skip = 9, cols("Date" = col_date(format = "%Y-%m-%d")))
weather <-  subset(weather, Territory == "Germany" & Date >= as.Date("2018-01-01"))
weather$month <- format(weather$Date, format = "%Y-%m")

weather_month <- aggregate(weather$HDD18, by = list(month = weather$month), FUN = sum)


### Natural gas consumption ###
demand <- read_csv("C:/Users/dja/Downloads/gastag.csv", 
                   col_types = cols(Gastag = col_date(format = "%d.%m.%Y"), 
                                    ...3 = col_skip(), ...4 = col_skip()))
demand$month <- format(demand$Gastag, format = "%Y-%m")

demand_month <- aggregate(demand$`Restlast[kWh]*`, by = list(month = demand$month), FUN = sum)

### Merging ###
data <- merge(demand_month, weather_month, by = "month")
colnames(data) <- c("month", "demand", "hdd")

data <- merge(data, verivox_prices[-1], by = "month")
colnames(data)[4] <- "price"

logdata <- data
logdata[-1] <- log(logdata[-1])

model <- auto_ardl(demand ~ hdd + price, data = logdata, max_order = 10)

summary(model$best_model)


### Plot ###
plot(logdata$demand)
lines(9:60, model$best_model$fitted.values)
