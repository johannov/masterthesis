library(readr)
library(tidyverse)
library(ARDL)
library(xts)

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

# Merging dataset
data <- merge(weather, demand, by = 1)
data <- merge(data, ttfclose, by = 1)
colnames(data) <- c("date", "hdd", "demand", "ttf")

# Weekly dataset
library(ISOweek)

# Convert date column to date format
data$date <- as.Date(data$date)

# Create new columns for iso-week and year
data <- mutate(data, week = ISOweek(date), year = format(date, format="%Y"))

# Group by iso-year and year, and calculate the sum of hdd, demand, and the mean of ttf
data_weekly <- group_by(data, week) %>% summarize(hdd = sum(hdd),
                                                  demand = sum(demand),
                                                  ttf = mean(ttf))



# Logarithm
logdata <- data_weekly
logdata[c("hdd", "demand", "ttf")] <- log(logdata[c("hdd", "demand", "ttf")])
logdata$ttf_lag <- dplyr::lag(logdata$ttf)
logdata$ttf_diff <- c(NA, diff(logdata$ttf))
logdata <- drop_na(logdata)

# ARDL model demand
model <- auto_ardl(demand ~ hdd + ttf, data = logdata, max_order = 8, selection = "BIC")$best_model
summary(model)
multipliers(model)
multipliers(model, type = "sr")


# 
# # Price check
# verivox_prices <- read_csv("./data/verivox_prices.csv", 
#                            col_types = cols(Wert = col_date(format = "%d.%m.%Y")))
# 
# verivox_prices$month <- format(verivox_prices$Wert, format="%Y-%m")
# 
# data$month <- format(data$date, format="%Y-%m")
# 
# ttf_monthly <- aggregate(data$ttf ~ data$month, data=data, mean)
# colnames(ttf_monthly) <- c("month", "ttf")
# 
# pricedf <- merge(verivox_prices, ttf_monthly, by = "month")
# 
# n <- 2
# paste("*** Correlation lag ", n-1 + 0.5, " ***")
# t <- 0.5 * dplyr::lag(pricedf$ttf, n) + 0.5 * dplyr::lag(pricedf$ttf, n - 1)
# v <- pricedf$Zahl
# plot(t, v)
# cor <- cor.test(t, v, method="pearson")
# cor$estimate
# 
# 
# 
# # data <- merge(verivox_prices, data, all.y = TRUE, by = 1)
# 
# # Plot MA and verivox
# ggplot(data, aes(x=Wert)) +
#   geom_point(aes(y=Zahl*12, color="Zahl"), na.rm=TRUE) +
#   geom_line(aes(y=ttfmean, color="ttfmean"), na.rm=TRUE) +
#   labs(x="Date", y="Value", color="Legend") +
#   scale_color_manual(values=c("blue", "red")) +
#   theme(legend.position="top")
# 
# # Rolling mean
# data$ttfmean <- zoo::rollmean(data$ttf, k = 50, fill = NA)
# dataw <- drop_na(data)
# dataw$dummy <- ifelse(dataw$Wert >= as.Date("2021-11-01"), 1, 0)
# 
# dataws <- subset(dataw, dummy == 1)
# # Model
# m1 <- lm(Zahl ~ ttfmean, data = dataws)
# summary(m1)
# 
# # Predict values for the existing data
# 
# 
# 
# # Plot actual and fitted
# ggplot(dataws, aes(x = Wert)) +
#   geom_point(aes(y = Zahl, color = "Actual"), na.rm = TRUE) +
#   geom_line(aes(y = fitted, color = "Fitted"), na.rm = TRUE) +
#   labs(x = "Date", y = "Zahl", color = "Legend") +
#   scale_color_manual(values = c("blue", "red")) +
#   theme(legend.position = "top")