library(readr)
library(tidyverse)
library(ARDL)
library(xts)
library(car)
library(xtable)
library(dynlm)
library(Metrics)

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
# data <- merge(data, ttfclose, by = 1)
data =  merge(data, eex[c(1, 3)], by = 1) 
data = merge(data, weather_th, by = 1)
data = merge(data, weather_16, by = 1)
data <- merge(data, weather_wind, by = 1)
data <- merge(data, weather_wind10, by = 1)
data <- merge(data, weather_evap, by = 1)
data <- merge(data, weather_humid, by = 1)
data <- merge(data, weather_cloud, by = 1)
colnames(data) <- c("date", "hdd", "demand", "eex", "hddth", "hdd16", "hddwind", "wind", "evap", "humid", "cloud")

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
demdata_2 <- data %>% 
  group_by(week) %>% 
  mutate(demand = demand - mean(demand), hdd = hdd - mean(hdd), hddth = hddth - mean(hddth), hdd16 = hdd16 - mean(hdd16), hddwind = hddwind - mean(hddwind), wind = wind - mean(wind), evap = evap - mean(evap), humid = humid - mean(humid), cloud = cloud - mean(cloud))

# demdata_2$ttf_logret <- log(demdata_2$ttf) - log(dplyr::lag(demdata_2$ttf))
demdata_2$eex_lag <- dplyr::lag(demdata_2$eex)
demdata_2$eex_lagsq <- demdata_2$eex_lag * demdata_2$eex_lag
demdata_2$eex_laglog <- log(demdata_2$eex_lag)
demdata_2$eex_cap <- ifelse(demdata_2$eex > 60, 60, demdata_2$eex)
demdata_2$eex_peak <- ifelse(demdata_2$eex > 60, demdata_2$eex - 60, 0)
demdata_2$eex_cap_lag <- ifelse(demdata_2$eex_lag > 60, 60, demdata_2$eex_lag)
demdata_2$eex_peak_lag <- ifelse(demdata_2$eex_lag > 60, demdata_2$eex_lag - 60, 0)
demdata_2$eex_prewar <- ifelse(demdata_2$date < as.Date("2022-02-24"), demdata_2$eex_lag, 0)
demdata_2$eex_postwar <- ifelse(demdata_2$date >= as.Date("2022-02-24"), demdata_2$eex_lag, 0)
demdata_2 <- drop_na(demdata_2)


## Month dummies ##

# Add a new column for month
demdata_2 <- demdata_2 %>%
  mutate(monthoy = format(date, "%m"))

# Create month dummies using model.matrix()
month_dummies <- model.matrix(~factor(monthoy)-1, data = demdata_2)

# Add the dummies to the dataset
demdata_2 <- cbind(demdata_2, month_dummies)

# Summer dummy
demdata_2$summer <- demdata_2$`factor(monthoy)06` + demdata_2$`factor(monthoy)07` + demdata_2$`factor(monthoy)08`
demdata_2$winter <- demdata_2$`factor(monthoy)12` + demdata_2$`factor(monthoy)01` + demdata_2$`factor(monthoy)02`

demdata_2$humsum <- demdata_2$summer * demdata_2$humid
demdata_2$humwin <- demdata_2$winter * demdata_2$humid

demdata_2$hddwind <- demdata_2$hdd16 * demdata_2$hddwind


# ARDL model demand
model <- auto_ardl(demand ~ cloud + wind + hdd16 + eex_prewar + eex_postwar, data = demdata_2, max_order = 5, selection = "BIC")
summary(model$best_model)

plot(demdata_2$demand, type="l", xlab="time", ylab="Restlast [kWh]")


#Cross validation

train_size <- floor(nrow(demdata_2) * 0.8)

# Split the data into training and testing sets
demdata_2_train <- demdata_2[1:train_size, ]
demdata_2_test <- demdata_2[(train_size + 1):nrow(demdata_2), ]

# Define the size of the rolling window
window_size <- floor(nrow(demdata_2_test) * 0.1)

# Initialize an empty vector to store the test set scores
scores <- c()

# Iterate over the rolling test set
for (i in seq(1, nrow(demdata_2_test) - window_size, window_size)) {
  # Define the current training and testing sets
  demdata_2_train_current <- rbind(demdata_2_train, demdata_2_test[1:i, ])
  demdata_2_test_current <- demdata_2_test[(i + 1):(i + window_size), ]
  
  # Fit the ARDL model on the current training set
  model <- auto_ardl(demand ~ cloud + wind + hdd16 + eex_prewar + eex_postwar, data = demdata_2_train_current, max_order = 5, selection = "BIC")
  model <- dynlm(model$best_model$full_formula, data = demdata_2_train_current)
  
  # Evaluate the model on the current test set
  
  preds <- predict(model, newdata = demdata_2_test_current)
  actual <- demdata_2_test_current$demand
  
  score <- rmse(actual, preds)

  scores <- c(scores, score)
  # Print the current test set score
  print(paste("Window:", i, "Score:", score))
}

# Compute the mean test set score
mean_score <- mean(scores)

# Print the mean test set score
print(paste("Mean Score:", mean_score))


fit=lm(demand~hdd+lag(hdd,1)+lag(hdd,2)+lag(hdd,3)+lag(hdd,4)+lag(hdd,5)+cloud+lag(cloud,1)+lag(cloud,2)+lag(cloud,3)+lag(clo))

vif(model_2$best_model)
model_2 <- auto_ardl(demand ~ cloud + wind + hdd16 + eex_lag, data = demdata_2, max_order = 5, selection = "BIC")
plot(test$demand, type="l", xlab="time", ylab="Restlast [kWh]")
lines(model_2$best_model$fitted.values[(1204-156):1204], col="blue")
lines(model$best_model$fitted.values[(1204-156):1204], col="red")

model_2$best_model$fitted.values
nrow(test)
ms=multipliers(model$best_model,"sr")
ml = multipliers(model$best_model)

xtable(cbind(ms,ml), type="latex")
cat("short", ms$Estimate[5] * (avg_price / avg_demand))
cat("long", ml$Estimate[5] * (avg_price / avg_demand))

avg_price_pre <- mean(data[data$date < as.Date("2022-02-24"),]$eex)
avg_demand_pre <- mean(data[data$date < as.Date("2022-02-24"),]$demand)

avg_price_post <- mean(data[data$date >= as.Date("2022-02-24"),]$eex)
avg_demand_post <- mean(data[data$date >= as.Date("2022-02-24"),]$demand)


cat("short", ms$Estimate[5] * (avg_price_pre / avg_demand_pre))
cat("long", ml$Estimate[5] * (avg_price_pre / avg_demand_pre))

cat("short", ms$Estimate[6] * (avg_price_post / avg_demand_post))
cat("long", ml$Estimate[6] * (avg_price_post / avg_demand_post))
summary(model)
recm(model, case="uc")

test = demdata_2[demdata_2$date >= as.Date("2022-02-24"),]
train = demdata_2[demdata_2$date < as.Date("2022-02-24"),]

model <- auto_ardl(demand ~ cloud + wind + hdd16, data = train, max_order = 5, selection = "BIC")
summary(model$best_model)

fit = lm(demand~lag(demand,1)+cloud+lag(cloud,1)+lag(cloud,2)+lag(cloud,3)+lag(cloud,4)+lag(cloud,5)+wind+lag(wind,1)+hdd16+lag(hdd16,1), data=train)

pred = predict(fit, newdata=test)
df = data.frame(p=pred, a=test$demand, d = test$date)

ggplot(df, aes(x=d, y=a))+
  geom_line(size=1)+
  geom_line(aes(y=p, color="red"), size=1)+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(title ="Actual vs predicted consumption", x= "Time", y="Restlast [kWh]")+
  theme_minimal()+
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 16))
