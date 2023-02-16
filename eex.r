source("data_prep.r")
source("google_trends.r")
library(dplyr)
library(ISOweek)

eex <- read_csv("data/eex.csv", col_types = cols(Datum = col_date(format = "%d.%m.%Y"))) %>%
  rename(date = Datum, frontjahr = `Frontjahr, Euro/MWh`, day_ahead = `Day-Ahead (1 MW), Euro/MWh`)

daily_freq = inner_join(weather,demand, by=c("Date"="date")) %>% 
  inner_join(eex, by=c("Date"="date")) %>% 
  rename(date = Date, hdd = HDD18) %>% 
  select(c("date","demand","day_ahead","hdd"))

#How does it look?
plot(daily_freq$day_ahead)

#Comment out line under if not w google.
daily_freq$google = c(0,0,daily_freq_w_google$hits)

#Long transforming for tryouts
daily_freq$log_demand = log(daily_freq$demand)
daily_freq$log_day_ahead = log(daily_freq$day_ahead)
daily_freq$log_hdd = ifelse(daily_freq$hdd>0,log(daily_freq$hdd),0)

#Lag of day ahead price as "today"
daily_freq$today = lag(daily_freq$day_ahead,1)
daily_freq$log_loday = log(daily_freq$today)

#Demeaning per month
demdata <- daily_freq %>% 
  mutate(week = substr(as.character(ISOweek(date)),7,8)) %>% 
  group_by(week) %>% 
  mutate(demand = demand - mean(demand), hdd = hdd - mean(hdd), log_demand = log_demand - mean(log_demand))

#Finding - log_demand and hdd is nearly linear
plot(demdata$hdd, demdata$log_demand)

#EEX prices are I(1)
adf.test(daily_freq$day_ahead)
adf.test(diff(log(daily_freq$day_ahead)))

#Fitting ARDL model on daily data, not logged 
ardl_daily = auto_ardl(demand ~ hdd + today + google, data=daily_freq, max_order=8, selection="BIC")

summary(ardl_daily$best_model)
m_l = multipliers(ardl_daily$best_model)
m_s = multipliers(ardl_daily$best_model, "sr")

m_l[3,2] * mean(daily_freq$today[2:nrow(daily_freq)])/mean(daily_freq$demand[2:nrow(daily_freq)])
m_s[3,2] * mean(daily_freq$today[2:nrow(daily_freq)])/mean(daily_freq$demand[2:nrow(daily_freq)])

m_l
m_s
