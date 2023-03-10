source("data_prep.r")
library(dplyr)
library(ISOweek)
library(ARDL)

eex <- read_csv("data/eex.csv", col_types = cols(Datum = col_date(format = "%d.%m.%Y"))) %>%
  rename(date = Datum, frontjahr = `Frontjahr, Euro/MWh`, day_ahead = `Day-Ahead (1 MW), Euro/MWh`)

daily_freq = inner_join(weather,demand, by=c("Date"="date")) %>% 
  inner_join(eex, by=c("Date"="date")) %>% 
  #rename variables 
  rename(date = Date, hdd = HDD18) %>% 
  #select subset
  select(c("date","demand","day_ahead","hdd")) %>% 
  #log transforms
  mutate(log_demand = log(demand),
         log_day_ahead = log(day_ahead),
         log_hdd = log(hdd),
         eex_lag = lag(day_ahead))

#Demeaning per month
demdata <- daily_freq %>% 
  mutate(week = substr(as.character(ISOweek(date)),7,8)) %>% 
  group_by(week) %>% 
  mutate(demand = demand - mean(demand), hdd = hdd - mean(hdd), log_demand = log_demand - mean(log_demand)) 


# --- Unit root testing ---#
#Daily frequencies - EEX prices are I(1) at 5% level, demand and hdd is I(1) - is ARDL  appropriate with all I(1)?
#adf.test(daily_freq$day_ahead)
#adf.test(daily_freq$demand)
#adf.test(daily_freq$hdd)
#adf.test(diff(log(daily_freq$demand)))
#adf.test(diff(log(daily_freq$day_ahead)))
#adf.test(diff(log(daily_freq$hdd)))

#Demeaned - demand and HDD is I(0) while price still I(1) - ARDL appropriate
#adf.test(demdata$demand)
#adf.test(demdata$hdd)

#--- ARDL ---#
#Fitting ARDL model on daily data, not logged 
ardl_daily = auto_ardl(demand ~ hdd + eex_lag, data=demdata, max_order=8, selection="BIC")

#Summary
summary(ardl_daily$best_model)

#Multipliers
m_l = multipliers(ardl_daily$best_model)
m_s = multipliers(ardl_daily$best_model, "sr")
m_l
m_s

#Scaling elasticities
cat("long run elasticity:", m_l[3,2] * mean(daily_freq$eex_lag[2:nrow(daily_freq)])/mean(daily_freq$demand[2:nrow(daily_freq)]))
cat("short run elasticity:", m_s[3,2] * mean(daily_freq$eex_lag[2:nrow(daily_freq)])/mean(daily_freq$demand[2:nrow(daily_freq)]))

# --- Bounds test for cointegration --- #
bounds = bounds_f_test(ardl_daily$best_model, case="uc")
