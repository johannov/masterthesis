library(car)
library(readr)
library(tidyverse)
library(ARDL)
library(xts)
library(car)
library(xtable)
library(ISOweek)
library(imputeTS)
library(zoo)
library(tseries)
library(dynlm)
library(olsrr)

#--- LOAD DATA ---#

#EEX proces
eex <- read_csv("data/eex.csv", col_types = cols(Datum = col_date(format = "%d.%m.%Y"))) %>%
  rename(date = Datum, frontjahr = `Frontjahr, Euro/MWh`, price_day_ahead = `Day-Ahead (1 MW), Euro/MWh`) 

#Demand
demand <- read_csv("data/gastag.csv", col_types = cols(Gastag = col_date(format = "%d.%m.%Y")))%>% 
  rename(date = Gastag, demand = `Restlast[kWh]*`)

#Weather 

weather_cloud <- read_csv("data/cloud.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))

#FROM IEA
weather <- read_csv("data/hdd16.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d"))) %>% 
  mutate(HDD16 = HDD16+ 0.00000001)

weather_wind10 <- read_csv("data/wind.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d")))

#FROM DAVID
weighted_weather <- read_csv("data/bl_weighted_weather.csv", 
                             col_types = cols(date = col_date(format = "%Y-%m-%d")))

#weighted_weather_pop <- read_csv("data/bl_weighted_weather (1).csv", 
#                             col_types = cols(date = col_date(format = "%Y-%m-%d")))

#Indices
production <- read_csv("data/prod.csv", col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) 

production_industry <- read_csv("data/prod_industry.csv", col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) 

earnings <- read_csv("data/earnings.csv", col_types = cols(date = col_date(format = "%d.%m.%Y")))%>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) 

employment <- read_csv("data/employment.csv", col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y"))

#Covid indicators
covid <- read_csv("data/covid.csv", col_types = cols(date = col_date(format = "%Y-%m-%d"))) %>%
  rename(government_response = "GovernmentResponseIndex_Average", economic_support = "EconomicSupportIndex", stay_at_home="C6M_Stay at home requirements", school_closed = "C1M_School closing", work_closed = "C2M_Workplace closing" )


#--- Imputing lower freq data ---#
#Employment and prod index until 01.01.23, earnings until 01.07.22
dates = data.frame(date=seq(from=as.Date("01-01-2018", format="%d-%m-%Y"), to=as.Date("01-01-2023", format="%d-%m-%Y"), by="day"))
dates_earn = data.frame(date=seq(from=as.Date("01-01-2018", format="%d-%m-%Y"), to=as.Date("01-07-2022", format="%d-%m-%Y"), by="day"))

#Spine approach:
emp.s = ts(left_join(dates, employment, by="date")$employment, start=c(2010,1), frequency=365) %>% na.spline()
prod.s = ts(left_join(dates, production_industry, by="date")$prod_index_adj, start=c(2010,1), frequency=365) %>% na.spline()
splined = data.frame(date = dates, emp = emp.s, prod_idx = prod.s)

#Linear approach:
emp.l = ts(left_join(dates, employment, by="date")$employment, start=c(2010,1), frequency=365) %>% na.approx()
prod.l = ts(left_join(dates, production_industry, by="date")$prod_index_adj, start=c(2010,1), frequency=365) %>% na.approx()
linear = data.frame(date = dates, emp = emp.l, prod_idx = prod.l)

earn = ts(left_join(dates_earn, earnings, by="date")$earnings_adj, start=c(2010,1), frequency = 365)%>% na.spline()
#Own df because of short time period
earning_df = data.frame(date = dates_earn, earnings = earn)





# --- Joining --- #

daily_freq <- inner_join(demand, weighted_weather, by=c("date")) %>% 
  inner_join(eex, by=c("date")) %>% 
  #By commenting in the line below you lose obs, but inner join to allow comparison
  inner_join(covid, by="date") %>% 
  #inner_join(earning_df, by=c("date")) %>% 
  inner_join(linear, by="date") %>% 
  
  #rename variables 
  rename(wind=wind_speed_sum) %>% 
  dplyr::select(date, hdd16, wind, demand, price_day_ahead, school_closed, 
                work_closed, government_response, economic_support, stay_at_home, emp, prod_idx) %>%  
  
  #log transforms
  mutate(demand = demand /1000,
         log_demand = log(demand),
         log_day_ahead = log(price_day_ahead),
         eex_lag = lag(price_day_ahead))

#Demeaning per week
demdata <- daily_freq %>% 
  mutate(week = substr(as.character(ISOweek(date)),7,8)) %>% 
  group_by(week) %>% 
  mutate(demand = demand - mean(demand), hdd16 = hdd16 - mean(hdd16), emp = emp-mean(emp), prod_idx = prod_idx-mean(prod_idx)) 


#More variables
demdata$eex_prewar= ifelse(demdata$date < as.Date("2022-02-24"), demdata$price_day_ahead, 0)
demdata$war_d = ifelse(demdata$date >= as.Date("2022-02-24"), 1, 0)
demdata$eex_postwar = ifelse(demdata$date >= as.Date("2022-02-24"), demdata$price_day_ahead, 0)
demdata$covid = ifelse(demdata$date >= as.Date("2020-02-24"), demdata$price_day_ahead, 0)





# --- ARDL --- #

#Base model
model <- auto_ardl(demand ~ hdd16 + wind + price_day_ahead, data = demdata, max_order = 5, selection = "BIC")

#Base model + earnings
full_mod <- auto_ardl(demand ~ hdd16 + wind + price_day_ahead + prod_idx + stay_at_home, data = demdata, max_order = 5, selection = "BIC")
summary(full_mod$best_model)

#Evaluation set up

summary(model$best_model)
summary(full_mod$best_model)

multipliers(full_mod$best_model)
multipliers(full_mod$best_model, "sr")


avg_price <- mean(daily_freq$price_day_ahead)
avg_demand <- mean(daily_freq$demand)
avg_hdd <- mean(daily_freq$hdd16)
avg_wind <- mean(daily_freq$wind)

#price
ms$Estimate[4]*avg_price/avg_demand
ml$Estimate[4]*avg_price/avg_demand

set.seed(123)


