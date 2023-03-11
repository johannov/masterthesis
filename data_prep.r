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
library(xlsx)

#--- LOAD DATA ---#
#EEX proces

bloomberg_eex <- read_csv("data/bloomberg_prices_combined.csv", 
                          col_types = cols(date = col_date(format = "%Y-%m-%d"))) %>% 
  rename(price = combined)

verivox_prices <- read_csv("data/verivox_prices.csv", 
                           col_types = cols(Wert = col_date(format = "%d.%m.%Y"))) %>% 
  mutate(month = format(Wert, "%m"), year = format(Wert, "%Y")) 

#Demand
demand <- read_csv("data/residual_load_1103.csv", 
                                      col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% 
  rename(demand = residual_load) %>%
  #Demand to MwH
  mutate(demand = demand /1000)

#Weather 

dwd_weather <- read_csv("data/dwd_pop_weighted_weather_mean.csv", 
                       col_types = cols(date = col_date(format = "%Y-%m-%d"))) %>% 
  rename(hdd16 = hdd16_sum, wind = wind_speed_sum, cloud = cloud_cover_total_sum)


head(dwd_weather)
#Indices
production <- read_csv("data/prod.csv", col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) 

production_industry <- read_csv("data/prod_industry.csv", col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) 

earnings <- read_csv("data/earnings.csv", col_types = cols(date = col_date(format = "%d.%m.%Y")))%>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) 

unemployment <- read_csv("data/unemp.csv", col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
  rename(nadj_unemp = "Non-adjusted value", adj_unemp= "Calendar- and seasonally adjusted (BV4.1)")

trade <- read_csv("data/trade.csv", col_types = cols(date = col_date(format = "%d.%m.%Y")))%>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y"))

#Covid indicators
covid <- read_csv("data/covid.csv", col_types = cols(date = col_date(format = "%Y-%m-%d"))) %>%
  rename(government_response = "GovernmentResponseIndex_Average", economic_support = "EconomicSupportIndex",
         stay_at_home="C6M_Stay at home requirements", school_closed = "C1M_School closing", work_closed = "C2M_Workplace closing",
         fiscal_measures = "E3_Fiscal measures")


#--- Imputing lower freq data ---#
dates_earn = 

#Spine approach:
unemp.dates = data.frame(date=seq(from=as.Date("01-01-2018", format="%d-%m-%Y"), to=as.Date("01-02-2023", format="%d-%m-%Y"), by="day"))
prod.dates = data.frame(date=seq(from=as.Date("01-01-2018", format="%d-%m-%Y"), to=as.Date("01-01-2023", format="%d-%m-%Y"), by="day"))
earn.dates = data.frame(date=seq(from=as.Date("01-01-2018", format="%d-%m-%Y"), to=as.Date("01-07-2022", format="%d-%m-%Y"), by="day"))

unemp.s = data.frame(ts(left_join(unemp.dates, unemployment, by="date")$nadj_unemp, start=c(2010,1), frequency=365) %>% na.spline())
prod.s = data.frame(ts(left_join(prod.dates, production_industry, by="date")$prod_index_adj, start=c(2010,1), frequency=365) %>% na.spline())
trade.s = data.frame(ts(left_join(prod.dates, trade, by="date")$adj_trade, start=c(2018,1), frequency=365) %>% na.spline())
earn.s = data.frame(ts(left_join(earn.dates, earnings, by="date")$earnings_adj, start=c(2010,1), frequency = 365)%>% na.spline())

splined = left_join(data.frame(date = unemp.dates, unemp = unemp.s), data.frame(date = prod.dates, prod = prod.s), by="date") %>% 
  left_join(data.frame(date = prod.dates, trade = trade.s), by="date") %>% 
  left_join(data.frame(date = earn.dates, earn = earn.s), by="date")
names(splined) = c("date", "unemp", "prod_idx", "trade", "earnings")


#Linear approach:
unemp.l = data.frame(ts(left_join(unemp.dates, unemployment, by="date")$nadj_unemp, start=c(2010,1), frequency=365) %>% na.approx())
prod.l = data.frame(ts(left_join(prod.dates, production_industry, by="date")$prod_index_adj, start=c(2010,1), frequency=365) %>% na.approx())
trade.l = data.frame(ts(left_join(prod.dates, trade, by="date")$adj_trade, start=c(2018,1), frequency=365) %>% na.approx())
earn.l = data.frame(ts(left_join(earn.dates, earnings, by="date")$earnings_adj, start=c(2010,1), frequency = 365)%>% na.approx())

linear = left_join(data.frame(date = unemp.dates, unemp = unemp.l), data.frame(date = prod.dates, prod = prod.l), by="date") %>% 
  left_join(data.frame(date = prod.dates, trade = trade.l), by="date") %>% 
  left_join(data.frame(date = earn.dates, earn = earn.l), by="date")
names(linear) = c("date", "unemp", "prod_idx", "trade", "earnings")


# --- Joining --- #

#First: Joining demand and weather
daily_freq <- inner_join(demand, dwd_weather, by="date") %>% 
  
  #Join with prices
  inner_join(bloomberg_eex, by=c("date")) %>% 
 
  #Join with covid variables, and replace NAs, as the period before covid will give NA
  left_join(covid, by="date") %>% 
  mutate(government_response = replace_na(government_response, 0),
         economic_support = replace_na(economic_support, 0),
         stay_at_home = replace_na(stay_at_home, 0),
         work_closed = replace_na(work_closed, 0),
         school_closed = replace_na(school_closed, 0)) %>% 
  
  
  #Macro imputed indicators
  left_join(linear, by="date") %>% 
  
  #To be able to deseason unemployment without creating NAs, I define the time period here.
  subset(date < as.Date("02-02-2023", format="%d-%m-%Y")) %>% 
  
  #rename variables for convenience
  dplyr::select(date, hdd16, wind, demand, price, 
                government_response, economic_support, stay_at_home, 
                school_closed, work_closed, unemp, prod_idx, cloud)

#Demeaning per week
demdata <- daily_freq %>% 
  mutate(week = substr(as.character(ISOweek(date)),7,8)) %>% 
  group_by(week) %>% 
  mutate(demand = demand - mean(demand), hdd16 = hdd16 - mean(hdd16), unemp=unemp-mean(unemp)) 


#More dummy variables
demdata$eex_prewar= ifelse(demdata$date < as.Date("2022-02-24"), demdata$price, 0)
demdata$war_d = ifelse(demdata$date >= as.Date("2022-02-24"), 1, 0)
demdata$eex_postwar = ifelse(demdata$date >= as.Date("2022-02-24"), demdata$price, 0)
demdata$covid = ifelse(demdata$date >= as.Date("2020-02-24"), demdata$price, 0)
demdata$hddwar = demdata$war_d*demdata$hdd16
demdata$covid = ifelse(demdata$work_closed>0 ,1, 0)




