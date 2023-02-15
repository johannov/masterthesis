
library(tsDyn)
library(vars)
library(readr)
library(tidyverse)
library(urca)
library(forecast)

# Load natural gas consumption data
demand <- read_csv("./data/gastag.csv", 
                   col_types = cols(Gastag = col_date(format = "%d.%m.%Y"), 
                                    ...3 = col_skip(), ...4 = col_skip()))

names(demand) <- c("date", "demand")

# Load natural gas fixed contract price index
verivox_prices <- read_csv("./data/verivox_prices.csv", 
                           col_types = cols(Wert = col_date(format = "%d.%m.%Y")))

# Filter based on dates (perhaps not necessary if we will later do a join)

verivox_prices
names(verivox_prices)=c("date", "price")

# Aggregate demand by month
demand_agg = demand %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(year, month) %>%
  summarise(demand = sum(demand), date = first(date))

#Weather
weather <- read_csv("./data/IEAhdd18_deu.csv", col_names = TRUE, cols("Date" = col_date(format = "%Y-%m-%d"))) %>% 
  subset(Territory == "Germany" & Date >= as.Date("2018-01-01"))

# Aggregate HDD by month
weather_agg <-  weather %>% mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(year, month) %>%
  summarise(hdd = sum(HDD18), date = first(Date))

#TTF
ttf <- ttfclose <- read_csv("data/ttfclose.csv", 
                            col_types = cols(Timestamp = col_date(format = "%d.%m.%Y")))

names(ttf) = c("date","close")


ttf_agg <- ttf %>% mutate(month = format(date, "%m"),  year = format(date, "%Y")) %>%
  group_by(year, month) %>%
  summarise(ttf = mean(close), date = last(date))

# --- Aggregated df with demand, prices and hdd. 

df <- inner_join(weather_agg,demand_agg,by="date") %>% 
  select(-c(1,2)) %>%
  inner_join(verivox_prices, by="date")

# --- Including ttf --- #
df_ttf <- inner_join(ttf_agg, df, by=c("year" = "year.y", "month" = "month.y"))

df["log_demand"] = log(df$demand)
df["log_price"] = log(df$price)
df["log_hdd"] = log(df$hdd)


#---daily---#
daily_freq = inner_join(weather,demand, by=c("Date"="date")) %>% 
  inner_join(ttf, by=c("Date"="date")) %>% 
  rename(date = Date, hdd = HDD18, price = close) %>% 
  select(c("date","demand","price","hdd"))

daily_freq$log_demand = log(daily_freq$demand)
daily_freq$log_price = log(daily_freq$price)
daily_freq$log_hdd = ifelse(daily_freq$hdd>0,log(daily_freq$hdd),0)
