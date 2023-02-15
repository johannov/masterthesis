#--- Packages ---#
library(TTR)
library(ARDL)
library(tidyr)
library(xlsx)
library(gtrendsR)
source("data_prep.r")

#--- decomposition ---#
ts = ts(df$log_demand, deltat=1/12)

#Decompose d
demand_d =decompose(ts)
#Plot decomposition
plot(demand_d, yax.flip=TRUE)
#Save trend and season variable
df$trend = demand_d$trend
df$season = demand_d$seasonal

#Fit ARDL
ardl_fit = auto_ardl(log_demand~log_hdd+log_price | trend + season, data=df, max_order=8)
summary(ardl_fit$best_model)

multipliers(ardl_fit$best_model)
multipliers(ardl_fit$best_model, "sr")

#Daily 
ardl_daily = auto_ardl(log_demand ~ log_hdd + log_price, data=daily_freq, max_order=8)
summary(ardl_daily$best_model)
multipliers(ardl_daily$best_model)
multipliers(ardl_daily$best_model, "sr")
