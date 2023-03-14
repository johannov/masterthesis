source("data_prep.r")

# --- ARDL --- #

#Best yet model:

#full_mod <- auto_ardl(demand ~ hdd16 + price + wind + cloud + hddwar + unemp + prod_idx + government_response + economic_support + covid, data = demdata, max_order = 5, selection = "BIC")

#full_mod_5 <- auto_ardl(demand ~ hdd16 + price + wind + cloud + hddwar + unemp + covid, data = demdata, max_order = c(7,7,7,7,2,1,1,1), selection = "BIC", grid=TRUE)
#best=c(6,7,0,1,0,1,0,0)
#Overruler dette med å sette lag av pris = 1.

mod <- ardl(demand ~ hdd16 + price + wind + cloud + hddwar + unemp + covid, data=demdata, order = c(6,7,1,1,0,1,0,0))
summary(mod)

multipliers(mod)
multipliers(mod, "sr")

#Fixed order -1 lar det variere, kan sette fast order på feks pris
mod <- auto_ardl(demand ~ hdd16 + price + cloud + hddwar + unemp | covid + wind, data=demdata, fixed_order=c(-1,-1,1,-1,-1,-1), max_order=7, selection="BIC")
recm(mod, case="uc")

cor(demdata$price, demdata$wind)

ml=multipliers(mod$best_model)
ms=multipliers(mod$best_model, "sr")

# Calculating elasticities #
avg_price <- mean(daily_freq$price)
avg_demand <- mean(daily_freq$demand)
avg_hdd <- mean(daily_freq$hdd16)
avg_wind <- mean(daily_freq$wind)
avg_unemp <- mean(daily_freq$unemp)
avg_cloud <- mean(daily_freq$cloud)

#price
ms$Estimate[3]*avg_price/avg_demand
ml$Estimate[3]*avg_price/avg_demand

#hdd
ms$Estimate[2]*avg_hdd/avg_demand
ml$Estimate[2]*avg_hdd/avg_demand

#hdd after war
(ms$Estimate[2]+ms$Estimate[7])*avg_hdd/avg_demand

#unemployment
ms$Estimate[6]*avg_unemp/avg_demand
ml$Estimate[6]*avg_unemp/avg_demand

#wind
ms$Estimate[4]*avg_wind/avg_demand
ml$Estimate[4]*avg_wind/avg_demand

#cloud
ms$Estimate[5]*avg_cloud/avg_demand
ml$Estimate[5]*avg_cloud/avg_demand

mod$best_order

# ---- BELOW HERE: MONTH FOR INTEREST --- #

#---Monthly agg of EEX ---#

monthly_data <- demdata %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
  group_by(month, year) %>% 
  summarize(demand = sum(demand), price = mean(price), hdd16 = sum(hdd16), wind=mean(wind), cloud=mean(cloud), work_closed=sum(work_closed), unemp=first(unemp), prod_idx = first(prod_idx), covid=mean(covid), war=mean(war_d))


monthly_data$hddwar = monthly_data$hdd16*monthly_data$war

month_mod <- auto_ardl(demand ~ hdd16 + price + wind + cloud + hddwar + unemp + covid, data = monthly_data, max_order = 3, selection = "BIC")
summary(month_mod$best_model)
multipliers(month_mod$best_model)
multipliers(month_mod$best_model, "sr")

#--- Compare to verivox ---#

verivox = monthly_data %>% 
  left_join(verivox_prices, by=c("month", "year")) %>% 
  rename(verivox = Zahl)

#Cloud og wind ikke signif, fjerner de fra estimering.

month_mod <- auto_ardl(demand ~ hdd16 + verivox + unemp + covid, data = verivox, max_order = 3, selection = "BIC")
summary(month_mod$best_model)

ml = multipliers(month_mod$best_model)
ms = multipliers(month_mod$best_model, "sr")

ms$Estimate[3]*mean(verivox$verivox %>% na.omit)/mean(daily_freq$demand)
ml$Estimate[3]*mean(verivox$verivox %>% na.omit)/mean(daily_freq$demand)
