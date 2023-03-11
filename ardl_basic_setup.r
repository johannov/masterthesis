
# --- ARDL --- #

#Best yet model:

#full_mod <- auto_ardl(demand ~ hdd16 + price + wind + cloud + hddwar + unemp + prod_idx + government_response + economic_support + covid, data = demdata, max_order = 5, selection = "BIC")
full_mod <- auto_ardl(demand ~ hdd16 + price + wind + cloud + hddwar + unemp + covid, data = demdata, max_order = 5, selection = "BIC")
summary(full_mod$best_model)

ml=multipliers(full_mod$best_model)
ms=multipliers(full_mod$best_model, "sr")
ms
ml

#Variance inflation factor - measure of collinearity
vif(full_mod$best_model)


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
(ms$Estimate[2]+ms$Estimate[6])*avg_hdd/avg_demand

#unemployment
ms$Estimate[7]*avg_unemp/avg_demand
ml$Estimate[7]*avg_unemp/avg_demand

#wind
ms$Estimate[4]*avg_wind/avg_demand
ml$Estimate[4]*avg_wind/avg_demand

#cloud
ms$Estimate[5]*avg_cloud/avg_demand
ml$Estimate[5]*avg_cloud/avg_demand


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
