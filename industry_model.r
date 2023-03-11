source("data_prep.r")
library(MLmetrics)


#mWh input
generation <- read_csv("data/generation.csv", 
                       col_types = cols(Date = col_date(format = "%d.%m.%Y"))) %>% 
  rename(date=Date, natgas_generation = "Fossil gas [MWh] Calculated resolutions") %>% 
  select(date, natgas_generation)


#kWh input
industry_including_powergen <- read_csv("data/industry_including_powergen.csv", 
                                        col_types = cols(Gasday = col_date(format = "%Y-%m-%d"))) %>% 
  filter(Status != "preliminary") %>% 
  rename(date=Gasday) %>% mutate(total_rlm = (HGasRLMmT+LGasRLMmT+HGasRLMoT+LGasRLMoT)/1000) %>% 
  select(date, total_rlm)

ind_daily_freq <- inner_join(generation, industry_including_powergen, by="date") %>% 
  left_join(bloomberg_eex, by="date") %>% 
  left_join(dwd_weather, by="date") %>% 
  left_join(covid, by="date") %>% 
  left_join(linear, by="date") %>% 
  mutate(industry_demand = total_rlm-natgas_generation) %>% 
  select(date, industry_demand, hdd16, wind, emp, prod_idx, trade, price, stay_at_home)

ind_demdata <- ind_daily_freq %>% 
  mutate(week = substr(as.character(ISOweek(date)),7,8)) %>% 
  group_by(week) %>% 
  mutate(industry_demand = industry_demand - mean(industry_demand), hdd16 = hdd16 - mean(hdd16), emp = emp-mean(emp), prod_idx = prod_idx-mean(prod_idx))

head(ind_demdata)
industry_model = auto_ardl(industry_demand ~ hdd16 + wind + price, data = ind_demdata, max_order = 5, selection = "BIC")

?arima()

summary(arima(ind_demdata$industry_demand, order=c(1,0,0)))

plot(ind_demdata$industry_demand)
points(industry_model$best_model$fitted.values, col="red")

MAPE(data.frame(industry_model$best_model$fitted.values)[,1], data.frame(ind_demdata[6:488,"industry_demand"])[,1])
data.frame(industry_model$best_model$fitted.values)
data.frame(ind_demdata[6:488,"industry_demand"])
ind_demdata[3:488,"industry_demand"]
summary(industry_model$best_model)
multipliers(industry_model$best_model)
multipliers(industry_model$best_model, "sr")
