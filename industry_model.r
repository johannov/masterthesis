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
  mutate(government_response = replace_na(government_response, 0),
         economic_support = replace_na(economic_support, 0),
         stay_at_home = replace_na(stay_at_home, 0),
         work_closed = replace_na(work_closed, 0),
         school_closed = replace_na(school_closed, 0)) %>% 
  
  left_join(linear, by="date") %>% 
  subset(date < as.Date("02-01-2023", format="%d-%m-%Y")) %>%
  
  mutate(industry_demand = total_rlm-natgas_generation, price = price / (cpi/100)) %>% 
  select(date, industry_demand, hdd16, wind, unemp, prod_idx, trade, price, work_closed, economic_support, cloud, fertilizer, government_response)

ind_demdata <- ind_daily_freq %>% 
  mutate(week = substr(as.character(ISOweek(date)),7,8)) %>% 
  group_by(week) %>% 
  mutate(industry_demand = industry_demand - mean(industry_demand), hdd16 = hdd16 - mean(hdd16), unemp = unemp - mean(unemp), prod_idx = prod_idx-mean(prod_idx))

ind_month =  ind_daily_freq %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
  group_by(month, year) %>% 
  summarise(date = first(date), price = mean(price), industry_demand = sum(industry_demand), hdd16 = sum(hdd16), wind=mean(wind), cloud=mean(cloud), 
            economic_support = mean(economic_support), government_response = mean(government_response)) %>% 
  left_join(unemployment, by="date") %>% 
  left_join(production_industry, by="date") %>% 
  left_join(fertilizer_idx, by="date") %>% 
  rename(unemp = adj_unemp, prod_idx = prod_index_adj) %>% 
  select(date, industry_demand, price, hdd16, wind, cloud, unemp, prod_idx, economic_support, government_response, fertilizer)

ind_demdata$covid = ifelse(ind_demdata$work_closed>0 ,1, 0)

industry_model = auto_ardl(industry_demand ~ hdd + price + unemp + prod_idx + economic_support + fertilizer, data = ind_demdata, max_order = 5, selection = "BIC")
summary(industry_model$best_model)

plot(ind_demdata$industry_demand)
plot(ind_demdata$price)
multipliers(industry_model$best_model, "sr")
multipliers(industry_model$best_model)
