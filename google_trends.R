source("data_prep.r")
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ARDL)
library(gtrendsR)

#--- GOOGLE TRENDS---#

#--- Scrape google trends ---#
#search_words = c("energiekrise", "erdgas", "Energie sparen", "Energieknappheit")
#all = gtrends(keyword = search_words, geo="DE", time="2018-01-01 2023-02-14")
#tail(all$interest_over_time)

#date_intervals = c("2018-01-01")
#date=as.Date("2018-01-01")
#while (date <= as.Date("2023-02-14")-7){
#  date = as.Date(date + 7)
#  date_intervals = cbind(date_intervals, format(date, "%Y-%m-%d"))
  
#}
#date_intervals

#daily = c()
#for (i in 1:(length(date_intervals)-1)){
#  t = paste(date_intervals[i], format(as.Date(date_intervals[i+1])-1,"%Y-%m-%d"))
#  result = gtrends(keyword = search_words, geo="DE", time=t)$interest_over_time
#  daily = rbind(daily, result)
#}

krise = gtrends(keyword="energiekrise", geo="DE", time="2018-01-01 2023-02-14")
erdgas = gtrends(keyword="erdgas", geo="DE", time="2018-01-01 2023-02-14")
energiesparen = gtrends(keyword = "energie sparen",geo="DE", time="2018-01-01 2023-02-14")
knappheit = energiesparen = gtrends(keyword = "energieknappheit",geo="DE", time="2018-01-01 2023-02-14")

#Save

#write.xlsx(daily, ".data/daily_google_trends.xlsx")
#write.xlsx(all$interest_over_time, ".data/weekly_google_trends.xlsx")

# ---- Begin here ---#
daily_google <- read_excel("data/daily_google_trends.xlsx")
weekly_google <- read_excel("data/weekly_google_trends.xlsx")

#Add all missing dates in week data and copy hits downswards (will be used for scaling)
weekly_filled <- weekly_google %>%
  group_by(keyword) %>%
  mutate(date = as.Date(date)) %>% 
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(hits)

#Join daily and weekly data
joined = inner_join(weekly_filled, daily_google, by=c("date", "keyword")) %>%
  rename(daily_hit = hits.y, weekly_hit=hits.x) %>% 
  select(c("date","keyword","daily_hit","weekly_hit","month"))


#Adjust daily hits by monthly score
joined$adjusted = as.numeric(joined$weekly_hit)
is.na(joined$adjusted)=0

#For now just summing the scores of the different keywords 
#(they are weighted either way, so that energieknappheit never reaches as high as erdgas for instance)

google_trend_indicator = joined %>% 
  group_by(date) %>% 
  summarise(hits = sum(adjusted))

#--- Daily ARDL w/ google trends ---#

daily_freq_w_google = inner_join(daily_freq, google_trend_indicator, by="date") %>% 
  mutate(month = format(date, "%m"))


#Fit ARDL on daily data

ardl_fit = auto_ardl(demand~hdd+price+hits | month, data=daily_freq_w_google, max_order=8, selection = "BIC")
summary(ardl_fit$best_model)

multipliers(ardl_fit$best_model)
multipliers(ardl_fit$best_model, "sr")


# --- monthly ---#
google_month = google_trend_indicator %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(year, month) %>%
  summarise(hits = sum(hits), date = first(date))

daily_freq_w_google$month = as.factor(daily_freq_w_google$month)

df_month = inner_join(df,google_month, by=c("year.y"="year", "month.y"="month")) %>% na.omit(hits)
#If need for season: df_month$month = as.factor(df_month$month.y)

ardl_google = auto_ardl(log_demand~log_hdd+log_price + hits, data=df_month, max_order=8, selection = "BIC")$best_model
ardl_wo_google = auto_ardl(log_demand~log_hdd+log_price, data=df_month[2:60,], max_order=8, selection = "BIC")$best_model

anova(ardl_google, ardl_wo_google)
