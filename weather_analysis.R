library(ARDL)
library(tidyverse)
library(ISOweek)
library(tseries)

# Load natural gas consumption data
demand <- read_csv("./data/230222_Restlast.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))
demand <- subset(demand, date != as.Date("2019-01-01"))

# Weather, Population from states
weather_statemean <- read_csv("data/dwd_pop_weighted_weather_mean.csv")

# Weather, population from georef
dwd_df <- read_csv("data/dwd_rawdata_march10.csv")
dwd_df$station_id <- as.double(dwd_df$station_id)
pop_df <- read_csv("data/WeatherStationsWithPopAndId.csv")
df <- merge(pop_df[c("SUM_pop_de", "station_id")], dwd_df, by = "station_id") # Remember to check rest

df <- pivot_wider(df, values_from = "value", names_from = "parameter", id_cols = c("station_id", "date", "SUM_pop_de"))

df$celc <- df$temperature_air_mean_200 - 273.15
df$hdd16 <- ifelse(df$celc < 16, 16 - df$celc, 0)

total_pop <-  sum(pop_df$SUM_pop_de)

df$weight <- df$SUM_pop_de / total_pop

df_all <- drop_na(df, any_of(c("wind_speed", "cloud_cover_total", "temperature_air_mean_200",
                         "celc", "hdd16")))

df_hdd <- drop_na(df, any_of(c("hdd16")))


summarizeWeather <- function(data) {

  data <- df_all %>%
    mutate(across(c("wind_speed", "cloud_cover_total", "temperature_air_mean_200",
                    "sunshine_duration", "celc", "hdd16"), ~ . * weight))
  
  
  data <- data %>% 
    group_by(date) %>% 
    summarize(
      wind_speed = sum(wind_speed),
      temperature_air_mean_200 = sum(temperature_air_mean_200),
      hdd16 = sum(hdd16),
      cloud_cover_total = sum(cloud_cover_total),
      sunshine_duration = sum(sunshine_duration),
      weight = sum(weight),
      celc = sum(celc)
    )
  
  data[c("wind_speed", "cloud_cover_total", "temperature_air_mean_200",
         "sunshine_duration", "celc", "hdd16")] <-
    data[c("wind_speed", "cloud_cover_total", "temperature_air_mean_200",
                                                          "sunshine_duration", "celc", "hdd16")] / data$weight

  
  data$yearweek <- ISOweek(data$date)
  data$week <- substr(as.character(data$yearweek), 7, 8)
  
  data$date <- as.Date(data$date)
  

  data <- merge(data, demand, by = "date")
  
  return(data)
}

data_all <- summarizeWeather(df_all)
data_hdd <- summarizeWeather(df_hdd)

data_comp <- merge(data_all, weather_statemean, by = "date")

# Demeaned, seasonal adjusted
demdata <- data_comp %>% 
  group_by(week) %>% 
  mutate(demand = demand - mean(demand),
         wind_speed = wind_speed - mean(wind_speed),
         hdd16 = hdd16 - mean(hdd16),
         cloud_cover_total = cloud_cover_total - mean(cloud_cover_total),
         hdd16_sum = hdd16_sum - mean(hdd16_sum),
         wind_speed_sum = wind_speed_sum - mean(wind_speed_sum),
         cloud_cover_total_sum = cloud_cover_total_sum - mean(cloud_cover_total_sum)
  )

train_demdata <- subset(demdata, date < as.Date("2021-08-01"))

lm_state <- lm(demand ~ hdd16_sum + dplyr::lag(hdd16_sum) + wind_speed_sum + cloud_cover_total_sum, data = train_demdata)
summary(lm_state)

lm_pop <- lm(demand ~ hdd16 + dplyr::lag(hdd16) + wind_speed + cloud_cover_total, data = train_demdata)
summary(lm_pop)
