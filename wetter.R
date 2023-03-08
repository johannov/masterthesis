library(readr)
library(stringr)
library(tidyverse)

wetter <- read_csv("wetterdienst/temp_data.csv")
station_info <- read_csv("wetterdienst/KL_Tageswerte_Beschreibung_Stationen.csv")
station_info$Bundeslan <- str_split(station_info$Bundeslan, ",") %>% 
  sapply(tail, 1) %>% 
  as.character()
station_info$station_id <- substr(station_info$`Stations_id von_datum bis_datum`, 1, 5)

station_info$long <- str_split(station_info$`geoLaenge Stationsname`, " ") %>% 
  sapply(head, 1) %>% 
  as.character()


wetter_agg <- merge(wetter, station_info, by = "station_id", all.x = TRUE)

wetter_agg <- pivot_wider(wetter_agg, values_from = "value", names_from = "parameter", id_cols = c("station_id", "date", "Bundeslan", "long", "geoBreite"))

wetter_agg <- drop_na(wetter_agg)

harald <- wetter_agg[c(1, 2, 3, 4, 5, 7)]
colnames(harald)[5] <- "lat"
colnames(harald)[6] <- "kelvin"
colnames(harald)[3] <- "bundesland"

write.csv(harald, "station_weather.csv", row.names = FALSE)

wetter_agg$celctemp <- wetter_agg$temperature_air_mean_200 - 272.15
wetter_agg$hdd16 <- ifelse(wetter_agg$celctemp > 16, 0, 16 - wetter_agg$celctemp)
wetter_agg$hdd17 <- ifelse(wetter_agg$celctemp > 17, 0, 17 - wetter_agg$celctemp)
wetter_agg$hdd15 <- ifelse(wetter_agg$celctemp > 15, 0, 15 - wetter_agg$celctemp)


wetter_med <- aggregate(wetter_agg, by = list(wetter_agg$Bundeslan, wetter_agg$date), FUN = median)
# wetter_mean <- aggregate(wetter_agg, by = list(wetter_agg$Bundeslan, wetter_agg$date), FUN = mean)

bundes_wetter <- wetter_med[c(1,2,8,9,10,11)]
colnames(bundes_wetter) <- c("bundesland", "date", "wind_speed", "temperature_air_mean_200", "temperature_air_min_200", "temperature_air_max_200")

# Calculate weights
bl16 <- read_csv("C:/Users/dja/Downloads/data-I5Kww.csv")
bl16 <- bl16[c(1, 2, 3)]
colnames(bl16) <- c("bl", "pop", "gas")
totalgas <- sum(bl16$gas)
bl16$gas_weight <- bl16$gas / totalgas
totalpop <- sum(bl16$pop)
bl16$pop_weight <- bl16$pop / totalpop
bl16$bl_id <- c(1:16)


bundes_wetter$bl_id <- plyr::mapvalues(bundes_wetter$bundesland, from = bundes_wetter$bundesland[1:16], to = c(1:16))

bundes_wetter_weighted <- merge(bundes_wetter, bl16, by = "bl_id", all.x = TRUE)

bundes_wetter_weighted$celctemp_200 <- bundes_wetter_weighted$temperature_air_mean_200 - 272.15
bundes_wetter_weighted$hdd16_200 <- ifelse(bundes_wetter_weighted$celctemp > 16, 0, 16 - bundes_wetter_weighted$celctemp)
bundes_wetter_weighted$hdd17_200 <- ifelse(bundes_wetter_weighted$celctemp > 17, 0, 17 - bundes_wetter_weighted$celctemp)
bundes_wetter_weighted$hdd15_200 <- ifelse(bundes_wetter_weighted$celctemp > 15, 0, 15 - bundes_wetter_weighted$celctemp)

bundes_wetter_weighted <- bundes_wetter_weighted %>%
  mutate(across(c("wind_speed", ends_with("200")), ~ . * gas_weight))

wetter_wa <- bundes_wetter_weighted %>% 
  group_by(date) %>% 
  summarize(
    wind_speed_sum = sum(wind_speed),
    temperature_air_mean_200_sum = sum(temperature_air_mean_200 ),
    temperature_air_min_200_sum = sum(temperature_air_min_200),
    temperature_air_max_200_sum = sum(temperature_air_max_200),
    hdd16_sum = sum(hdd16_200),
    hdd17_sum = sum(hdd17_200),
    hdd15_sum = sum(hdd15_200)
  )

wetter_wa$celctemp <- wetter_wa$temperature_air_mean_200_sum - 272.15
wetter_wa$hdd16 <- ifelse(wetter_wa$celctemp > 16, 0, 16 - wetter_wa$celctemp)
wetter_wa$hdd17 <- ifelse(wetter_wa$celctemp > 17, 0, 17 - wetter_wa$celctemp)
wetter_wa$hdd15 <- ifelse(wetter_wa$celctemp > 15, 0, 15 - wetter_wa$celctemp)
wetter_wa$date <- as.Date(wetter_wa$date)
write.csv(wetter_wa, "bl_weighted_weather.csv", row.names = FALSE)
