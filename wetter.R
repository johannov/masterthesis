library(readr)
library(stringr)
library(tidyverse)
library(vegperiod)

wetter <- read_csv("wetterdienst/dwd_rawdata_march10.csv")
station_info <- read_csv("wetterdienst/KL_Tageswerte_Beschreibung_Stationen.csv")
station_info$Bundeslan <- str_split(station_info$Bundeslan, ",") %>% 
  sapply(tail, 1) %>% 
  as.character()
station_info$station_id <- substr(station_info$`Stations_id von_datum bis_datum`, 1, 5)

station_info$long <- str_split(station_info$`geoLaenge Stationsname`, " ") %>% 
  sapply(head, 1) %>% 
  as.character()

new_station <- data <- read_delim("wetterdienst/rr_wetter_tageswerte_Beschreibung_Stationen - Copy.csv", delim = "  ", trim_ws = TRUE)
new_station$id <- str_split

wetter_agg <- merge(wetter, all_stations, by = "station_id", all.x = TRUE)

wetter_agg <- pivot_wider(wetter_agg, values_from = "value", names_from = "parameter", id_cols = c("station_id", "date", "geoBreite", "geoLaenge"))

wetter_agg <- drop_na(wetter_agg, any_of(c("temperature_air_mean_200")))


wetter_agg$celctemp <- wetter_agg$temperature_air_mean_200 - 272.15
wetter_agg$hdd16 <- ifelse(wetter_agg$celctemp > 16, 0, 16 - wetter_agg$celctemp)
wetter_agg$hdd17 <- ifelse(wetter_agg$celctemp > 17, 0, 17 - wetter_agg$celctemp)
wetter_agg$hdd15 <- ifelse(wetter_agg$celctemp > 15, 0, 15 - wetter_agg$celctemp)

harald <- wetter_agg[c(1,2,4,5,6,9,10,11)]
colnames(harald)[4] <- "lat"
write.csv(harald, "wetterdienst/harald_weather.csv")



wetter_med <- aggregate(wetter_agg, by = list(wetter_agg$Bundeslan, wetter_agg$date), FUN = median)
wetter_mean <- aggregate(wetter_agg, by = list(wetter_agg$Bundeslan, wetter_agg$date), FUN = mean)

bundes_wetter_med <- wetter_med[c(1,4,8:15)]
colnames(bundes_wetter_med)[1] <- "bundesland"

bundes_wetter_mean <- wetter_mean[c(1,4,8:15)]
colnames(bundes_wetter_mean)[1] <- "bundesland"

# Calculate weights
bl16 <- read_csv("data/state_statistics.csv")
bl16 <- bl16[c(1, 2, 3)]
colnames(bl16) <- c("bl", "pop", "gas")
totalgas <- sum(bl16$gas)
bl16$gas_weight <- bl16$gas / totalgas
totalpop <- sum(bl16$pop)
bl16$pop_weight <- bl16$pop / totalpop
bl16$bl_id <- c(1:16)


bundes_wetter_med$bl_id <- plyr::mapvalues(bundes_wetter_med$bundesland, from = bundes_wetter_med$bundesland[1:16], to = c(1:16))
bundes_wetter_mean$bl_id <- plyr::mapvalues(bundes_wetter_mean$bundesland, from = bundes_wetter_mean$bundesland[1:16], to = c(1:16))

bundes_wetter_weighted_med <- merge(bundes_wetter_med, bl16, by = "bl_id", all.x = TRUE)
bundes_wetter_weighted_mean <- merge(bundes_wetter_mean, bl16, by = "bl_id", all.x = TRUE)

bundes_wetter_weighted_med$celctemp_200 <- bundes_wetter_weighted_med$temperature_air_mean_200 - 272.15
bundes_wetter_weighted_med$hdd16_200 <- ifelse(bundes_wetter_weighted_med$celctemp > 16, 0, 16 - bundes_wetter_weighted_med$celctemp)
bundes_wetter_weighted_med$hdd17_200 <- ifelse(bundes_wetter_weighted_med$celctemp > 17, 0, 17 - bundes_wetter_weighted_med$celctemp)
bundes_wetter_weighted_med$hdd15_200 <- ifelse(bundes_wetter_weighted_med$celctemp > 15, 0, 15 - bundes_wetter_weighted_med$celctemp)

bundes_wetter_weighted_mean$celctemp_200 <- bundes_wetter_weighted_mean$temperature_air_mean_200 - 272.15
bundes_wetter_weighted_mean$hdd16_200 <- ifelse(bundes_wetter_weighted_mean$celctemp > 16, 0, 16 - bundes_wetter_weighted_mean$celctemp)
bundes_wetter_weighted_mean$hdd17_200 <- ifelse(bundes_wetter_weighted_mean$celctemp > 17, 0, 17 - bundes_wetter_weighted_mean$celctemp)
bundes_wetter_weighted_mean$hdd15_200 <- ifelse(bundes_wetter_weighted_mean$celctemp > 15, 0, 15 - bundes_wetter_weighted_mean$celctemp)

bundes_wetter_weighted_med <- bundes_wetter_weighted_med %>%
  mutate(across(c(4:10), ~ . * pop_weight))

bundes_wetter_weighted_mean <- bundes_wetter_weighted_mean %>%
  mutate(across(c(4:10), ~ . * pop_weight))

wetter_wa_med <- bundes_wetter_weighted_med %>% 
  group_by(date) %>% 
  summarize(
    wind_speed_sum = sum(wind_speed),
    temperature_air_mean_200_sum = sum(temperature_air_mean_200 ),
    hdd16_sum = sum(hdd16_200),
    hdd17_sum = sum(hdd17_200),
    hdd15_sum = sum(hdd15_200),
    sunshine_duration_sum = sum(sunshine_duration),
    cloud_cover_total_sum = sum(cloud_cover_total)
  )


wetter_wa_mean <- bundes_wetter_weighted_mean %>% 
  group_by(date) %>% 
  summarize(
    wind_speed_sum = sum(wind_speed),
    temperature_air_mean_200_sum = sum(temperature_air_mean_200 ),
    hdd16_sum = sum(hdd16_200),
    hdd17_sum = sum(hdd17_200),
    hdd15_sum = sum(hdd15_200),
    sunshine_duration_sum = sum(sunshine_duration),
    cloud_cover_total_sum = sum(cloud_cover_total)
  )


# wetter_wa$celctemp <- wetter_wa$temperature_air_mean_200_sum - 272.15
# wetter_wa$hdd16 <- ifelse(wetter_wa$celctemp > 16, 0, 16 - wetter_wa$celctemp)
# wetter_wa$hdd17 <- ifelse(wetter_wa$celctemp > 17, 0, 17 - wetter_wa$celctemp)
# wetter_wa$hdd15 <- ifelse(wetter_wa$celctemp > 15, 0, 15 - wetter_wa$celctemp)
# wetter_wa$date <- as.Date(wetter_wa$date)
# write.csv(wetter_wa, "bl_weighted_weather.csv", row.names = FALSE)

# Write file with currently best weather variables
write.csv(wetter_wa_med[c("date", "hdd16_sum", "wind_speed_sum")], "data/dwd_pop_weighted_weather_med.csv", row.names = FALSE)
write.csv(wetter_wa_mean[c("date", "hdd16_sum", "wind_speed_sum", "cloud_cover_total_sum")], "data/dwd_pop_weighted_weather_mean.csv", row.names = FALSE)

# Remove temp files
rm(bl16, bundes_wetter, bundes_wetter_weighted, harald, wetter, wetter_agg, wetter_med, totalgas, totalpop, station_info, bundes_wetter_weighted_med, bundes_wetter_weighted_mean,bundes_wetter_mean, bundes_wetter_med, wetter_med, wetter_mean, wetter_wa, wetter_wa_med, wetter_wa_mean )


colnames(pop_dens) <- c("gitter_id", "ags", "municipality", "enhet", "county", "bundesland", "reference_dens", "pop_dens", "plausibility")
pop_dens <- pop_dens[-4]
pop_dens$pop_dens <- ifelse(pop_dens$pop_dens == "[0-3]", 0, pop_dens$pop_dens)

write.csv(pop_dens, "wetterdienst/pop_dens.csv")

read_stations <- function(file_path) {
  # Read in the data
  lines <- readLines(file_path)
  header <- lines[1]
  lines <- lines[-1]
  
  df <- data.frame(Stations_id = character(),
                   von_datum = character(),
                   bis_datum = character(),
                   Stationshoehe = character(),
                   geoBreite = character(),
                   geoLaenge = character(),
                   stringsAsFactors = FALSE)
  
  
  for (line in lines) {
    newline <- str_split(line, " +")[[1]][c(1:6)]
    df <- rbind(df, newline)
  }
  
  colnames(df) <- str_split(header, " ")[[1]][c(1:6)]
  colnames(df)[1] <- "station_id"
  return(df)
}

kl_stations <- read_stations("wetterdienst/KL_Tageswerte_Beschreibung_Stationen.txt")
rr_stations <- read_stations("wetterdienst/rr_wetter_tageswerte_Beschreibung_Stationen.txt")

all_stations <- rbind(kl_stations, rr_stations)
all_stations <- all_stations %>% distinct(station_id, .keep_all = TRUE)

my_stations <- wetter_agg %>% distinct(station_id, .keep_all = TRUE)
my_stations <- drop_na(my_stations, any_of("geoBreite"))

colnames(my_stations)[c(11,12)] <- c("lat", "long") 

my_stations <- my_stations[c(1, 8:12)]

write.csv(my_stations, "wetterdienst/stations.csv", row.names = FALSE)

stations <- read.DWDstations(type = "climate", period = "recent", resolution = "daily")
