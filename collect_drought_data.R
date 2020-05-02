library("bomrang")
library("RCurl")
library("dplyr")

# Testing the bomrang api
#rain_1007 <- get_historical(stationid = 1007, type = 'rain') # type = rain, solar, max, min

# Get Weather Station File
bom_stn_id_file <- read.csv("data/BOM_StationID.csv")

#  Store Ids,Region, Lat, Long Only
stn_ids_only <- bom_stn_id_file %>%
   rename("station_id"= "ï..STN_NUM") %>% 
   select(station_id, REGION, LATITUDE,LONGITUDE)


#Loop through the IDs for Rainfall data
rainfall_df = data.frame()
for(id in stn_ids_only$station_id) {
tryCatch({
  rainfall_df <- rbind(temperature_df, get_historical(stationid = id, type = 'rain'))}, error= function(e) {paste("Data not found for the station", id)})
}
rainfall_df <- rainfall_df %>% 
  inner_join(stn_ids_only, by= c("station_id" = "Bureau of Meteorology station number"))

rainfall_df <- rainfall_df %>%
  filter(year >= 1990)

save(rainfall_df, file="data/bom_rainfall.RData")


# Loop through the IDs for Max temperature data
temperature_df = data.frame()
for(id in stn_ids_only$station_id) {
  tryCatch({
    temperature_df <- rbind(temperature_df, get_historical(stationid = id, type = 'max'))}, error= function(e) {paste("Data not found for the station", id)})
}
temperature_df <- temperature_df %>% 
  inner_join(stn_ids_only, by= c("station_id" = "Bureau of Meteorology station number"))

temperature_df <- temperature_df %>%
  filter(year >= 1990)

save(temperature_df, file="data/bom_temperature.RData")


