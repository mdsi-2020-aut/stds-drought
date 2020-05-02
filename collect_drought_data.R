library("bomrang")
library("ASGS")
library("ASGS.foyer")
library("dplyr")

# Testing the bomrang api
# rain_1007 <- get_historical(stationid = 1007, type = 'rain') # type = rain, solar, max, min

# Get Weather Station File
bom_stn_id_file <- read.csv("data/BOM_StationID.csv")

#  Store Ids,Region, Lat, Long Only
stn_ids_only <- bom_stn_id_file %>%
   rename("station_id"= "ï..STN_NUM") %>% 
   select(station_id, REGION, LATITUDE,LONGITUDE)


#Loop through the IDs for Rainfall data
rainfall_df = data.frame()
temperature_df = data.frame()
names(stn_ids_only)
for(id in stn_ids_only$station_id) {
tryCatch({
  rainfall_df <- rbind(temperature_df, get_historical(stationid = id, type = 'rain'))}, error= function(e) {paste("Rainfall Data not found for the station", id)}) 
  
  tryCatch({
    temperature_df <- rbind(temperature_df, get_historical(stationid = id, type = 'max'))}, error= function(e) {paste("Temperature Data not found for the station", id)})

}
names(rainfall_df)
rainfall_df <- rainfall_df %>% 
  inner_join(stn_ids_only, by= c("station_number" = "station_id"))
head(rainfall_df)

temperature_df <- temperature_df %>% 
  inner_join(stn_ids_only, by= c("station_number" = "station_id"))
head(temperature_df)

rainfall_df <- rainfall_df %>%
  filter(year >= 1990)
nrow(rainfall_df) # 11080

temperature_df <- temperature_df %>%
  filter(year >= 1990)
nrow(temperature_df) # 4672525

save(rainfall_df, file="data/bom_rainfall.RData")
save(temperature_df, file="data/bom_temperature.RData")

write.csv(rainfall_df, "data/bom_rainfall.csv")
write.csv(temperature_df, "data/bom_temperature.csv")
head(rainfall_df)


# Merge with SA4 Data
temperature_df$territory_sa4 <- latlon2SA(temperature_df$LATITUDE, temperature_df$LONGITUDE, to = "SA4", yr = "2016")
rainfall_df$territory_sa4 <- latlon2SA(rainfall_df$LATITUDE, rainfall_df$LONGITUDE, to = "SA4", yr = "2016")
nrow(rainfall_df)

# Save Again
save(rainfall_df, file="data/bom_rainfall.RData")
save(temperature_df, file="data/bom_temperature.RData")

head(temperature_df)
# Loop through the IDs for Max temperature data
# for(id in stn_ids_only$station_id) {
#   tryCatch({
#     temperature_df <- rbind(temperature_df, get_historical(stationid = id, type = 'max'))}, error= function(e) {paste("Data not found for the station", id)})
# }










