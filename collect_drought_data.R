library("bomrang")
library("RCurl")
library("dplyr")

# Testing the bomrang api
# rain_1007 <- get_historical(stationid = 1007, type = 'rain') # type = rain, solar, max, min

# Get Weather Station File
bom_stn_id_file <- read.csv("data/BOM_StationID.csv")
names(bom_stn_id_file)
# # Store Ids Only
stn_ids_only <- bom_stn_id_file %>%
  select(1)
head(stn_ids_only)

# stn_ids_only <- c(1006, 1007, 066062)
# name <- paste(paste("rain", 1234, sep = "_"),"csv", sep=".")

#Loop through the IDs for Rainfall data
rainfall_df = data.frame()
for(id in stn_ids_only) {
  print(id)
  rainfall_df <- rbind(rainfall_df, get_historical(stationid = id, type = 'rain'))
}
write.csv(rainfall_df, "BOM_Rainfall.csv")

#Loop through the IDs for Max temperature data
temperature_df = data.frame()
for(id in stn_ids_only) {
  print(id)
  temperature_df <- rbind(temperature_df, get_historical(stationid = id, type = 'max'))
}
write.csv(temperature_df, "BOM_Temperature.csv")

# Remove the observations prior to 1920
bom_rainfall <- read.csv("BOM_Rainfall.csv")
names(bom_rainfall)
nrow(bom_rainfall) #108113
bom_rainfall <- bom_rainfall %>% 
  filter(year >= 1920)
nrow(bom_rainfall) #85469

# Remove the observations prior to 1920
bom_temperature <- read.csv("BOM_Temperature.csv")
names(bom_rainfall)
nrow(bom_rainfall) 
bom_rainfall <- bom_temperature %>% 
  filter(year >= 1920)
nrow(bom_temperature)
