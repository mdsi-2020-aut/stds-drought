library("bomrang")
library("ASGS")
library("ASGS.foyer")
library("dplyr")
library("stringr")
library(tidyverse)

# Testing the bomrang api
# rain_1007 <- get_historical(stationid = 1007, type = 'rain') # type = rain, solar, max, min

# Get Weather Station File
bom_stn_data_main <- read.csv("data/BOM_StationID.csv")
view(bom_stn_data_main)

#  Store Ids,Region, Lat, Long Only
station_info <- bom_stn_data_main %>%
  rename("station_id"= "ï..STN_NUM") %>% 
  select(station_id, REGION, LATITUDE,LONGITUDE)
view(station_info)

#Loop through the IDs for Rainfall data
rainfall_df = data.frame()
temperature_df = data.frame()
names(station_info)
for(id in station_info$station_id) {
  tryCatch({
    rainfall_df <- rbind(temperature_df, get_historical(stationid = id, type = 'rain'))}, error= function(e) {paste("Rainfall Data not found for the station", id)}) 
  
  tryCatch({
    temperature_df <- rbind(temperature_df, get_historical(stationid = id, type = 'max'))}, error= function(e) {paste("Temperature Data not found for the station", id)})
}

head(temperature_df)
# save the original data downloaded from Boomrang in R-data for future re-use
save(rainfall_df, file="data/HPT/bom_rainfall_org.RData")
save(temperature_df, file="data/HPT/bom_temperature_org.RData")

load("data/HPT/bom_temperature_org.RData")
load("data/HPT/bom_rainfall_org.RData")
head(temperature_df)
head(rainfall_df)

# Join with Weather Station dataframe to get lat/lon for future merging with SA4
rainfall_stn <- rainfall_df %>% 
  inner_join(station_info, by= c("station_number" = "station_id"))

temperature_stn <- temperature_df %>% 
  inner_join(station_info, by= c("station_number" = "station_id"))

# Filter out the records prior to 1990
rainfall_stn <- rainfall_stn %>%
  filter(year >= 1996)
nrow(rainfall_stn) # 8893

temperature_stn <- temperature_stn %>%
  filter(year >= 1996)
head(temperature_stn) # 2900834

# Self-tracking purpose
# write.csv(rainfall_df, "data/bom_rainfall.csv")
# write.csv(temperature_df, "data/bom_temperature.csv")

# Merge with SA4 Data
temperature_stn$territory_sa4 <- ASGS::latlon2SA(temperature_stn$LATITUDE, temperature_stn$LONGITUDE, to = "SA4", yr = "2016")
rainfall_stn$territory_sa4 <- ASGS::latlon2SA(rainfall_stn$LATITUDE, rainfall_stn$LONGITUDE, to = "SA4", yr = "2016")

head(temperature_stn)
head(rainfall_stn)
temperature_sa4 <- temperature_stn
rainfall_sa4 <- rainfall_stn

# Load the preprocessed Data
load("data/HPT/bom_rainfall_sa4.RData")
load("data/HPT/bom_temperature_sa4.RData")
View(rainfall_sa4) # 8893
nrow(temperature_sa4) # 2900834
save(rainfall_sa4, file="data/HPT/bom_rainfall_sa4.RData")
save(temperature_sa4, file="data/HPT/bom_temperature_sa4.RData")

load("data/HPT/bom_rainfall_sa4.RData")
load("data/HPT/bom_temperature_sa4.RData")
head(temperature_sa4)
head(rainfall_sa4)

# Look up the count of rainfall station Id in each SA4
terr_rainfall_stn_count <- rainfall_sa4 %>% 
  select(territory_sa4, station_number) %>% 
  distinct() %>% 
  group_by(territory_sa4) %>%
  summarise(rainfall_stn_count = n()) %>%
  arrange(desc(rainfall_stn_count)) 
View(terr_rainfall_stn_count)

# Look up the count of temperature station Id in each SA4
terr_weather_stn_count <- temperature_sa4 %>% 
  select(territory_sa4, station_number) %>% 
  distinct() %>% 
  group_by(territory_sa4) %>%
  summarise(temp_stn_count = n()) %>%
  arrange(desc(temp_stn_count)) 
View(terr_weather_stn_count)
# Get SA4 list
unemployment$territory_sa4 <- str_trim(unemployment$territory_sa4, side="both")
unique(unemployment$territory_sa4)
pure_sa4_list <- unemployment %>% 
  select(territory_sa4) %>% 
  distinct()
pure_sa4_list # 87 
# Check count of stations in each SA4 to identify the SA4 with no Station 
lookup_missing_weatherstn <- pure_sa4_list %>% 
  left_join(terr_weather_stn_count, by=c("territory_sa4"= "territory_sa4"))
View(lookup_missing_weatherstn)
write.csv(lookup_missing_weatherstn, file="data/HPT/temp_stn_count_by_terriority.csv")

# Check count of stations in each SA4 to identify the SA4 with no Station 
lookup_missing_rainfallstn <- pure_sa4_list %>% 
  left_join(terr_rainfall_stn_count, by=c("territory_sa4"= "territory_sa4"))
View(lookup_missing_rainfallstn)
write.csv(lookup_missing_rainfallstn, file="data/HPT/rainfall_stn_count_by_terriority.csv")


# Save Again for self-trecking purpose, optional
# save(rainfall_df, file="data/bom_rainfall.RData")
# save(temperature_df, file="data/bom_temperature.RData")

# load("data/bom_temperature.RData")
# load("data/bom_rainfall.RData")


## Summarise daily data to Monthly Data
temperature_sa4_monthyear <- temperature_sa4 %>% mutate(yearmonth = as.Date(paste(year, str_pad(month,width=2,side="left",pad="0"),"01",sep="-"))) %>% group_by(territory_sa4, yearmonth) %>% summarise(tmp_mean = mean(max_temperature), tmp_max = max(max_temperature), tmp_min = min(max_temperature), tmp_median = median(max_temperature))
head(temperature_sa4_monthyear)

rainfall_sa4_monthyear <- rainfall_sa4 %>%  mutate(yearmonth = as.Date(paste(year, str_pad(month,width=2,side="left",pad="0"),"01",sep="-"))) %>% group_by(territory_sa4, yearmonth)%>% summarise(rain_mean = mean(rainfall), rain_max = max(rainfall), rain_min = min(rainfall), rain_median = median(rainfall))
head(rainfall_sa4_monthyear)


# Standardisation of terriority names in Sync with Unemployment Data
rainfall_sa4_monthyear$territory_sa4 <- as.character(rainfall_sa4_monthyear$territory_sa4)
temperature_sa4_monthyear$territory_sa4 <- as.character(temperature_sa4_monthyear$territory_sa4)
#evap_sa4 %>% filter(str_detect(territory_sa4, "^Hobart"))
unemploy_sa4 <- c("Greater Hobart","New South Wales - Central West","Victoria - North West",
  "Western Australia - Outback (North and South)","Western Australia - Outback (North and South)",
  "Tasmania - South East","Tasmania - West and North West")

boomrang_sa4 <- c("Hobart","Central West","North West","Western Australia - Outback (North)",
  "Western Australia - Outback (South)","South East","West and North West")

load("data/unemployment.RData")
for(i in 1:length(boomrang_sa4)){
  rainfall_sa4_monthyear$territory_sa4[rainfall_sa4_monthyear$territory_sa4 == boomrang_sa4[i]] <- unemploy_sa4[i]
  temperature_sa4_monthyear$territory_sa4[temperature_sa4_monthyear$territory_sa4 == boomrang_sa4[i]] <- unemploy_sa4[i]
}
head(temperature_sa4_monthyear)
head(rainfall_sa4_monthyear)

# Merge with Unemployment Data
head(unemployment)
unemployment_temperature <- unemployment %>% 
  left_join(temperature_sa4_monthyear, by=c("territory_sa4" = "territory_sa4", "date" = "yearmonth"))
unemployment_rainfall <- unemployment %>% 
  left_join(rainfall_sa4_monthyear, by=c("territory_sa4" = "territory_sa4", "date" = "yearmonth"))
View(unemployment_temperature)

# Save the finalised merged Temperature & Rainfall ~ Unemployment data into R
save(unemployment_rainfall, file="data/unemployment_rainfall_boomrang.RData")
save(unemployment_temperature, file="data/unemployment_temperature_boomrang.RData")
