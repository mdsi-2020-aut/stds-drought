library("xml2")
library(dplyr)
library(stringr)
library(dplyr)
library(ASGS)
library(ASGS.foyer)

# The source of file http://www.bom.gov.au/waterdata/services?service=SOS
out_obs.xml <- read_xml("out_obs.xml")
#each station observation is separated by OM_Observation
observation <- xml_find_all(out_obs.xml,".//om:OM_Observation")
head(observation)
#initiate data.frame to store time series
waterdata <- data.frame()
for(obs in observation){
  #get station id per observation
  station_url <- xml_attr(xml_find_all(obs, ".//om:featureOfInterest"),"href")
  #parse station_URL to get ID Only
  station_id <- strsplit(station_url,"/")[[1]][7]
  #get observation. each observation separated by MeasurementTVP
  series <- xml_find_all(obs, ".//wml2:MeasurementTVP")
  for(point in series){
    #date stored in wml2:time
    xdate <- xml_text(xml_find_all(point, ".//wml2:time"))
    #date stored in wml2:value
    xvalue <- xml_text(xml_find_all(point, ".//wml2:value"))
    #store it into data.frame
    waterdata <- rbind(waterdata, data.frame(station_url=station_url,
      station_id = station_id,
      date=xdate,
      value = xvalue))
  }
}
out_obs2.xml <- read_xml("out_obs2.xml")
#each station observation is separated by OM_Observation
observation2 <- xml_find_all(out_obs2.xml,".//om:OM_Observation")
#initiate data.frame to store time series
waterdata2 <- data.frame()
for(obs in observation2){
  #get station id per observation
  station_url <- xml_attr(xml_find_all(obs, ".//om:featureOfInterest"),"href")
  #parse station_URL to get ID Only
  station_id <- strsplit(station_url,"/")[[1]][7]
  #get observation. each observation separated by MeasurementTVP
  series <- xml_find_all(obs, ".//wml2:MeasurementTVP")
  for(point in series){
    #date stored in wml2:time
    xdate <- xml_text(xml_find_all(point, ".//wml2:time"))
    #date stored in wml2:value
    xvalue <- xml_text(xml_find_all(point, ".//wml2:value"))
    #store it into data.frame
    waterdata2 <- rbind(waterdata2, data.frame(station_url=station_url,
      station_id = station_id,
      date=xdate,
      value = xvalue))
  }
}

nrow(waterdata) # 98817
nrow(waterdata2) # 10411
waterdata <- rbind(waterdata, waterdata2)
nrow(waterdata) # 150872
save(waterdata, file="data/HPT/org_waterdata.RData")

# Load the downloaded Water level csv and Station list csv

# waterlevel <- read.csv("df.waterlevel.csv")
# dam_stationlist <- read.csv("Station_lat_lng.csv")
# nrow(waterlevel) # 109228

waterlevel <- read.csv("waterdata.csv")
dam_stationlist <- read.csv("dam_station_list.csv")
nrow(waterlevel) # 109228
nrow(dam_stationlist) #572
# Check the count of each stationId 
dam_list_check <- dam_stationlist %>% 
  group_by(station_id) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
dam_list_check

unique_station_id_list <- dam_stationlist %>% 
  distinct(station_id, .keep_all = TRUE)
nrow(unique_station_id_list) # 561
head(unique_station_id_list)
str(unique_station_id_list)

# Join Water Data and Station Data
water_stn <- waterlevel %>% 
 left_join(unique_station_id_list, by= c("station_id" = "station_id")) %>% 
  mutate(date = as.Date(date, '%Y-%m-%d'))
nrow(water_stn) # 109228
head(water_stn)

water_stn$station_url <- NULL
head(water_stn)
water_stn$lng.x <- NULL

# Merge with SA4 Data
water_stn$territory_sa4 <- ASGS::latlon2SA(water_stn$lat.y, water_stn$lng.y, to = "SA4", yr = "2016")
water_sa4 <- water_stn
head(water_stn)

#  Renaming the terriorities
water_sa4$territory_sa4 <- as.character(water_sa4$territory_sa4)
# water_sa4 %>% filter(str_detect(territory_sa4, "Grater Hobart"))

unemploy_sa4 <- c("Greater Hobart","New South Wales - Central West","Victoria - North West",
  "Western Australia - Outback (North and South)","Western Australia - Outback (North and South)",
  "Tasmania - South East","Tasmania - West and North West")

waterlevel_sa4 <- c("Hobart","Central West","North West","Western Australia - Outback (North)",
  "Western Australia - Outback (South)","South East","West and North West")
str(watert)

# Renaming
for(i in 1:length(water_sa4)){
  water_sa4$territory_sa4[water_sa4$territory_sa4 == waterlevel_sa4[i]] <- unemploy_sa4[i]
}

# Save water_sa4
nrow(water_sa4) # 109228
save(water_sa4, file="data/HPT/water_sa4.RData")

# Before merging with Unemployment
# Check are there any terriorities which has more than one station
head(water_sa4)
unique(water_sa4$station_id) # 600 stations
terr_damstn_count <- water_sa4 %>% 
  select(territory_sa4, station_id) %>% 
  distinct() %>% 
  group_by(territory_sa4) %>%
  summarise(damstn_count = n()) %>%
  arrange(desc(damstn_count)) 
nrow(terr_damstn_count) # 60 Terr
head(terr_damstn_count)
# Get SA4 list
pure_sa4_list <- unemployment %>% 
  select(territory_sa4) %>% 
  distinct()
pure_sa4_list # 87 
# Check the missing 
lookup_missing_terr_damstn <- pure_sa4_list %>% 
  left_join(terr_damstn_count, by=c("territory_sa4"= "territory_sa4"))
View(lookup_missing_terr_damstn)
write.csv(lookup_missing_terr_damstn, file="data/HPT/dam_stn_count_by_terriority.csv")


# Going to merge with Unemployment Data
load("data/unemployment.RData")
# Identidied the extra space at the end of unemployment$terriority_sa2 and Trimming it
# "Darwin " in unemploymnet, "Darwin" in water_sa4
# unique(water_sa4$territory_sa4) # unique(unemployment$territory_sa4)
unemployment$territory_sa4 <- str_trim(unemployment$territory_sa4, side="both")


# Actual Merging with Unemployment data using left_join
water_unemployment <- unemployment %>% 
  left_join(water_sa4, by=c("territory_sa4" = "territory_sa4", "date" = "date"))
head(water_unemployment)

# Aggreate by SA4 and date for Average Water Level and Unemployment Rate
water_unemployment <- water_unemployment %>% 
  group_by(territory_sa4, date) %>% 
  summarise(waterlevel_mean = mean(value), unemployment_rate = mean(unemployment_rate))

# Save the finalised merged data
save(water_unemployment, file="data/unemployment_water.RData")

# nrow(water_unemployment) # 21489
# View(water_unemployment)

# Check missing data count by terriority and period
View(water_unemployment)
missing_check_water_unemp <- water_unemployment %>% 
  filter(is.na(waterlevel_mean)) %>% 
  group_by(territory_sa4) %>% 
  summarise(missing_count = n(), max_date= max(date), min_date=min(date))
write.csv(missing_check_water_unemp, file="data/HPT/missing_check_water_unemp.csv")
