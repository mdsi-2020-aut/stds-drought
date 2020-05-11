library(stringr)
library(dplyr)
library(ASGS)
library(ASGS.foyer)

# BOM Data source  Monthly Rainfall - ftp://ftp.bom.gov.au/anon/home/ncc/www/change/HQmonthlyR/ 
# Save the unzipped file into your local according to the path mentioned below
# The unzipped file contains a file of weather station list and multiple files of Precipitation data for monthly/annual/seasonal
# The name of weather station list file is "HQMR_stations.txt"

# Load the text files of monthly Precipitation data
bom_precip_monthly_list <- list.files("HQ_monthly_prcp_txt/", pattern="*.month.txt$")
# Read the text file of weather station list (space delimited file)
bom_station_list <- read.delim(paste("HQ_monthly_prcp_txt", "HQMR_stations.txt", sep="/"), sep=" ", header=F, col.names=c("station_id","lat","long","elv", "name1","name2", "name3"))

# Concantenate the station names spreaded out in three different columns into one
bom_station_list <- bom_station_list %>% 
  mutate(station_name = paste(name1,name2,name3))
# Discard the three columns of names, not longer need to use in future
bom_station_list$name1 <- NULL
bom_station_list$name2 <- NULL
bom_station_list$name3 <- NULL

# Precipitation Data Reading
precipitation_df = data.frame()
for (filename in bom_precip_monthly_list) {
  
  ## Getting Station Id from the first line of the text file
  first_line <- readLines(paste("HQ_monthly_prcp_txt", filename, sep="/"),n=1)
  firstline_data <- str_split(first_line, pattern=" ")
  stationid = firstline_data[[1]][8]
  
  ## Read the text file of Precipitation into a Dataframe
  onestation_df <- read.table(paste("HQ_monthly_prcp_txt", filename, sep="/"), skip=1)
  # Add a column of stationId into the df
  onestation_df$stationid <- stationid
  
  # Append to the main df
  precipitation_df <- rbind(precipitation_df,onestation_df)
 
}
save(precipitation_df, file="data/HPT/org_precp.RData")
sum(is.na(precipitation_df$V3)) 
precp_missing_value_percentage = sum(is.na(precipitation_df$V3))/nrow(precipitation_df)
precp_missing_value_percentage # 0 m,v
nrow(precipitation_df) #441154 obs

# glimpse(bom_station_list)
# class(precipitation_df$stationid)
# class(bom_station_list$station_id)
# bom_station_list$station_id is factor and precipitation_df$stationid is character
bom_station_list$station_id <- as.character(bom_station_list$station_id)
# Join with station Table
precp_stn <- precipitation_df %>% 
  inner_join(bom_station_list, by= c("stationid" = "station_id"))
nrow(precp_stn) #441154 rows


# Reformat the date (V1, V2) from string to "1999-09-01" in Sync wiht Unemploymnet Data
colnames(precp_stn) <- c("from", "to", "precp", "stationid", "lat","long", "elv","stationname")
precp_stn <- precp_stn %>% 
  mutate(from = as.Date(as.character(from), '%Y%m%d')) %>% 
  mutate(to = as.Date(as.character(to), '%Y%m%d'))
nrow(precp_stn) #441154 rows

# Filter out the records prior to 1990
precp_stn <- precp_stn %>%
  filter(from >= as.Date("1996-01-01") & to >= as.Date("1996-01-31"))
head(precp_stn) #8206

save(precp_stn, file="data/HPT/bom_precp_stn_org.RData")

# Merge with SA4 Data
precp_stn$territory_sa4 <- ASGS::latlon2SA(precp_stn$lat, precp_stn$long, to = "SA4", yr = "2016")

# precp_stn_tmp<-precp_stn
precp_sa4 <- precp_stn
nrow(precp_sa4) #82026

# Standardisation of terriority names with Unemployment Data
precp_sa4$territory_sa4 <- as.character(precp_sa4$territory_sa4)
#precp_stn %>% filter(str_detect(territory_sa4, "^Hobart"))
unemploy_sa4 <- c("Greater Hobart","New South Wales - Central West","Victoria - North West",
  "Western Australia - Outback (North and South)","Western Australia - Outback (North and South)",
  "Tasmania - South East","Tasmania - West and North West")

rainfall_sa4 <- c("Hobart","Central West","North West","Western Australia - Outback (North)",
  "Western Australia - Outback (South)","South East","West and North West")

# Renaming
for(i in 1:length(rainfall_sa4)){
  precp_sa4$territory_sa4[precp_sa4$territory_sa4 == rainfall_sa4[i]] <- unemploy_sa4[i]
}
head(precp_sa4) #82026
sum(is.na(precp_sa4$precp)) # zero missing 
unique(precp_sa4$territory_sa4) 
unique(unemployment$territory_sa4)
unemployment$territory_sa4 <- str_trim(unemployment$territory_sa4, side="both")
save("precp_sa4", file="data/precp_sa4.RData")


# To Check are there any terriorities which has more than one station, count stn Id by terriority
head(precp_sa4)
unique(precp_sa4$stationid) # 307 stations
terr_precp_stn_count <- precp_sa4 %>% 
  select(territory_sa4, stationid) %>% 
  distinct() %>% 
  group_by(territory_sa4) %>%
  summarise(precp_stn_count = n()) %>%
  arrange(desc(precp_stn_count)) 
nrow(terr_precp_stn_count) # 45 Terr have more than one station
View(terr_precp_stn_count)
# Get SA4 list
pure_sa4_list <- unemployment %>% 
  select(territory_sa4) %>% 
  distinct()
pure_sa4_list # 87 
# Check the count of station in each terriority and identify which terriorities have no station 
lookup_missing_terr_precpstn <- pure_sa4_list %>% 
  left_join(terr_precp_stn_count, by=c("territory_sa4"= "territory_sa4"))
View(lookup_missing_terr_precpstn)
write.csv(lookup_missing_terr_precpstn, file="data/HPT/precp_stn_count_by_terriority.csv")


# Merge with Unemployment Data
load("data/unemployment.RData")
precp_unemployment <- unemployment %>% 
  left_join(precp_sa4, by=c("territory_sa4" = "territory_sa4", "date" = "from"))
nrow(precp_unemployment)# 78631
head(precp_unemployment)

# Aggreate by SA4 and date for Average Precipitation and Unemployment Rate
precp_unemployment <- precp_unemployment %>% 
  group_by(territory_sa4, date) %>% 
  summarise(precp_mean = mean(precp), unemployment_rate = mean(unemployment_rate))
nrow(precp_unemployment) # 

save(precp_unemployment, file="data/unemployment_precp.RData")
sum(is.na(precp_unemployment$precp_mean)) # 1151


# Extra Checking - Optional
View(precp_unemployment)
precp_sa4 %>% filter(str_detect(territory_sa4,"^Australian Capital Territory"))
tail(unemployment)

# Check missing data count by terriority and period
View(precp_unemployment)
missing_check_precp_unemp <- precp_unemployment %>% 
  filter(is.na(precp_mean)) %>% 
  group_by(territory_sa4) %>% 
  summarise(missing_count = n(), max_date= max(date), min_date=min(date))
write.csv(missing_check_precp_unemp, file="data/HPT/missing_check_precp_unemp.csv")


