library(stringr)
library(dplyr)
library(ASGS)
library(ASGS.foyer)

# BOM Data source  Monthly Rainfall - ftp://ftp.bom.gov.au/anon/home/ncc/www/change/HQmonthlyR/ 
# Save the unzipped file into your local according to the path mentioned below
bom_precip_monthly_list <- list.files("HQ_monthly_prcp_txt/", pattern="*.month.txt$")
bom_station_list <- read.delim(paste("HQ_monthly_prcp_txt", "HQMR_stations.txt", sep="/"), sep=" ", header=F, col.names=c("station_id","lat","long","elv", "name1","name2", "name3"))

bom_station_list <- bom_station_list %>% 
  mutate(station_name = paste(name1,name2,name3))

bom_station_list$name1 <- NULL
bom_station_list$name2 <- NULL
bom_station_list$name3 <- NULL

head(bom_station_list)

precipitation_df = data.frame()
for (filename in bom_precip_monthly_list) {
  
  ## Getting Station Id from the first line of the text file
  first_line <- readLines(paste("HQ_monthly_prcp_txt", filename, sep="/"),n=1)
  print(first_line)
  firstline_data <- str_split(first_line, pattern=" ")
  stationid = firstline_data[[1]][8]
  print(stationid)
  
  ## Read the text file of prcp into a Data frame
  onestation_df <- read.table(paste("HQ_monthly_prcp_txt", filename, sep="/"), skip=1)
  # Add a column of stationId into the df
  onestation_df$stationid <- stationid
  
  # Append to the main df
  precipitation_df <- rbind(precipitation_df,onestation_df)
 
}

glimpse(precipitation_df)
glimpse(bom_station_list)

# Join with station Table
precp_stn <- precipitation_df %>% 
  inner_join(bom_station_list, by= c("stationid" = "station_id"))
head(precp_stn)

# Reformat the date (V1, V2) from strin to "1999-09-01" in Sync wiht Unemploymnet date
load("data/unemployment.RData")
 glimpse(unemployment)
colnames(precp_stn) <- c("from", "to", "precp", "stationid", "lat","long", "elv","stationname")
precp_stn <- precp_stn %>% 
  mutate(from = as.Date(as.character(from), '%Y%m%d')) %>% 
  mutate(to = as.Date(as.character(to), '%Y%m%d'))

class(precp_stn$from)
#filter out the records prior to 1990
precp_stn <- precp_stn %>%
  filter(from >= as.Date("1996-01-01") & to >= as.Date("1996-01-31"))
nrow(precp_stn)

# Merge with SA4 Data started at 11:58am
precp_stn$territory_sa4 <- ASGS::latlon2SA(precp_stn$lat, precp_stn$long, to = "SA4", yr = "2016")
#precp_stn_tmp<-precp_stn
precp_sa4 <- precp_stn

# name Standardisation"
precp_sa4$territory_sa4 <- as.character(precp_sa4$territory_sa4)
precp_stn %>% filter(str_detect(territory_sa4, "^Hobart"))
unemploy_sa4 <- c("Greater Hobart","New South Wales - Central West","Victoria - North West",
  "Western Australia - Outback (North and South)","Western Australia - Outback (North and South)",
  "Tasmania - South East","Tasmania - West and North West")

rainfall_sa4 <- c("Hobart","Central West","North West","Western Australia - Outback (North)",
  "Western Australia - Outback (South)","South East","West and North West")

# Merging with Unemployment
for(i in 1:length(rainfall_sa4)){
  precp_sa4$territory_sa4[precp_sa4$territory_sa4 == rainfall_sa4[i]] <- unemploy_sa4[i]
}
head(precp_sa4,20)

# Merge with Unemployment Data
precp_unemployment <- unemployment %>% 
  left_join(precp_sa4, by=c("territory_sa4" = "territory_sa4", "date" = "from"))
save(precp_unemployment, file="data/bom_precp.RData")

# Extra Checking - Optional
head(precp_unemployment)
precp_sa4 %>% filter(str_detect(territory_sa4,"^Australian Capital Territory"))



# print(ASGS::latlon2SA(-15.65, 128.71, to = "SA2", yr = "2016"))
# ?latlon2SA

# Merge with Unemploymnet
# x <- str_split("PRCP       099005 18990301 20200430 missing_value=99999.9 FLINDERS ISLAND AIRPORT    ", pattern=" ")
# print(x)
# print( x[[1]][8])
# print(x[[1]][12])


