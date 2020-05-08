library(stringr)
library(dplyr)
library(ASGS)
library(ASGS.foyer)

# BOM Data source ftp://ftp.bom.gov.au/anon/home/ncc/www/change/HQmonthlyE/ 
# Save the unzipped file into your local according to the path mentioned below
# The unzipped file contains a file of weather station list and multiple files of Evap data for monthly/annual/seasonal
# The name of weather station list file is "HQME_stations.txt"

# Load the text files of monthly Evaporation data
bom_evap_monthly_list <- list.files("HQ_monthly_evap_txt/", pattern="*.month.txt$")
# Read the text file of weather station list (space delimited file)
bom_evap_stn_list <- read.delim(paste("HQ_monthly_evap_txt", "HQME_stations", sep="/"), sep=" ", header=F, col.names=c("station_id","lat","long","elv", "name1","name2", "name3"))
head(bom_evap_stn_list)
# Concantenate the station names spreaded out in three different columns into one
bom_evap_stn_list <- bom_evap_stn_list %>% 
  mutate(station_name = paste(name1,name2,name3))
# Discard the three columns of names, not longer need to use in future
bom_evap_stn_list$name1 <- NULL
bom_evap_stn_list$name2 <- NULL
bom_evap_stn_list$name3 <- NULL
head(bom_evap_stn_list)

# Evaporation Data Reading
evap_df = data.frame()
for (filename in bom_evap_monthly_list) {
  
  ## Getting Station Id from the first line of the text file
  first_line <- readLines(paste("HQ_monthly_evap_txt", filename, sep="/"),n=1)
  firstline_data <- str_split(first_line, pattern=" ")
  stationid = firstline_data[[1]][8]

  # Read the text file of Evap into a Data frame
  onestation_df <- read.table(paste("HQ_monthly_evap_txt", filename, sep="/"), skip=1)
  # Add a column of stationId into the df
  onestation_df$stationid <- stationid
  
  # Append to the main df
  evap_df <- rbind(evap_df,onestation_df)
}
nrow(evap_df) #33122

# bom_evap_stn_list$station_id is factor and precipitation_df$stationid is character
bom_evap_stn_list$station_id <- as.character(bom_evap_stn_list$station_id)
# Join Evaporation Dataframe with station Dataframe by stationid
evap_stn <- evap_df %>% 
  inner_join(bom_evap_stn_list, by= c("stationid" = "station_id"))
head(evap_stn)

# Reformat the date (V1, V2) from strin to "1999-09-01" in Sync with date in Unemployment Data
colnames(evap_stn) <- c("from", "to", "evap", "stationid", "lat","long", "elv","stationname")

#filter out the records prior to 1990
evap_stn <- evap_stn %>% 
  mutate(from = as.Date(as.character(from), '%Y%m%d')) %>% 
  mutate(to = as.Date(as.character(to), '%Y%m%d'))
head(evap_stn)

#filter out the records prior to 1990
evap_stn <- evap_stn %>%
  filter(from >= as.Date("1996-01-01") & to >= as.Date("1996-01-31"))
nrow(evap_stn) #14337

# Merge with SA4 Data started
evap_stn$territory_sa4 <- ASGS::latlon2SA(evap_stn$lat, evap_stn$long, to = "SA4", yr = "2016")
evap_sa4 <- evap_stn
nrow(evap_stn) # 14337

# Standardisation of terriority names in Sync with Unemployment Data
evap_sa4$territory_sa4 <- as.character(evap_sa4$territory_sa4)
#evap_sa4 %>% filter(str_detect(territory_sa4, "^Hobart"))
unemploy_sa4 <- c("Greater Hobart","New South Wales - Central West","Victoria - North West",
  "Western Australia - Outback (North and South)","Western Australia - Outback (North and South)",
  "Tasmania - South East","Tasmania - West and North West")

evaporation_sa4 <- c("Hobart","Central West","North West","Western Australia - Outback (North)",
  "Western Australia - Outback (South)","South East","West and North West")

load("data/unemployment.RData")
for(i in 1:length(evaporation_sa4)){
  evap_sa4$territory_sa4[evap_sa4$territory_sa4 == evaporation_sa4[i]] <- unemploy_sa4[i]
}
nrow(evap_sa4) # 14337

# Merge with Unemployment Data
evap_unemployment <- unemployment %>% 
  left_join(evap_sa4, by=c("territory_sa4" = "territory_sa4", "date" = "from"))
nrow(evap_unemployment) # 21489

# Save the finalised merged Evaporation ~ Unemployment data into R
save(evap_unemployment, file="data/unemployment_evap.RData")

# Extra Checking - Optional
nrow(evap_unemployment) # 21489
precp_sa4 %>% filter(str_detect(territory_sa4,"^Australian Capital Territory"))


