library(ISLR)
library(dlookr)
library(dplyr)
library(tidyverse)

# Load Data
load("data/unemployment_precp.RData")
str(precp_unemployment)
# 10 variables and 787631 obs
describe(precp_unemployment)

group_by_terr <- precp_unemployment %>% 
  group_by(territory_sa4) 
View(group_by_terr)

# Check the count and % of rows with Missing Precp Values
precp_missing_value_percentage = sum(is.na(precp_unemployment$precp))/nrow(precp_unemployment)  # 11151 , 14%
precp_missing_value_percentage
map(precp_unemployment, ~sum(is.na(.))) # precp,stationid,lat,long,elv,stationname - 11151

# What are the terriorities with missing data
terr_with_missing_rainfall_data <- precp_unemployment %>% 
  select(territory_sa4) %>% 
  filter(is.na(precp_unemployment$precp))
View(unique(terr_with_missing_rainfall_data))

terr_with_missing_rainfall_data %>% group_by(territory_sa4) %>% summarise(count=n()) # 54 Unique Terriorites



