library(ISLR)
library(dlookr)
library(dplyr)
library(tidyverse)

# Load Data
load("data/unemployment_evap.RData")
str(evap_unemployment) # 27488 obs 10 variable
describe(evap_unemployment) # will skewness useful ??
View(evap_unemployment)
# Check the count and % of rows with Missing Evaporation Values
sum(is.na(evap_unemployment$evap)) # 15791
evap_missing_value_percentage = sum(is.na(evap_unemployment$evap))/nrow(evap_unemployment)  # 15791 , 57%
evap_missing_value_percentage
map(evap_unemployment, ~ sum(is.na(.))) # evap,stationid,lat,long,elv,stationname - 15791

# What are the terriorities with missing data
terr_with_missing_evap_data <- evap_unemployment %>% 
  select(territory_sa4) %>% 
  filter(is.na(evap_unemployment$evap))
View(unique(terr_with_missing_evap_data))

terr_with_missing_evap_data %>% group_by(territory_sa4) %>% summarise(count=n()) # 87 Unique Terriorites


