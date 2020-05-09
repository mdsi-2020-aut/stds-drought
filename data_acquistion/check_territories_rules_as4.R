### Get data directly from the ABS (Australian Bureau of Statistics) directory
# NOTE: package tutorial in https://cran.r-project.org/web/packages/raustats/vignettes/raustats_introduction.html 

#---------------------------------------------------------------------------------------------------------------------------------------------------#
###### R Environment - Load Data ######
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#### ENVIRONMENT ####

### Clean current environment and set the working directory
rm(list = ls())
setwd('~/UTS/STDS/AT2/stds-drought')

### Install and/or load libraries to be used
#install.packages(c('raustats','ggplot2','dplyr','stringr','scales','ASGS') #To install all required libraries
library(raustats)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ASGS)

#### GET DATA ####

### Download table
# Catalogue: 6291.0.55.001 - Labour Force, Australia, Detailed - Electronic Delivery, Mar 2020
# Table: Table 16b. Labour force status by Labour market region (ASGS) and Sex, Annual averages of the previous 12 months
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6291.0.55.001Mar%202020?OpenDocument
labour_abs <- abs_cat_stats("6291.0.55.001", tables="Table.+16b\\D")


#---------------------------------------------------------------------------------------------------------------------------------------------------#
###### unemployment with required information ######
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#### FILTER DATA ####

### Unemployment totals
unemployment_ini <- labour_abs %>% 
  filter(grepl("  Employed total ;  Persons", data_item_description, ignore.case=TRUE)) 


### SA4 totals
unemployment <- unemployment_ini %>% 
  select(date,value,data_item_description) %>% 
  rename(unemployment_rate = value) %>% 
  filter(grepl(">>>|Australian|Hobart", data_item_description, ignore.case=TRUE)) %>% # SA4 geographical divisions can be identified by ">>>"
  mutate(territory_sa4 = str_replace_all(word(data_item_description,1,sep = "\\;"),'>>> |>> |> ',''))

### Territories list
uniques_sa4 <- unemployment %>% select(data_item_description,territory_sa4) %>% unique()
uniques_sa4$territory_sa4
nrow(uniques_sa4)

### Aggregated
unemployment_add_up <- unemployment %>% 
  select(date, unemployment_rate,territory_sa4) %>% unique() %>% 
  group_by(date) %>% summarise(total = sum(unemployment_rate))

###AUSTRALIA TOTAL
unemployment_raw_au <- unemployment_ini %>% 
  select(date,value,data_item_description) %>% 
  rename(unemployment_rate = value) %>% 
  filter(grepl("^Australia ;", data_item_description, ignore.case=TRUE)) %>% # SA4 geographical divisions can be identified by ">>>"
  mutate(territory_sa4 = str_replace_all(word(data_item_description,1,sep = "\\;"),'>>> |>> |> ',''))

### Compare with aggregation
check_territories <- unemployment_raw_au %>% select(date,unemployment_rate) %>% 
  mutate(unemployment_real = round(unemployment_rate)) %>% 
  left_join(unemployment_add_up, by='date') %>% 
  mutate(unemployment_add_up = round(total),
         check_difference = (abs(unemployment_real - unemployment_add_up) > 1 )) %>% 
  select(date, unemployment_real, unemployment_add_up, check_difference)# %>% 
  #filter(check_difference == TRUE)
