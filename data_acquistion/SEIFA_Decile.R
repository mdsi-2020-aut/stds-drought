library(tidyverse)
library(raustats)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(Amelia)

load("data/post_sa2_sa4.RData")

#where you want to save the datasets (raw)
dir = "data"


#SEFIA 2006 - 2016 getting the data

tab2016 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = 'latest')
url_2016 <- tab2016$path_xls[5] 
file2016 <- abs_cat_download(url_2016,dir)
seifa2016 <- read_xls(file2016,sheet = 'Table 1',skip =4)

tab2011 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '2011')
url_2011 <- tab2011$path_xls[6] 
file2011 <- abs_cat_download(url_2011,dir)
seifa2011 <- read_xls(file2011,sheet = 'Table 1',skip =4)

tab2006 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '2006')

url_2006 <- tab2006$path_xls[12] 
file2006 <- abs_cat_download(url_2006,dir)
seifa2006 <- read_xls(file2006,sheet = 'Table 1',skip =4)

save(seifa2006, file="data/seifa2006.RData")

skim(seifa2006)

#TIDY DATA
#cleaning names of the dataset
seifa2006 <- seifa2006[-1,] 

tidy_seifa2006 <- seifa2006 %>% 
  clean_names() %>% 
  rename(IRSAD_Decile = x3, IRSD_Decile = x5, IER_Decile = x7, IEO_Decile = x9, POA = x2006_postal_area_code_poa, Population = usual_resident_population) %>% 
  mutate(POA = as.numeric(POA)) %>% 
  mutate(IRSAD_Decile = as.numeric(IRSAD_Decile)) %>% 
  mutate(IRSD_Decile = as.numeric(IRSD_Decile)) %>% 
  mutate(IER_Decile = as.numeric(IER_Decile)) %>% 
  mutate(IEO_Decile = as.numeric(IEO_Decile)) %>% 
  select(c(IRSAD_Decile,IRSD_Decile,IER_Decile,IEO_Decile,Population,POA)) %>%
  mutate(Year = 2006)
tidy_seifa2006 <- tidy_seifa2006[complete.cases(tidy_seifa2006),] #get rid of NA
  

seifa2011 <- seifa2011[-1,]

tidy_seifa2011 <- seifa2011 %>%  
  clean_names() %>% 
  rename(IRSAD_Decile = x3, IRSD_Decile = x5, IER_Decile = x7, IEO_Decile = x9, POA = x2011_postal_area_code_poa, Population = usual_resident_population) %>% 
  mutate(POA = as.numeric(POA)) %>% 
  mutate(IRSAD_Decile = as.numeric(IRSAD_Decile)) %>% 
  mutate(IRSD_Decile = as.numeric(IRSD_Decile)) %>% 
  mutate(IER_Decile = as.numeric(IER_Decile)) %>% 
  mutate(IEO_Decile = as.numeric(IEO_Decile)) %>% 
  select(c(IRSAD_Decile,IRSD_Decile,IER_Decile,IEO_Decile,Population,POA)) %>%
  mutate(Year = 2011)
tidy_seifa2011 <- tidy_seifa2011[complete.cases(tidy_seifa2011),] #get rid of NA

seifa2016 <- seifa2016[-1,]

tidy_seifa2016 <- seifa2016 %>% 
  clean_names() %>% 
  rename(IRSD_Decile = x3, IRSAD_Decile = x5, IER_Decile = x7, IEO_Decile = x9, POA= x2016_postal_area_poa_code, Population = usual_resident_population) %>% 
  mutate(POA = as.numeric(POA)) %>% 
  mutate(IRSAD_Decile = as.numeric(IRSAD_Decile)) %>% 
  mutate(IRSD_Decile = as.numeric(IRSD_Decile)) %>% 
  mutate(IER_Decile = as.numeric(IER_Decile)) %>% 
  mutate(IEO_Decile = as.numeric(IEO_Decile)) %>% 
  select(c(IRSAD_Decile,IRSD_Decile,IER_Decile,IEO_Decile,Population,POA)) %>%
  mutate(Year = 2016)
tidy_seifa2016 <- tidy_seifa2016[complete.cases(tidy_seifa2016),] #get rid of NA

tidy_seifa_list <- list(tidy_seifa2006,tidy_seifa2011,tidy_seifa2016)
tidy_seifa <- NULL
#join to post_sa4 df and get weighted averages of POA to SA4

for(tidy in tidy_seifa_list){
  mapped_df <- inner_join(tidy,post_sa2_sa4,by = c("POA" = "POSTCODE")) %>% 
    mutate(Weight = Population) %>% 
    group_by(SA4_NAME_2016) %>%
# make the value weighted mean of each SA4
    mutate(Weight = Weight/sum(Weight)) %>% 
    mutate(IRSD_Decile = weighted.mean(IRSD_Decile,Weight)) %>% 
    mutate(IRSAD_Decile = weighted.mean(IRSAD_Decile,Weight)) %>% 
    mutate(IER_Decile = weighted.mean(IER_Decile,Weight)) %>% 
    mutate(IEO_Decile = weighted.mean(IEO_Decile,Weight))  %>%
    mutate(Population = sum(Population)) %>% 
    select(-c(Weight,POA,SA2_MAINCODE)) %>% 
    distinct()
  
#Create SEIFA dataset of 1006-2016 data by binding rows together
  tidy_seifa <- bind_rows(tidy_seifa,mapped_df)
}
#Remove Chrismas Island, Cocos Islands, Jervis Bay,Norfolk Island
tidy_seifa <-tidy_seifa %>% filter(SA4_NAME_2016 != "Other Territories") 

load("data/unemployment.RData")

#Add last census date to data frame
unemployment$territory_sa4 = trimws(unemployment$territory_sa4)
unemployment$year = year(unemployment$date)
unemployment$census  = (5*floor((unemployment$year-1)/5))+1
unemployment$year = NULL

#join by SA4 & Census
unemploy_seifa <- inner_join(tidy_seifa,unemployment,by = c("SA4_NAME_2016" = "territory_sa4","Year" = "census"))
save(unemploy_seifa, file="data/unemploy_seifa.RData")

load("data/unemploy_seifa.RData")