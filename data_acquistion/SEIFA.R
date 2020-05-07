library(tidyverse)
library(raustats)
library(lubridate)

load("data/post_sa2_sa4.RData")

#where you want to save the datasets (raw)
dir = "data"


#SEFIA 1986 - 2016
#the method of storage for all of them is diffrent for some bizzare reason

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

tab2001 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '2001')
url_2001 <- tab2001$path_zip
folder2001<-abs_cat_unzip(abs_cat_download(url_2001,dir),dir)
seifa2001 <- read_xls(folder2001[5,1])

tab1996 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '1996')
url_1996 <- tab1996$path_zip
folder1996<-abs_cat_unzip(abs_cat_download(url_1996,dir),dir)

seifa1996 <- NULL
for(i in 1:(length(folder1996)-1)){
  xls<-read_xls(folder1996[i])
  seifa1996 <- bind_rows(seifa1996,xls)
}

# 1986 and 1991 SEIFA data not used
tab1991 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '1991')
url_1991 <- tab1991$path_zip
folder1991<-abs_cat_unzip(abs_cat_download(url_1991,dir),dir)
seifa1991 <- read_xls(folder1991[1])

tab1986 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '1986')
url_1986 <- tab1986$path_zip
folder1986<-abs_cat_unzip(abs_cat_download(url_1986,dir),dir)

seifa1986 <- read_xls(folder1986[1])

save(seifa1986,seifa1991,seifa1996,seifa2001,seifa2006, file="data/seifa1986_06.RData")

#Tidy seifa1996
#Removed unwanted column: CD, State, SD, SSD, LGA, SLA
# Note this was the only year in our collection that also didn't have the weighted mean already grouped by POA
#Kept columns: 
# Postcode
# SEIFA population
# Index of Socio-Econmic Advantage and Disadvantage
# Index of Relative Socio-Economic Disadvantage
# Index of Economic Resources
# Index of Education & OCcupation

tidy_seifa_1996 <- seifa1996 %>%  mutate(`Index of Relative Socio-Economic Advantage and Disadvantage` =  
                                       if_else(is.na(`Urban Index of Relative Socio-Economic Advantage`),
                                               `Rural Index of Relative Socio-Economic Advantage`,
                                               `Urban Index of Relative Socio-Economic Advantage`)) %>%
                                                #Combined from two mutually exclusive columns "Rural" and 
                                                #"Urban" Indexes of Relative Socio-Economic Advantage into one
                                                #Only occurs in seifa1996
                                  mutate(Population = `SEIFA96 Population`) %>%
                                  select(c(`Index of Relative Socio-Economic Advantage and Disadvantage`,
                                         `Index of Relative Socio-Economic Disadvantage`,
                                         `Index of Economic Resources`, `Index of Education & Occupation`,
                                          Population,POA)) %>% 
                                  mutate(Year = 1996) %>% 
                                  mutate(Weight = Population) %>% 
                                  group_by(POA) %>%
                                  # make the value weighted mean of each POA
                                  mutate(Weight = Weight/sum(Weight)) %>% 
                                  mutate(`Index of Relative Socio-Economic Advantage and Disadvantage` = 
                                           weighted.mean(`Index of Relative Socio-Economic Advantage and Disadvantage`,Weight)) %>% 
                                  mutate(`Index of Relative Socio-Economic Disadvantage` = 
                                           weighted.mean(`Index of Relative Socio-Economic Disadvantage`,Weight)) %>% 
                                  mutate(`Index of Economic Resources` = 
                                           weighted.mean(`Index of Economic Resources`,Weight)) %>% 
                                  mutate(`Index of Education & Occupation` = 
                                           weighted.mean(`Index of Education & Occupation`,Weight))  %>%
                                  mutate(Population = sum(Population)) %>% 
                                  select(-c(Weight)) %>% 
                                  distinct()

#Tidy seifa2001
#Removed:
#Postal_Area_Name
#State_Code
#State_Name
#All Q, Min, Max and Med data (total 39 removed cols)

#Kept:
# Postal_Area_Code
# SEIFA_2001_Population
# Advantage_Disadvantage
# Disadvantage
# Econ_Resource
# Educ_Occupation

tidy_seifa_2001 <- seifa2001 %>%  mutate(Population = `SEIFA_2001_Population`) %>%
                                  mutate(POA = `Postal_Area_Code`) %>%
                                  mutate(`Index of Education & Occupation` = 
                                           `Educ_Occupation`) %>%
                                  mutate(`Index of Economic Resources` = 
                                           `Econ_Resource`) %>%                      
                                  mutate(`Index of Relative Socio-Economic Disadvantage` = 
                                           `Disadvantage`) %>%
                                  mutate(`Index of Relative Socio-Economic Advantage and Disadvantage` = 
                                           `Advantage_Disadvantage`) %>%    
                                  select(c(`Index of Relative Socio-Economic Advantage and Disadvantage`,
                                           `Index of Relative Socio-Economic Disadvantage`,
                                           `Index of Economic Resources`, `Index of Education & Occupation`,
                                           Population,POA)) %>% 
                                    mutate(Year = 2001)

tidy_seifa_2006 <- seifa2006[-1,] %>% mutate(Population = `Usual Resident Population`) %>%
                                      mutate(POA = as.numeric(`2006 Postal Area code (POA)`)) %>%
                                      mutate(`Index of Education & Occupation` = 
                                               as.numeric(`Index of Education and Occupation`)) %>%
                                      mutate(`Index of Economic Resources` = 
                                               as.numeric(`Index of Economic Resources`)) %>%
                                      mutate(`Index of Relative Socio-Economic Disadvantage` = 
                                               as.numeric(`Index of Relative Socio-economic Disadvantage`)) %>%
                                      mutate(`Index of Relative Socio-Economic Advantage and Disadvantage` = 
                                               as.numeric(`Index of Relative Socio-economic Advantage and Disadvantage`)) %>%
                                      select(c(`Index of Relative Socio-Economic Advantage and Disadvantage`,
                                               `Index of Relative Socio-Economic Disadvantage`,
                                               `Index of Economic Resources`, `Index of Education & Occupation`,
                                               Population,POA)) %>% 
                                      mutate(Year = 2006)
tidy_seifa_2006 <- tidy_seifa_2006[complete.cases(tidy_seifa_2006),]

tidy_seifa_2011 <- seifa2011[-1,] %>% mutate(Population = `Usual Resident Population`) %>%
                                      mutate(POA = as.numeric(`2011 Postal Area Code (POA)`)) %>%
                                      mutate(`Index of Education & Occupation` = 
                                               as.numeric(`Index of Education and Occupation`)) %>%
                                      mutate(`Index of Economic Resources` = 
                                               as.numeric(`Index of Economic Resources`)) %>%
                                      mutate(`Index of Relative Socio-Economic Disadvantage` = 
                                               as.numeric(`Index of Relative Socio-economic Disadvantage`)) %>%
                                      mutate(`Index of Relative Socio-Economic Advantage and Disadvantage` = 
                                               as.numeric(`Index of Relative Socio-economic Advantage and Disadvantage`)) %>%
                                      select(c(`Index of Relative Socio-Economic Advantage and Disadvantage`,
                                               `Index of Relative Socio-Economic Disadvantage`,
                                               `Index of Economic Resources`, `Index of Education & Occupation`,
                                               Population,POA)) %>% 
                                      mutate(Year = 2011)
tidy_seifa_2011 <- tidy_seifa_2011[complete.cases(tidy_seifa_2011),]

tidy_seifa_2016 <- seifa2016[-1,] %>% mutate(Population = `Usual Resident Population`) %>%
                                      mutate(POA = as.numeric(`2016 Postal Area (POA) Code`)) %>%
                                      mutate(`Index of Education & Occupation` = 
                                               as.numeric(`Index of Education and Occupation`)) %>%
                                      mutate(`Index of Economic Resources` = 
                                               as.numeric(`Index of Economic Resources`)) %>%
                                      mutate(`Index of Relative Socio-Economic Disadvantage` = 
                                               as.numeric(`Index of Relative Socio-economic Disadvantage`)) %>%
                                      mutate(`Index of Relative Socio-Economic Advantage and Disadvantage` = 
                                               as.numeric(`Index of Relative Socio-economic Advantage and Disadvantage`)) %>%
                                      select(c(`Index of Relative Socio-Economic Advantage and Disadvantage`,
                                               `Index of Relative Socio-Economic Disadvantage`,
                                               `Index of Economic Resources`, `Index of Education & Occupation`,
                                               Population,POA)) %>% 
                                      mutate(Year = 2016)
tidy_seifa_2016 <- tidy_seifa_2016[complete.cases(tidy_seifa_2016),]



tidy_seifa_list <- list(tidy_seifa_1996,tidy_seifa_2001,tidy_seifa_2006,tidy_seifa_2011,tidy_seifa_2016)
tidy_seifa <- NULL
#join to post_sa4 df and get weighted averages of POA to SA4
for(tidy in tidy_seifa_list){
  mapped_df <- inner_join(tidy,post_sa2_sa4,by = c("POA" = "POSTCODE")) %>% 
                                      mutate(Weight = Population) %>% 
                                      group_by(SA4_NAME_2016) %>%
                                      # make the value weighted mean of each SA4
                                      mutate(Weight = Weight/sum(Weight)) %>% 
                                      mutate(`Index of Relative Socio-Economic Advantage and Disadvantage` = 
                                               weighted.mean(`Index of Relative Socio-Economic Advantage and Disadvantage`,Weight)) %>% 
                                      mutate(`Index of Relative Socio-Economic Disadvantage` = 
                                               weighted.mean(`Index of Relative Socio-Economic Disadvantage`,Weight)) %>% 
                                      mutate(`Index of Economic Resources` = 
                                               weighted.mean(`Index of Economic Resources`,Weight)) %>% 
                                      mutate(`Index of Education & Occupation` = 
                                               weighted.mean(`Index of Education & Occupation`,Weight))  %>%
                                      mutate(Population = sum(Population)) %>% 
                                      select(-c(Weight,POA,SA2_MAINCODE)) %>% 
                                      distinct()
  #Create SEIFA dataset of 1996-2016 data by binding rows together
  tidy_seifa <- bind_rows(tidy_seifa,mapped_df)
}
#Kicking out Chrismas Island, Cocos Islands, Jervis Bay,Norfolk Island
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


