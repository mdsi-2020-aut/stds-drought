#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_251') # Uncomment out if RJDMX  does not work out of the box for you
#library(rJava) #this too

#Loading libraries required
library(RJSDMX)
library(rsdmx)
library(readabs)
library(tidyverse)
library(raustats)
library(lubridate)

#load data frames direct from Rda frame
load("data/seifa_proficiency.RData")
load("data/unemployment.RData")
load("data/seifa1986_06.RData")
load("data/post_sa2_sa4.RData")

#where you want to save the datasets (raw)
dir = "data"

#here's the code the download and tidying the data
org <- "ABS"
Sys.setenv(R_READABS_PATH = "data/")

######################################################################################
#changed method of getting SEIFA data to align with other years.
# #use RJSDMX to explore the structure
# #get SEIFA based on post code flows ID
# flows <- getFlows(org,"*SEIFA*Postal*")
# flows
# 
# #do loop to download the data
# 
# seifa_data_bypostcode <- data.frame()
# for(seifa_flow in names(flows)){
#   #get list of dimensions of "SEIFA per post code" DSD
#   seifa_pos_dim <- getDimensions(org, seifa_flow)
#   
#   #get all the codelists
#   seifa_pos_codelist <- map(.x = names(flatten(seifa_pos_dim)),
#                             flow = seifa_flow,
#                             provider = org,
#                             .f = getCodes) %>% set_names(names(flatten(seifa_pos_dim)))
#   
#   #get the entire dataseries at once will return error
#   #do looping per SEIFAINDEXTYPE
#   for (idx in seifa_pos_codelist[2]){
#     df <- as.data.frame(readSDMX(providerId = "ABS", 
#                                  resource = "data", 
#                                  flowRef = seifa_flow,
#                                  key = list(NULL, names(idx), NULL), 
#                                  dsd = TRUE), 
#                         labels = TRUE)
#     
#     #delete additional 3 columns in SEIFA 2011
#     #rename column names 
#     if (idx == "SEIFA_POA"){
#       df$OBS_STATUS <- NULL
#       df$OBS_STATUS_label.fr <- NULL
#       df$OBS_STATUS_label.en <- NULL
#       
#       df <- df %>% rename(SEIFAINDEXTYPE = INDEX_TYPE,
#                           SEIFAINDEXTYPE_label.en = INDEX_TYPE_label.en,
#                           SEIFAINDEXTYPE_label.fr = INDEX_TYPE_label.fr,
#                           SEIFA_MEASURE = MEASURE,
#                           SEIFA_MEASURE_label.fr = MEASURE_label.fr,
#                           SEIFA_MEASURE_label.en = MEASURE_label.en)
#     }
#     seifa_data_bypostcode <- rbind(seifa_data_bypostcode, df)
#   }
# }
# 
# flows2 <- getFlows(org,"*CENSUS*2016*")
# flows2
#get list of dimensions of "SEIFA per post code" DSD
######################################################################################

#Get English proficiency data
proficiency_dims <- getDimensions(org, "ABS_C16_T08_LGA")

#get all the codelists
proficienct_codelist <- map(.x = names(flatten(proficiency_dims)),
                           flow = "ABS_C16_T08_LGA",
                           provider = org,
                           .f = getCodes) %>% set_names(names(flatten(proficiency_dims)))

proficiency <- data.frame()
for (idx in proficienct_codelist[3]){
  proficiency <- rbind(proficiency, as.data.frame(readSDMX(providerId = "ABS", 
                                                           resource = "data", 
                                                           flowRef = "ABS_C16_T08_LGA",
                                                           key = list(NULL, NULL, names(idx), NULL, NULL, NULL), 
                                                           dsd = TRUE), 
                                                  labels = TRUE))
}
#######################################################################################
#changed method of getting SEIFA data to align with other years.
# #tidying SEIFA data
# #check measure naming consistency
# unique(seifa_data_bypostcode$SEIFAINDEXTYPE)
# seifa_data_bypostcode %>% filter(SEIFAINDEXTYPE == "IRSD") %>% 
#   group_by(SEIFA_MEASURE, SEIFA_MEASURE_label.en) %>%
#   summarise(count = n()) %>% arrange(SEIFA_MEASURE)
# #all measures are consistent
# 
# seifa_data_bypostcode$SEIFA_MEASURE_label.en <- toupper(seifa_data_bypostcode$SEIFA_MEASURE_label.en)
# seifa_data_bypostcode$SEIFA_MEASURE_label.en <- str_replace_all(seifa_data_bypostcode$SEIFA_MEASURE_label.en," -","")
# seifa_data_bypostcode$SEIFA_MEASURE_label.en <- str_replace_all(seifa_data_bypostcode$SEIFA_MEASURE_label.en," ","_")
# 
# #delete colums
# seifa_data_bypostcode$POA_label.fr <- NULL
# seifa_data_bypostcode$POA_label.en <- NULL
# seifa_data_bypostcode$SEIFA_MEASURE <- NULL
# seifa_data_bypostcode$SEIFA_MEASURE_label.fr <- NULL
# seifa_data_bypostcode$SEIFAINDEXTYPE_label.fr <- NULL
# seifa_data_bypostcode$TIME_FORMAT <- NULL
# seifa_data_bypostcode$TIME_FORMAT_label.en <- NULL
# 
# seifa_data_bypostcode <- seifa_data_bypostcode %>% rename(SEIFAINDEXTYPE_LABEL = SEIFAINDEXTYPE_label.en,
#                                                           PERIOD = obsTime)
# 
# seifa_data_bypostcode <- pivot_wider(seifa_data_bypostcode, 
#                                      id_cols = -SEIFA_MEASURE_label.en,
#                                      names_from = SEIFA_MEASURE_label.en,
#                                      values_from = obsValue)
#######################################################################################


#tidying proficiency data
proficiency %>% mutate(proficiency_level = case_when(ENGLP_2016 == "1" ~ "ENGLISH_ONLY",
                                                     ENGLP_2016 == "2" ~ "OTHER_LANG_AND_ENGLISH_VERY_WELL",
                                                     ENGLP_2016 == "3" ~ "OTHER_LANG_AND_ENGLISH_WELL",
                                                     ENGLP_2016 == "4" ~ "OTHER_LANG_AND_ENGLISH_NOT_WELL",
                                                     ENGLP_2016 == "5" ~ "OTHER_LANG_AND_ENGLISH_NOT_ATALL",
                                                     ENGLP_2016 == "6" ~ "LANGUAGE_AND_PROFICIENCY_NOT_STATED",
                                                     ENGLP_2016 == "Z" ~ "LANGUAGE_STATED_PROFICIENCY_NOT_STATED",
                                                     TRUE ~ "TOTAL"))

proficiency$ENGLP_2016 <- NULL
proficiency$ENGLP_2016_label.en <- NULL
proficiency$ENGLP_2016_label.fr <- NULL
proficiency$AGE_label.fr <- NULL
proficiency$SEX_label.fr <- NULL
proficiency$STATE_label.fr <- NULL
proficiency$REGIONTYPE_label.en <- NULL
proficiency$REGIONTYPE_label.fr <- NULL
proficiency$LGA_2016_label.fr <- NULL
proficiency$TIME_FORMAT <- NULL
proficiency$TIME_FORMAT_label.en <- NULL


proficiency <- pivot_wider(proficiency, id_cols = -proficiency_level,
                           names_from = proficiency_level, 
                           values_from = obsValue)

proficiency <- proficiency %>% rename(AGE_ID = AGE,
                                      AGE = AGE_label.en,
                                      SEX_ID = SEX,
                                      SEX = SEX_label.en,
                                      STATE_ID = STATE,
                                      STATE = STATE_label.en,
                                      LGA_ID = LGA_2016,
                                      LGA_2016 = LGA_2016_label.en,
                                      PERIOD = obsTime)

#Remoteness data by category and postcode acquisition
rem_tab <- abs_cat_tables("1270.0.55.005",include_urls = T)
rem_url <- rem_tab$path_zip[8] 
rem_file <-  abs_cat_unzip(abs_cat_download(rem_url,dir),dir)
remotness <- read_xls(rem_file,sheet = 'Table 3',skip =4)

save(remotness, file="data/rem.RData")

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
#join to post_sa4 df and get weighted averages of POA to SA4
tidy_seifa_1996 <- inner_join(tidy_seifa_1996,post_sa2_sa4,by = c("POA" = "POSTCODE")) %>% 
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
#join to post_sa4 df and get weighted averages
tidy_seifa_2001 <- inner_join(tidy_seifa_2001,post_sa2_sa4,by = c("POA" = "POSTCODE")) %>% 
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

tidy_seifa_2006 <- inner_join(tidy_seifa_2006,post_sa2_sa4,by = c("POA" = "POSTCODE")) %>% 
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

tidy_seifa_2011 <- inner_join(tidy_seifa_2011,post_sa2_sa4,by = c("POA" = "POSTCODE")) %>% 
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

tidy_seifa_2016 <- inner_join(tidy_seifa_2016,post_sa2_sa4,by = c("POA" = "POSTCODE")) %>% 
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

#Create SEIFA dataset of 1996-2016 data
tidy_seifa <-  bind_rows(tidy_seifa_1996,tidy_seifa_2001,tidy_seifa_2006,tidy_seifa_2011, tidy_seifa_2016)
#Kicking out Chrismas Island, Cocos Islands, Jervis Bay,Norfolk Island
tidy_seifa <-tidy_seifa %>% filter(SA4_NAME_2016 != "Other Territories") 


#Add last census date to data frame
unemployment$year = year(unemployment$date)
unemployment$census  = (5*floor((unemployment$year-1)/5))+1
unemployment$year = NULL

#join by SA4 & Census
unemploy_seifa <- inner_join(tidy_seifa,unemployment,by = c("SA4_NAME_2016" = "territory_sa4","Year" = "census"))
save(unemploy_seifa, file="data/unemploy_seifa.RData")
