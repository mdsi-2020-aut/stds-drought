#load 
load("data/seifa_proficiency.RData")

#here's the code the download and tidying the data
library(RJSDMX)
library(rsdmx)
library(readabs)
library(tidyverse)
library(raustats)

org <- "ABS"
Sys.setenv(R_READABS_PATH = "data/")

#use RJSDMX to explore the structure
#get SEIFA based on post code flows ID
flows <- getFlows(org,"*SEIFA*Postal*")
flows

#do loop to download the data

seifa_data_bypostcode <- data.frame()
for(seifa_flow in names(flows)){
  #get list of dimensions of "SEIFA per post code" DSD
  seifa_pos_dim <- getDimensions(org, seifa_flow)
  
  #get all the codelists
  seifa_pos_codelist <- map(.x = names(flatten(seifa_pos_dim)),
                            flow = seifa_flow,
                            provider = org,
                            .f = getCodes) %>% set_names(names(flatten(seifa_pos_dim)))
  
  #get the entire dataseries at once will return error
  #do looping per SEIFAINDEXTYPE
  for (idx in seifa_pos_codelist[2]){
    df <- as.data.frame(readSDMX(providerId = "ABS", 
                                 resource = "data", 
                                 flowRef = seifa_flow,
                                 key = list(NULL, names(idx), NULL), 
                                 dsd = TRUE), 
                        labels = TRUE)
    
    #delete additional 3 columns in SEIFA 2011
    #rename column names 
    if (idx == "SEIFA_POA"){
      df$OBS_STATUS <- NULL
      df$OBS_STATUS_label.fr <- NULL
      df$OBS_STATUS_label.en <- NULL
      
      df <- df %>% rename(SEIFAINDEXTYPE = INDEX_TYPE,
                          SEIFAINDEXTYPE_label.en = INDEX_TYPE_label.en,
                          SEIFAINDEXTYPE_label.fr = INDEX_TYPE_label.fr,
                          SEIFA_MEASURE = MEASURE,
                          SEIFA_MEASURE_label.fr = MEASURE_label.fr,
                          SEIFA_MEASURE_label.en = MEASURE_label.en)
    }
    seifa_data_bypostcode <- rbind(seifa_data_bypostcode, df)
  }
}

flows2 <- getFlows(org,"*CENSUS*2016*")
flows2
#get list of dimensions of "SEIFA per post code" DSD
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

#tidying SEIFA data
#check measure naming consistency
unique(seifa_data_bypostcode$SEIFAINDEXTYPE)
seifa_data_bypostcode %>% filter(SEIFAINDEXTYPE == "IRSD") %>% 
  group_by(SEIFA_MEASURE, SEIFA_MEASURE_label.en) %>%
  summarise(count = n()) %>% arrange(SEIFA_MEASURE)
#all measures are consistent

seifa_data_bypostcode$SEIFA_MEASURE_label.en <- toupper(seifa_data_bypostcode$SEIFA_MEASURE_label.en)
seifa_data_bypostcode$SEIFA_MEASURE_label.en <- str_replace_all(seifa_data_bypostcode$SEIFA_MEASURE_label.en," -","")
seifa_data_bypostcode$SEIFA_MEASURE_label.en <- str_replace_all(seifa_data_bypostcode$SEIFA_MEASURE_label.en," ","_")

#delete colums
seifa_data_bypostcode$POA_label.fr <- NULL
seifa_data_bypostcode$POA_label.en <- NULL
seifa_data_bypostcode$SEIFA_MEASURE <- NULL
seifa_data_bypostcode$SEIFA_MEASURE_label.fr <- NULL
seifa_data_bypostcode$SEIFAINDEXTYPE_label.fr <- NULL
seifa_data_bypostcode$TIME_FORMAT <- NULL
seifa_data_bypostcode$TIME_FORMAT_label.en <- NULL

seifa_data_bypostcode <- seifa_data_bypostcode %>% rename(SEIFAINDEXTYPE_LABEL = SEIFAINDEXTYPE_label.en,
                                                          PERIOD = obsTime)

seifa_data_bypostcode <- pivot_wider(seifa_data_bypostcode, 
                                     id_cols = -SEIFA_MEASURE_label.en,
                                     names_from = SEIFA_MEASURE_label.en,
                                     values_from = obsValue)

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

#use readabs package to download australian demographics statistics
age <- data.frame()
for (i in 51:59){
  age <- rbind(age,read_abs(cat_no = "3101.0",tables = i))
}

age <- age %>% mutate(state = case_when(table_no =="3101051" ~ "NSW",
                                 table_no =="3101052" ~ "VIC",
                                 table_no =="3101053" ~ "QLD",
                                 table_no =="3101054" ~ "SA",
                                 table_no =="3101055" ~ "WA",
                                 table_no =="3101056" ~ "TAS",
                                 table_no =="3101057" ~ "NT",
                                 table_no =="3101058" ~ "ACT",
                                 table_no =="3101059" ~ "AUS")) %>%
  mutate(count_item = length(str_split(series, pattern = ";")))
