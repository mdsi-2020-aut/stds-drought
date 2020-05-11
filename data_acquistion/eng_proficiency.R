library(RJSDMX)
library(rsdmx)
library(tidyverse)
library(lubridate)

org <- "ABS"

flows <- getFlows(org,"*Profic*")
flows

#get list of dimensions of "Proficiency in Spoken English by Age by Sex (SA2+)" DSD
proficiency_dims <- getDimensions(org, "ABS_C16_T08_TS_SA")

#get all the codelists
proficiency_codelist <- map(.x = names(flatten(proficiency_dims)),
                            flow = "ABS_C16_T08_TS_SA",
                            provider = org,
                            .f = getCodes) %>% set_names(names(flatten(proficiency_dims)))


proficiency_sa4 <- tibble()
#split the process into several iteartions to make it faster
#loop by year and age
for (year in seq(2006,2016,5)){
  for (idx in names(proficiency_codelist[2][[1]])){
    print(paste(year,idx,sep = "-"))
    proficiency_sa4 <- rbind(proficiency_sa4,
                             as.data.frame(readSDMX(providerId = "ABS", 
                                                    resource = "data", 
                                                    flowRef = "ABS_C16_T08_TS_SA",
                                                    key = list(NULL, idx, names(proficiency_codelist[[3]]), NULL, "SA4", NULL), 
                                                    dsd = F,
                                                    start = year, 
                                                    end = year), 
                                           labels = TRUE))
  }
}

#cleanse & tidying dataset
glimpse(proficiency_sa4)
#proficiency_sa4_temp <- proficiency_sa4
#proficiency_sa4 <-proficiency_sa4_temp

proficiency_sa4 <- proficiency_sa4 %>% 
  mutate(PROFICIENCY_LEVEL = case_when(ENGLP_2016 == "1" ~ "ENGLISH_ONLY",
                                       ENGLP_2016 == "2" ~ "OTHER_LANG_AND_ENGLISH_VERY_WELL",
                                       ENGLP_2016 == "3" ~ "OTHER_LANG_AND_ENGLISH_WELL",
                                       ENGLP_2016 == "4" ~ "OTHER_LANG_AND_ENGLISH_NOT_WELL",
                                       ENGLP_2016 == "5" ~ "OTHER_LANG_AND_ENGLISH_NOT_ATALL",
                                       ENGLP_2016 == "6" ~ "LANGUAGE_AND_PROFICIENCY_NOT_STATED",
                                       ENGLP_2016 == "Z" ~ "LANGUAGE_STATED_PROFICIENCY_NOT_STATED",
                                       TRUE ~ "TOTAL"))

#convert codelist into data.frame for joining purposes
age.df <- tibble(ID=names(proficiency_codelist[["AGE"]]), AGE_RANGE=unlist(proficiency_codelist[["AGE"]]))
sex.df <- tibble(ID=names(proficiency_codelist[["SEX_ABS"]]), SEX=unlist(proficiency_codelist[["SEX_ABS"]]))
state.df <- tibble(ID=names(proficiency_codelist[["STATE"]]), STATE_NAME=unlist(proficiency_codelist[["STATE"]]))
asgs.df <- tibble(ID=names(proficiency_codelist[["ASGS_2016"]]), SA4_NAME=unlist(proficiency_codelist[["ASGS_2016"]]))

#join to get the label
proficiency_sa4 <- proficiency_sa4 %>% left_join(age.df, by=c("AGE" = "ID")) %>%
  left_join(sex.df, by=c("SEX_ABS" = "ID")) %>%
  left_join(state.df, by=c("STATE" = "ID")) %>%
  left_join(asgs.df, by=c("ASGS_2016" = "ID")) 

glimpse(proficiency_sa4)  

#delete column
proficiency_sa4$ENGLP_2016 <- NULL
proficiency_sa4$AGE <- NULL
proficiency_sa4$SEX_ABS <- NULL
proficiency_sa4$STATE <- NULL
proficiency_sa4$REGIONTYPE <- NULL
proficiency_sa4$TIME_FORMAT <- NULL

#convert period to integer
proficiency_sa4 <- proficiency_sa4 %>% rename("SA4_CODE" = "ASGS_2016","PERIOD" = "obsTime")

proficiency_sa4$PERIOD <- as.integer(proficiency_sa4$PERIOD)


#rearrange the column
proficiency_sa4 <- proficiency_sa4[,c(1,8,7,5,6,2,4,3)] 

#variable as a column (make it tidy)
proficiency_sa4 <- pivot_wider(proficiency_sa4, id_cols = -PROFICIENCY_LEVEL,
                                   names_from = PROFICIENCY_LEVEL, 
                                   values_from = obsValue)

glimpse(proficiency_sa4)

#calculate the percentage for each english proficiency level and sex
#Column TOTAL is not equal to the addition of it's detailed proficiency level, so use the detail value instead
proficiency_sa4_summary <- proficiency_sa4 %>% filter(SEX != "Persons" & AGE_RANGE != "All ages") %>%
  group_by(SA4_NAME, PERIOD) %>%
  summarise(FEMALES = sum(ifelse(SEX == "Females",ENGLISH_ONLY + OTHER_LANG_AND_ENGLISH_VERY_WELL + 
                                   OTHER_LANG_AND_ENGLISH_WELL + OTHER_LANG_AND_ENGLISH_NOT_WELL +
                                   OTHER_LANG_AND_ENGLISH_NOT_ATALL + LANGUAGE_AND_PROFICIENCY_NOT_STATED +
                                   LANGUAGE_STATED_PROFICIENCY_NOT_STATED,0)),
            MALES = sum(ifelse(SEX == "Males",ENGLISH_ONLY + OTHER_LANG_AND_ENGLISH_VERY_WELL + 
                                 OTHER_LANG_AND_ENGLISH_WELL + OTHER_LANG_AND_ENGLISH_NOT_WELL +
                                 OTHER_LANG_AND_ENGLISH_NOT_ATALL + LANGUAGE_AND_PROFICIENCY_NOT_STATED +
                                 LANGUAGE_STATED_PROFICIENCY_NOT_STATED,0)),
            ENGLISH_ONLY  = sum(ENGLISH_ONLY), 
            OTHER_LANG_AND_ENGLISH_VERY_WELL = sum(OTHER_LANG_AND_ENGLISH_VERY_WELL),
            OTHER_LANG_AND_ENGLISH_WELL = sum(OTHER_LANG_AND_ENGLISH_WELL), 
            OTHER_LANG_AND_ENGLISH_NOT_WELL = sum(OTHER_LANG_AND_ENGLISH_NOT_WELL),
            OTHER_LANG_AND_ENGLISH_NOT_ATALL = sum(OTHER_LANG_AND_ENGLISH_NOT_ATALL), 
            LANGUAGE_AND_PROFICIENCY_NOT_STATED = sum(LANGUAGE_AND_PROFICIENCY_NOT_STATED), 
            LANGUAGE_STATED_PROFICIENCY_NOT_STATED = sum(LANGUAGE_STATED_PROFICIENCY_NOT_STATED),
            TOTAL = sum(TOTAL)) %>%
  ungroup() %>%
  mutate(ALL_TOTAL = ENGLISH_ONLY + OTHER_LANG_AND_ENGLISH_VERY_WELL + 
           OTHER_LANG_AND_ENGLISH_WELL + OTHER_LANG_AND_ENGLISH_NOT_WELL +
           OTHER_LANG_AND_ENGLISH_NOT_ATALL + LANGUAGE_AND_PROFICIENCY_NOT_STATED +
           LANGUAGE_STATED_PROFICIENCY_NOT_STATED) %>%
  mutate(ENGLISH_ONLY  = ENGLISH_ONLY/ALL_TOTAL, 
         OTHER_LANG_AND_ENGLISH_VERY_WELL = OTHER_LANG_AND_ENGLISH_VERY_WELL/ALL_TOTAL,
         OTHER_LANG_AND_ENGLISH_WELL = OTHER_LANG_AND_ENGLISH_WELL/ALL_TOTAL, 
         OTHER_LANG_AND_ENGLISH_NOT_WELL = OTHER_LANG_AND_ENGLISH_NOT_WELL/ALL_TOTAL,
         OTHER_LANG_AND_ENGLISH_NOT_ATALL = OTHER_LANG_AND_ENGLISH_NOT_ATALL/ALL_TOTAL, 
         LANGUAGE_AND_PROFICIENCY_NOT_STATED = LANGUAGE_AND_PROFICIENCY_NOT_STATED/ALL_TOTAL, 
         LANGUAGE_STATED_PROFICIENCY_NOT_STATED = LANGUAGE_STATED_PROFICIENCY_NOT_STATED/ALL_TOTAL,
         FEMALES = FEMALES/ALL_TOTAL,
         MALES = MALES/ALL_TOTAL,
         TOTAL = NULL,
         ALL_TOTAL = NULL)

#calculate the percentage for each age
#Column TOTAL is not equal to the addition of it's detailed proficiency level, so use the detail value instead
proficiency_sa4_summary_age <- proficiency_sa4 %>% filter(SEX != "Persons" & AGE_RANGE != "All ages") %>%
  group_by(SA4_NAME, PERIOD, AGE_RANGE) %>%
  summarise(TOTAL_PER_AGE = sum(ENGLISH_ONLY + OTHER_LANG_AND_ENGLISH_VERY_WELL + 
                          OTHER_LANG_AND_ENGLISH_WELL + OTHER_LANG_AND_ENGLISH_NOT_WELL +
                          OTHER_LANG_AND_ENGLISH_NOT_ATALL + LANGUAGE_AND_PROFICIENCY_NOT_STATED +
                          LANGUAGE_STATED_PROFICIENCY_NOT_STATED)) %>%
  mutate(AGE_RANGE = paste("AGE", str_replace_all(AGE_RANGE,c(" " = "", "-" = "_", "and" = "_")),sep = "_")) %>%
  inner_join(proficiency_sa4 %>% filter(SEX != "Persons" & AGE_RANGE != "All ages") %>%
               group_by(SA4_NAME, PERIOD) %>%
               summarise(TOTAL = sum(ENGLISH_ONLY + OTHER_LANG_AND_ENGLISH_VERY_WELL + 
                                           OTHER_LANG_AND_ENGLISH_WELL + OTHER_LANG_AND_ENGLISH_NOT_WELL +
                                           OTHER_LANG_AND_ENGLISH_NOT_ATALL + LANGUAGE_AND_PROFICIENCY_NOT_STATED +
                                           LANGUAGE_STATED_PROFICIENCY_NOT_STATED)), by=c("SA4_NAME","PERIOD")) %>%
  mutate(TOTAL_PER_AGE = TOTAL_PER_AGE/TOTAL) %>%
  pivot_wider(names_from = AGE_RANGE, values_from = TOTAL_PER_AGE) %>%
  mutate(TOTAL = NULL)

#join both summary
proficiency_sa4_summary <- proficiency_sa4_summary %>% inner_join(proficiency_sa4_summary_age,by=c("SA4_NAME","PERIOD"))
summary(proficiency_sa4_summary)

#do some correction to SA4 name in proficiency data
unemploy_sa4 <- c("Greater Hobart","New South Wales - Central West","Victoria - North West",
              "Western Australia - Outback (North and South)","Western Australia - Outback (North and South)",
              "Tasmania - South East","Tasmania - West and North West")
prof_sa4 <- c("Hobart","Central West","North West","Western Australia - Outback (North)",
         "Western Australia - Outback (South)","South East","West and North West")

for(i in 1:length(prof_sa4)){
  proficiency_sa4_summary $SA4_NAME[proficiency_sa4_summary $SA4_NAME == prof_sa4[i]] <- unemploy_sa4[i]
}
load('data/unemployment.RData')
#check if there are SA4 Area that is not available unemployment (vice versa)
unemployment$territory_sa4 = str_trim(unemployment$territory_sa4,side = "both")
prof_to_unemploy <- proficiency_sa4_summary %>% left_join(unemployment, by=c("SA4_NAME"="territory_sa4")) %>% filter(is.na(unemployed)) %>% select(SA4_NAME, unemployed,population)  
unique(prof_to_unemploy$SA4_NAME)
unemploy_to_prof <- unemployment %>% left_join(proficiency_sa4_summary, by=c("territory_sa4" = "SA4_NAME")) %>% filter(is.na(ENGLISH_ONLY)) %>% select(territory_sa4, ENGLISH_ONLY)  
unique(unemploy_to_prof$territory_sa4)

#add column to unemployment
unemployment <- unemployment %>% mutate(census_year = (5*floor((year(date)-1)/5))+1)


#join unemployment with english proficiency
unemployment_proficiency <- unemployment %>% left_join(proficiency_sa4_summary, by=c("territory_sa4" = "SA4_NAME","census_year"="PERIOD")) %>% 
    filter(census_year >=2006)

glimpse(unemployment)
glimpse(proficiency_sa4)
View(unemployment_proficiency[!complete.cases(unemployment_proficiency),])

save(unemployment_proficiency, proficiency_sa4, proficiency_sa4_summary, file = "data/unemployment_proficiency.RData")


