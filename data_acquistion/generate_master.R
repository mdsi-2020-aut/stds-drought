rm(list = ls())

library(tidyverse)

load("./data/unemploy_seifa.RData")
#load("./data/unemployment.RData")
#load("./data/unemployment_proficiency.RData")
load("./data/unemployment_remote.RData")

#read ABS Table Builder Data
labourforce_tablebuilder <- read_csv("./ABS-tablebuilderdata/ABS_labour_force_unemployment.csv")

glimpse(unemploy_seifa)
glimpse(unemployment_RAPopWtd)
glimpse(labourforce_tablebuilder)

summary(labourforce_tablebuilder)

#slice SEIFA data to 2016 only
unemploy_seifa <- unemploy_seifa %>% filter(date == "2019-12-01")
#remove unneeded column
unemploy_seifa$Year <- NULL
unemploy_seifa$date <- NULL
unemploy_seifa$unemployed <- NULL
unemploy_seifa$Population <- NULL

glimpse(unemploy_seifa)

#slice remoteness data to one period only, since the value is similar for entire period
unemployment_RAPopWtd <- unemployment_RAPopWtd %>% 
  filter(date == "2019-12-01") %>%
  select(territory_sa4, PopWtdRA_rank) %>% 
  rename(remoteness_index = PopWtdRA_rank)

glimpse(unemployment_RAPopWtd)

#Join ABS table builder dataset with SEIFA and Remoteness
glimpse(labourforce_tablebuilder)

#rename SA4 name in unemployment table builder to be consistent with other dataset, to make it consistent
otherdata_sa4 <- c("Greater Hobart","New South Wales - Central West","Victoria - North West",
              "Western Australia - Outback (North and South)","Western Australia - Outback (North and South)",
              "Tasmania - South East","Tasmania - West and North West")
tablebuilder_sa4 <- c("Hobart","Central West","North West","Western Australia - Outback (North)",
         "Western Australia - Outback (South)","South East","West and North West")

for(i in 1:length(otherdata_sa4)){
  labourforce_tablebuilder$sa4[labourforce_tablebuilder$sa4 == tablebuilder_sa4[i]] <- otherdata_sa4[i]
}

#are there any unidentified SA4
labourforce_tablebuilder %>% 
  left_join(unemploy_seifa, by=c("sa4" = "SA4_NAME_2016")) %>%
  filter(is.na(IER_Decile)) %>% 
  group_by(sa4) %>% 
  summarise(unemployment = sum(unemployment))

#delete based on this list, instread of automotic deletion from left join above
delete_sa4 = c("Migratory - Offshore - Shipping (ACT)", "Migratory - Offshore - Shipping (NSW)", 
               "Migratory - Offshore - Shipping (NT)", "Migratory - Offshore - Shipping (OT)", "Migratory - Offshore - Shipping (Qld)", 
               "Migratory - Offshore - Shipping (SA)", "Migratory - Offshore - Shipping (Tas.)", "Migratory - Offshore - Shipping (Vic.)", 
               "Migratory - Offshore - Shipping (WA)", "No Usual Address (ACT)", "No Usual Address (NSW)", "No Usual Address (NT)", 
               "No Usual Address (OT)", "No Usual Address (Qld)","No Usual Address (SA)", "No Usual Address (Tas.)", "No Usual Address (Vic.)",
               "No Usual Address (WA)", "Other Territories", "Total")

#delete unidentified SA4 from labour force
labourforce_tablebuilder <- labourforce_tablebuilder %>% 
  filter(!sa4 %in% delete_sa4)


#join all the dataset
master_social_good <- labourforce_tablebuilder %>%
  left_join(unemploy_seifa, by=c("sa4" = "SA4_NAME_2016")) %>%
  left_join(unemployment_RAPopWtd, by=c("sa4" = "territory_sa4"))

summary(unemploy_seifa)
summary(unemployment_RAPopWtd)
summary(master_social_good)

View(master_social_good[!complete.cases(master_social_good),])

#check join result
#check are there any missing value
tibble(columns = tolower(names(master_social_good)), 
       values = 100 - colSums(!is.na(master_social_good))*100/nrow(master_social_good)) %>%
  arrange(desc(values)) %>%
  ggplot(mapping = aes(x = columns, y=values)) +
  geom_bar(stat = "identity",fill="blue") +
  coord_flip() +
  ggtitle("Missing Value") +
  ylab("%") +
  ylim(0,100)


#check if there is a new missing value appear after join conducted (if yes, then there are some rows that unmatch between unemployment and predictors)
tibble(columns = names(labourforce_tablebuilder), 
       values = 100 - colSums(!is.na(labourforce_tablebuilder))*100/nrow(labourforce_tablebuilder)) %>%
  bind_rows(tibble(columns = names(unemploy_seifa), 
                   values = 100 - colSums(!is.na(unemploy_seifa))*100/nrow(unemploy_seifa))) %>%
  bind_rows(tibble(columns = names(unemployment_RAPopWtd), 
                   values = 100 - colSums(!is.na(unemployment_RAPopWtd))*100/nrow(unemployment_RAPopWtd))) %>%
  inner_join(tibble(join_columns = names(master_social_good), 
                    join_values = 100 - colSums(!is.na(master_social_good))*100/nrow(master_social_good)),by=c("columns"="join_columns")) %>%
  filter(join_values != values) %>%
  arrange(desc(join_values))
#no new missing value found 

#check if mean is consistent in join result
check_mean <- labourforce_tablebuilder %>% select(c(unemployment,labour_force)) %>% group_by() %>%
  summarise(unemployed = mean(unemployment)) %>%
  bind_cols(unemploy_seifa %>% group_by() %>%
              summarise_if(is.numeric, funs(mean(., na.rm = T)))) %>%
  bind_cols(unemployment_RAPopWtd %>% group_by() %>%
              summarise_if(is.numeric, funs(mean(., na.rm = T))))

tibble(columns = names(check_mean), 
       values = colSums(check_mean)) %>%
  inner_join(tibble(join_columns = names(master_social_good %>% 
                                           select(sa4, IRSAD_Decile, IRSD_Decile, IER_Decile, IEO_Decile, remoteness_index) %>% 
                                           distinct() %>%
                                           group_by() %>%
                                           summarise_if(is.numeric, funs(mean(., na.rm = T)))),
                    join_values = colSums(master_social_good %>% 
                                            select(sa4, IRSAD_Decile, IRSD_Decile, IER_Decile, IEO_Decile, remoteness_index) %>%
                                            distinct() %>%
                                            group_by() %>%
                                            summarise_if(is.numeric, funs(mean(., na.rm = T))))),
             by=c("columns" = "join_columns")) %>%
  filter(values != join_values)

#check number of rows per SA4
labourforce_tablebuilder %>% group_by(sa4) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
#last disnct modify
master_social_good <- master_social_good %>% 
  distinct()


#save RData
save(master_social_good,labourforce_tablebuilder, file="./data/master_data_tablebuilder.RData")




