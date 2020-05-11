load("./data/unemploy_seifa.RData")
load("./data/unemployment.RData")
load("./data/unemployment_evap.RData")
load("./data/unemployment_precp.RData")
load("./data/unemployment_proficiency.RData")
load("./data/unemployment_remote.RData")
load("./data/unemployment_water.RData")

library(tidyverse)

glimpse(unemployment)
glimpse(unemploy_seifa)
glimpse(unemployment_RAPopWtd)
glimpse(unemployment_proficiency)
glimpse(water_unemployment)
glimpse(precp_unemployment)
glimpse(evap_unemployment)

#remove extra spaces in the unemployment SA4 name
unemployment$territory_sa4 <- str_trim(unemployment$territory_sa4, side="both")

names(unemployment_proficiency) <- tolower(names(unemployment_proficiency))
#join all the dataset
master_social_good <- unemployment %>% 
  left_join(unemploy_seifa %>% select(-c(unemployed,population)), by=c("territory_sa4" = "SA4_NAME_2016","date")) %>%
  left_join(unemployment_RAPopWtd %>% select(-c(unemployed,population)), by=c("territory_sa4","date")) %>%
  rename("remoteness_index" = "PopWtdRA_rank") %>%
  left_join(unemployment_proficiency %>% select(-c(unemployed,population)), by=c("territory_sa4","date")) %>%
  left_join(water_unemployment %>% select(-c(unemployed,population)), by=c("territory_sa4","date")) %>%
  left_join(precp_unemployment %>% select(-c(unemployed,population)), by=c("territory_sa4","date")) %>%
  left_join(evap_unemployment %>% select(-c(unemployed,population)), by=c("territory_sa4","date")) %>%
  mutate(Year = NULL)

summary(master_social_good)
glimpse(master_social_good %>% filter(date > "2006-01-01"))

#check join result
#check are there any missing value
tibble(columns = tolower(names(master_social_good %>% select(-date))), 
       values = 100 - colSums(!is.na(master_social_good %>% select(-date)))*100/nrow(master_social_good)) %>%
  arrange(desc(values)) %>%
  ggplot(mapping = aes(x = columns, y=values)) +
  geom_bar(stat = "identity",fill="blue") +
  coord_flip() +
  ggtitle("Missing Value")


#check if there is a new missing value appear after join conducted (if yes, then there are some rows that unmatch between unemployment and predictors)
tibble(columns = names(unemployment %>% select(-date)), 
       values = 100 - colSums(!is.na(unemployment %>% select(-date)))*100/nrow(unemployment)) %>%
  bind_rows(tibble(columns = names(unemploy_seifa %>% select(-date)), 
                   values = 100 - colSums(!is.na(unemploy_seifa %>% select(-date)))*100/nrow(unemploy_seifa))) %>%
  bind_rows(tibble(columns = names(unemployment_RAPopWtd %>% select(-date) %>% rename("remoteness_index" = "PopWtdRA_rank")), 
                   values = 100 - colSums(!is.na(unemployment_RAPopWtd %>% select(-date)))*100/nrow(unemployment_RAPopWtd))) %>%
  bind_rows(tibble(columns = names(unemployment_proficiency %>% select(-date)), 
                   values = 100 - colSums(!is.na(unemployment_proficiency %>% select(-date)))*100/nrow(unemployment_proficiency))) %>%
  bind_rows(tibble(columns = names(water_unemployment %>% select(-date)), 
                   values = 100 - colSums(!is.na(water_unemployment %>% select(-date)))*100/nrow(water_unemployment))) %>%
  bind_rows(tibble(columns = names(precp_unemployment %>% select(-date)), 
                   values = 100 - colSums(!is.na(precp_unemployment %>% select(-date)))*100/nrow(precp_unemployment))) %>%
  bind_rows(tibble(columns = names(evap_unemployment %>% select(-date)), 
                   values = 100 - colSums(!is.na(evap_unemployment %>% select(-date)))*100/nrow(evap_unemployment))) %>%
  inner_join(tibble(join_columns = names(master_social_good %>% select(-date)), 
                    join_values = 100 - colSums(!is.na(master_social_good %>% select(-date)))*100/nrow(master_social_good)),by=c("columns"="join_columns")) %>%
  filter(join_values != values) %>%
  arrange(desc(join_values))
#no new missing value found 

#check if mean is consistent in join result
check_mean <- unemployment %>% select(c(unemployed,population)) %>% group_by() %>%
  summarise(c(unemployed,population) = mean(c(unemployed,population))) %>%
  bind_cols(unemploy_seifa %>% select(-c(unemployed,population), -Year) %>% group_by() %>%
              summarise_if(is.numeric, funs(mean(., na.rm = T)))) %>%
  bind_cols(unemployment_RAPopWtd %>% select(-c(unemployed,population)) %>% group_by() %>%
              summarise_if(is.numeric, funs(mean(., na.rm = T)))) %>%
  bind_cols(unemployment_proficiency %>% select(-c(unemployed,population), -census_year) %>% group_by() %>%
              summarise_if(is.numeric, funs(mean(., na.rm = T)))) %>%
  bind_cols(water_unemployment %>% select(-c(unemployed,population)) %>% group_by() %>%
              summarise_if(is.numeric, funs(mean(., na.rm = T)))) %>%
  bind_cols(precp_unemployment %>% select(-c(unemployed,population)) %>% group_by() %>%
              summarise_if(is.numeric, funs(mean(., na.rm = T)))) %>%
  bind_cols(evap_unemployment %>% select(-c(unemployed,population)) %>% group_by() %>%
              summarise_if(is.numeric, funs(mean(., na.rm = T))))

tibble(columns = names(check_mean), 
       values = colSums(check_mean)) %>%
  inner_join(tibble(join_columns = names(master_social_good %>% group_by() %>%
                                           summarise_if(is.numeric, funs(mean(., na.rm = T)))),
                    join_values = colSums(master_social_good %>% group_by() %>%
                                            summarise_if(is.numeric, funs(mean(., na.rm = T))))),
             by=c("columns" = "join_columns")) %>%
  filter(values != join_values)


#save RData
save(master_social_good,file="./data/master_data.RData")



