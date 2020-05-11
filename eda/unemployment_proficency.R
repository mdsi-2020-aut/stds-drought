library(tidyverse)
library(lubridate)
library(corrplot)
# 
# Porjct Questions
# What socio-economic, demographic, environmental and geospatial factors should be included in an unemployment insurance premium model?
# Potential matters of interest:
#   In what ways does unemployment differ between urban, regional and remote areas? 
#   What seasonal patterns are significant to provide an insurer better understanding and planning on fluctuating claims payments?
#   How effective is it to consider environmental externalities to model unemployment seasonality?


# Load Data
load("data/unemployment_proficiency.RData")
dir <- "eda/prof_charts/"


#MAke bins for ages and drop them.
unemployment_proficiency <- unemployment_proficiency %>% 
  filter(census_year >= 2006)  %>% 
  mutate(children =  AGE_0_4 + AGE_5_9 + AGE_10_14) %>% 
  mutate(young_ad = AGE_15_19 + AGE_20_24 + AGE_25_29) %>% 
  mutate(work_ad = AGE_30_34 + AGE_35_39 + AGE_40_44 + AGE_45_49) %>% 
  mutate (mid_ad = AGE_50_54 + AGE_55_59 +AGE_60_64 + AGE_65_69) %>% 
  mutate(old = 1 - children -young_ad - work_ad - mid_ad) %>% 
  select(-c(AGE_0_4:AGE_95_99)) %>% 
  mutate(month = month(date))
glimpse(unemployment_proficiency)
summary(unemployment_proficiency)



#corrplot (save manually)
age_cor <- cor(unemployment_proficiency %>% select(c(unemployment_rate,children:old)))
corrplot(age_cor)


#Not partiuclalry helpful
lang_cor <- cor(unemployment_proficiency %>% select(c(unemployment_rate,ENGLISH_ONLY:LANGUAGE_STATED_PROFICIENCY_NOT_STATED)))
corrplot(lang_cor,tl.pos = 'n')


#why we don;t use labels
all_cor <- cor(unemployment_proficiency %>% select(c(unemployment_rate,vars)))
corrplot(all_cor,tl.pos = 'n')




vars <- colnames(unemployment_proficiency)[5:18]
#make single variable graphs based on non-averaged data
for(col in vars){
  title <- paste(col,"vs Unemployment")
  g <- ggplot(unemployment_proficiency,aes_string(x = col ,y = 'unemployment_rate')) + 
    geom_point() +geom_jitter() +ggtitle(title)
  print(g)
  unlink(paste(dir,title,".png",sep = ""))
  ggsave(paste(dir,title,".png",sep = ""))
}

year_avg <- unemployment_proficiency %>% 
  group_by(census_year,territory_sa4) %>% 
  summarise(unemployment_rate = mean(unemployment_rate)) %>% 
  ungroup()
#WARNING: USing this tables makes te asssumption that year or month does not matter
clean_avg <- unemployment_proficiency %>% 
  select(-c(date,unemployment_rate,month)) %>% 
  distinct() %>% 
  full_join(year_avg)

#make single variable graphs based on averaged data
for(col in vars){
  title <- paste("Avg",col,"vs Unemployment")
  g <- ggplot(clean_avg,aes_string(x = col ,y = 'unemployment_rate')) + 
    geom_point() +geom_jitter() +ggtitle(title)
  print(g)
  unlink(paste(dir,title,".png",sep = ""))
  ggsave(paste(dir,title,".png",sep = ""))
}
#Stupidly long way to meaure unemplpoyment base on two variables
md_vars <- vars
for(col1 in md_vars){
  for(col2 in md_vars[-1]){
    
    if(col1 == col2){
      next
    }
    #print(paste(col1,col2))
    
    #Ignoreing Queensland-Outback 2016 who's uemployemnt was always > 10%
    title <- paste(col1,col2,"Avg_Unemployment", sep = "_")
    g <- ggplot(clean_avg %>% filter(unemployment_rate <10.0),aes_string(x = col1 ,y = 'unemployment_rate',colour = col2)) + 
      geom_point() +geom_jitter() +ggtitle(paste(col1,",\n",col2,"\nand Avg Unemployment", sep = ""))  + 
      scale_color_gradient(low="light blue", high="blue",name= 'See title')
    print(g)
    unlink(paste(dir,title,".png",sep = ""))
    ggsave(paste(dir,title,".png",sep = ""))
  }
  md_vars<- md_vars[-1]
}


#try binning a more strict English - Can speak otehr langauge divide (I'm counting the ones with nothing stated as non-english)
new_clean_avg <- clean_avg %>% 
  mutate(not_english = 1- ENGLISH_ONLY) %>% 
  select(-c(OTHER_LANG_AND_ENGLISH_VERY_WELL:LANGUAGE_STATED_PROFICIENCY_NOT_STATED))

new_vars <- colnames(new_clean_avg %>%  select(-c(territory_sa4,census_year)))


#MAke graphs for new variable
com_var <- c(dplyr::intersect(new_vars,vars))

for(col in com_var){
  
  #print(paste(col,'not_english'))
  
  #Ignoreing Queensland-Outback 2016 who's uemployemnt was always > 10%
  title <- paste(col,'not_english',"Avg_Unemployment", sep = "_")
  g <- ggplot(new_clean_avg %>% filter(unemployment_rate <10.0),aes_string(x = col ,y = 'unemployment_rate',colour = 'not_english')) +
    geom_point() +geom_jitter() +ggtitle(paste(col,"\nnot_english\nand Avg Unemployment", sep = ""))  +
    scale_color_gradient(low="light blue", high="blue",name= 'See title')
  print(g)
  unlink(paste(dir,title,".png",sep = ""))
  ggsave(paste(dir,title,".png",sep = ""))
  
  
}
  


uniques_sa4 <- unemployment_proficiency %>% select(territory_sa4) %>% unique() %>% arrange(territory_sa4)



#Variables to consider: Children, OTHER_LANG_AND_ENGLISH_NOT_WELL,OTHER_LANG_AND_ENGLISH_NOT_ATALL
#LANGUAGE_AND_PROFICIENCY_NOT_STATED LANGUAGE_STATED_PROFICIENCY_NOT_STATED

#some ineration between children and middle/elders?
#working and young aduls defineity decrease uenployment?


#Dividing by SA4 si not helpful, limited data fo each
i <- 1
while(i <= nrow(uniques_sa4)) {
  g <- ggplot(unemployment_proficiency %>% filter(territory_sa4 %in% uniques_sa4[i:(i+8),]),
              aes(x= children, y = OTHER_LANG_AND_ENGLISH_NOT_WELL,colour = unemployment_rate)) +
    geom_point() + scale_color_gradient(low="green", high="red",name= 'Unemployment') +
    ggtitle("Unemployment rate  by AS4") +
    facet_wrap(. ~territory_sa4)
  
  i = i+8
  
  print(g)
}



#Not english may be a factor?
#Honestyl, considering notthing is jumping out to me, I'm hesitatn to say if there is a clear
#relationship in demographics  here by montsh anyways