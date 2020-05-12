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
load("data/master_data.RData")
dir <- "eda/prof_charts/"


#MAke bins for ages and drop them.

unemployment_proficiency <- master_social_good %>% 
  filter(census_year >= 2006)  %>% 
  mutate(children =  age_0_4 + age_5_9 + age_10_14) %>% 
  mutate(young_ad = age_15_19 + age_20_24 + age_25_29) %>% 
  mutate(work_ad = age_30_34 + age_35_39 + age_40_44 + age_45_49) %>% 
  mutate (mid_ad = age_50_54 + age_55_59 +age_60_64 + age_65_69) %>% 
  mutate(old = 1 - children -young_ad - work_ad - mid_ad) %>% 
  select(-c(age_0_4:age_95_99)) %>% 
  mutate(unemployment_ratio = (unemployed)/(population)) %>% 
  select(-c(unemployed,population,Population,IRSAD_Decile,IRSD_Decile,IER_Decile,IEO_Decile ))
  
glimpse(unemployment_proficiency)
summary(unemployment_proficiency)

  

#corrplot (save manually)
age_cor <- cor(unemployment_proficiency %>% select(c(unemployment_ratio,children:old)))
corrplot(age_cor)


#Not partiuclalry helpful
lang_cor <- cor(unemployment_proficiency %>% select(c(unemployment_ratio,english_only:language_and_proficiency_not_stated)))
corrplot(lang_cor,tl.pos = 'n')

vars <- colnames(unemployment_proficiency)[c(3,5:18)]
#why we don;t use labels
all_cor <- cor(unemployment_proficiency %>% select(c(unemployment_ratio,vars)))
corrplot(all_cor,tl.pos = 'n')





#make single variable graphs based on non-averaged data
for(col in vars){
  title <- paste(col,"vs Unemployment")
  g <- ggplot(unemployment_proficiency,aes_string(x = col ,y = 'unemployment_ratio')) + 
    geom_point() +geom_jitter() +ggtitle(title)
  print(g)
  unlink(paste(dir,title,".png",sep = ""))
  ggsave(paste(dir,title,".png",sep = ""))
}

year_avg <- unemployment_proficiency %>% 
  group_by(census_year,territory_sa4) %>% 
  summarise(unemployment_ratio = mean(unemployment_ratio)) %>% 
  ungroup()

#WARNING: USing this tables makes te asssumption that year or month does not matter
clean_avg <- unemployment_proficiency %>% 
  select(-c(date,unemployment_ratio)) %>% 
  distinct() %>% 
  full_join(year_avg)

#make single variable graphs based on averaged data
for(col in vars){
  title <- paste("Avg",col,"vs Unemployment")
  g <- ggplot(clean_avg,aes_string(x = col ,y = 'unemployment_ratio')) + 
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
    #We're exluding Queensland - Outback 2016
    title <- paste(col1,col2,"Avg_Unemployment", sep = "_")
    g <- ggplot(clean_avg %>% filter(unemployment_ratio < 0.06) ,aes_string(x = col1 ,y = 'unemployment_ratio',colour = col2)) + 
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
  mutate(not_english = 1- english_only) %>% 
  select(-c(other_lang_and_english_very_well:language_stated_proficiency_not_stated))

new_vars <- colnames(new_clean_avg %>%  select(-c(territory_sa4,census_year)))


#MAke graphs for new variable
com_var <- c(dplyr::intersect(new_vars,vars))

for(col in com_var){
  
  #print(paste(col,'not_english'))
  
  title <- paste(col,'not_english',"Avg_Unemployment", sep = "_")
  g <- ggplot(new_clean_avg %>% filter(unemployment_ratio < 0.06),aes_string(colour = col ,y = 'unemployment_ratio',x = 'not_english')) +
    geom_point() +geom_jitter() +ggtitle(paste(col,"\nnot_english\nand Avg Unemployment", sep = ""))  +
    scale_color_gradient(low="light blue", high="blue",name= 'See title')
  print(g)
  unlink(paste(dir,title,".png",sep = ""))
  ggsave(paste(dir,title,".png",sep = ""))
  
  
}




uniques_sa4 <- unemployment_proficiency %>% select(territory_sa4) %>% unique() %>% arrange(territory_sa4)



#Variables to consider: Children, OTHER_LANG_AND_ENGLISH_NOT_WELL,OTHER_LANG_AND_ENGLISH_NOT_ATALL
#LANGUage_AND_PROFICIENCY_NOT_STATED LANGUage_STATED_PROFICIENCY_NOT_STATED

#some ineration between children and middle/elders?
#working and young aduls defineity decrease uenployment?


#Dividing by SA4 si not helpful, limited data fo each
i <- 1
while(i <= nrow(uniques_sa4)) {
  g <- ggplot(unemployment_proficiency %>% filter(territory_sa4 %in% uniques_sa4[i:(i+8),]),
              aes(x= children, y = OTHER_LANG_AND_ENGLISH_NOT_WELL,colour = unemployment_ratio)) +
    geom_point() + scale_color_gradient(low="green", high="red",name= 'Unemployment') +
    ggtitle("Unemployment rate  by AS4") +
    facet_wrap(. ~territory_sa4)
  
  i = i+8
  
  print(g)
}

#More refiend graphs

cleaner <- clean_avg %>% 
  select(c(unemployment_ratio,other_lang_and_english_not_atall,
           language_stated_proficiency_not_stated,young_ad,work_ad,territory_sa4,census_year)) %>% 
  inner_join(new_clean_avg %>% select(c(territory_sa4,not_english,census_year))) %>% 
  distinct() %>% 
  select(-c(census_year,territory_sa4)) %>% 
  mutate(other_lang_and_english_not_atall = other_lang_and_english_not_atall*100) %>% 
  mutate(young_ad = young_ad*100) %>% 
  mutate(not_english = not_english*100)


g <- ggplot(cleaner %>% filter(unemployment_ratio < 0.06) ,aes(x = other_lang_and_english_not_atall ,y = unemployment_ratio,colour = young_ad)) + 
  geom_point() +geom_jitter() +  xlab("% of pop who can't speak English") + ylab("Unemployment ratio") +
  scale_color_gradient(low="light blue", high="blue",name= 'Young Adults\npercentage')
print(g)
unlink(paste(dir,"good1.png",sep = ""))
ggsave(paste(dir,"good1.png",sep = ""))



g <- ggplot(cleaner %>% filter(unemployment_ratio < 0.06) ,aes(x =not_english,y = unemployment_ratio,colour = other_lang_and_english_not_atall)) + 
  geom_point() +geom_jitter() +  xlab("% of pop who don't speak english purely") + ylab("Unemployment ratio") +
  scale_color_gradient(low="light blue", high="blue",name= 'Poor English\npercentage')
print(g)
unlink(paste(dir,"good2.png",sep = ""))
ggsave(paste(dir,"good2.png",sep = ""))




