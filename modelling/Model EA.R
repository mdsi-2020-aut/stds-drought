library(tidyverse)
library(GGally)

load("data/master_data_tablebuilder.RData")


master_social_good %>% filter(unemployment > labour_force)

capitals <- c("Adelaide","Australian","Brisbane","Darwin","Greater","Melbourne","Perth","Sydney")
seniors <- c("100 years and over","90-99 years","80-89 years","70-79 years")
young <-c("15-19 years","20-29 years")
work<- c("30-39 years","40-49 years")
middle <-c("50-59 years","60-69 years")

#cleaning dat for niceness and
master_social_good <- master_social_good %>% 
  mutate(City = sub(" .*", "", sa4)) %>% 
  filter(!(age %in% seniors))  

#make it easier to read and set capitals and age bans
master_social_good$education[master_social_good$education == "hs_finished"] = "Fin. HS"
master_social_good$education[master_social_good$education == "hs_not_finished"] = "Not Fin. HS"
master_social_good$capital = master_social_good$City %in% capitals
master_social_good$capital[master_social_good$capital == T] = "Capital City"
master_social_good$capital[master_social_good$capital == F] = "Non-Capital City"
master_social_good$City = NULL

master_social_good$age_band[master_social_good$age %in% young] = "young"
master_social_good$age_band[master_social_good$age %in% work] = "work"
master_social_good$age_band[master_social_good$age %in% middle] = "middle"


dir <- "modelling/graphs/"
cat_vars <-c("age_band","sex","education","ingp","capital")
num_var <- c('IRSAD_Decile','IRSD_Decile','IER_Decile','IEO_Decile','remoteness_index')

#if three way nets for loop isn;t hersey, i don;t know what is
#this is all focusing on unemployment rate
c_vars <- cat_vars
for(col1 in cat_vars){
  for(col2 in c_vars[-1]){
    if(col1 == col2){next}
    
    title = paste(col1,col2,sep = "-")
    table <- master_social_good %>% 
      group_by_at(vars(c('sa4',col1,col2,num_var))) %>% 
      summarise_at(vars(unemployment,labour_force),sum) %>% 
      mutate(unemployment_rate = unemployment/labour_force*100)
    
    #box plot on pairwsie categroical vairables
    g <- ggplot(table,aes(y = unemployment_rate)) + 
      geom_boxplot()+ facet_wrap(reformulate(col1,col2))
    print(g)
    unlink(paste(dir,title,"_box.png",sep = ""))
    ggsave(paste(dir,title,"_box.png",sep = ""))
    
    for(col3 in num_var){
      num_title = paste(title,col3,sep = "-")
      # scatte rplot with numerical on x axis
      g <- ggplot(table,aes_string(x = col3,y = 'unemployment_rate')) + 
        geom_point()+ facet_wrap(reformulate(col1,col2))
      print(g)
      unlink(paste(dir,num_title,"_scatter.png",sep = ""))
      ggsave(paste(dir,num_title,"_scatter.png",sep = ""))
      
    }
  }
  
  
  c_vars <- c_vars[-1]
}

#surprise, surpirse, seifa is heavoly cporellated
ggpairs(master_social_good,num_var)
unlink(paste(dir,"num_cor.png",sep = ""))
ggsave(paste(dir,"num_cor.png",sep = ""))

ggpairs(master_social_good,c(cat_vars[1:2],num_var[c(1,3,5)]))
unlink(paste(dir,"cat1_cor.png",sep = ""))
ggsave(paste(dir,"cat1_cor.png",sep = ""))

ggpairs(master_social_good,c(cat_vars[3:5],num_var[c(1,3,5)]))
unlink(paste(dir,"cat2_cor.png",sep = ""))
ggsave(paste(dir,"cat2_cor.png",sep = ""))


