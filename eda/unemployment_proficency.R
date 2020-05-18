library(tidyverse)
library(lubridate)
# 
# Porjct Questions
# What socio-economic, demographic, environmental and geospatial factors should be included in an unemployment insurance premium model?
# Potential matters of interest:
#   In what ways does unemployment differ between urban, regional and remote areas? 
#   What seasonal patterns are significant to provide an insurer better understanding and planning on fluctuating claims payments?
#   How effective is it to consider environmental externalities to model unemployment seasonality?


# Load Data
load("data/master_data_tablebuilder.RData")
dir <- "eda/prof_charts/"
capitals <- c("Adelaide","Australian","Brisbane","Darwin","Greater","Melbourne","Perth","Sydney")
seniors <- c("100 years and over","90-99 years","80-89 years","70-79 years")


#cleaning dat for niceness
master_social_good <- master_social_good %>% 
  mutate(City = sub(" .*", "", sa4))

master_social_good$education[master_social_good$education == "hs_finished"] = "Fin. HS"
master_social_good$education[master_social_good$education == "hs_not_finished"] = "Not Fin. HS"
master_social_good$capital = master_social_good$City %in% capitals
master_social_good$capital[master_social_good$capital == T] = "Capital City"
master_social_good$capital[master_social_good$capital == F] = "Non-Capital City"
master_social_good$City = NULL





demographics <- master_social_good %>% 
  select(-c(IRSAD_Decile:population)) %>% 
  filter(!(age %in% seniors))




summary(demographics)

#age_singluar

age <- demographics %>% 
  mutate(age = str_replace(age," years.*","")) %>% 
  group_by(sa4,age,capital) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)

title = 'Unemployment Rate by Age Band'
g <- ggplot(age,aes(x = labour_force,y = unemployment_rate,colour = age)) + 
  geom_point() +geom_jitter(height = 0)+
  scale_color_discrete(name= 'Age Band')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))


g <- ggplot(age,aes(x = age,y = unemployment_rate)) + 
  geom_boxplot() +
  labs(title=title,
       y="Unemployment Rate(%)", 
       x="Age Band") 
print(g)
unlink(paste(dir,title,"_box.png",sep = ""))
ggsave(paste(dir,title,"_box.png",sep = ""))

#gender

gender <- demographics %>% 
  mutate(age = str_replace(age," years.*","")) %>% 
  group_by(sa4,sex,capital) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100) %>% 
  mutate(labour_force = labour_force/1000)

title = 'Unemployment Rate by Gender'
g <- ggplot(gender,aes(x = labour_force,y = unemployment_rate,colour = sex)) + 
  geom_point() +geom_jitter(height = 0)+ facet_grid(~capital)+
  scale_color_discrete(name= 'Sex')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force 000s") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))



#All logical but capital

title = "Indigenous Unemployment"
logi <- demographics %>% 
  group_by(sa4,sex,education,ingp) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)

g <- ggplot(logi %>% filter(ingp == "Indigenous"),aes(x = sex,y = unemployment_rate)) + 
  geom_boxplot() + facet_grid(~education) +
  labs(title=paste(title),
       y="Unemployment Rate(%)", 
       x="Gender") 
print(g)
unlink(paste(dir,title,"_box.png",sep = ""))
ggsave(paste(dir,title,"_box.png",sep = ""))


title = "Non-Indigenous Unemployment"
logi <- demographics %>% 
  group_by(sa4,sex,education,ingp) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)

g <- ggplot(logi %>% filter(ingp != "Indigenous"),aes(x = sex,y = unemployment_rate)) + 
  geom_boxplot() + facet_grid(~education) +
  labs(title=paste(title),
       y="Unemployment Rate(%)", 
       x="Gender") 
print(g)
unlink(paste(dir,title,"_box.png",sep = ""))
ggsave(paste(dir,title,"_box.png",sep = ""))

#All logical by capital

title = "Unemployment by Capital City"
logi_cap <- demographics %>% 
  group_by(sa4,capital,education) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100) %>% 
  mutate(labour_force = labour_force/1000)

g <- ggplot(logi_cap,aes(x = labour_force,y = unemployment_rate)) + 
  geom_point() + facet_grid(education~capital) +
  labs(title=paste(title),
       y="Unemployment Rate(%)", 
       x="No. in Labour Force 000s") 
print(g)
unlink(paste(dir,title,".png",sep = ""))
ggsave(paste(dir,title,".png",sep = ""))


#aindigenous
ind <- demographics %>% 
  mutate(age = str_replace(age," years.*","")) %>% 
  group_by(sa4,age,ingp,capital,sex,remoteness_index) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)

#ignoreing wired 100yrs old lookign for work
title = 'Indigenous Unemployment by Capital city'
g <- ggplot(ind %>% filter(age != "100" &ingp == "Indigenous"),
            aes(x = labour_force,y = unemployment_rate,colour = age)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(sex~capital) +
  scale_color_discrete(name= 'Age')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))

title = 'Indigenous Unemployment by Labour force'
g <- ggplot(ind %>% filter(age != "100"  & unemployment_rate < 100),
            aes(x = labour_force,y = unemployment_rate,colour = ingp)) + 
  geom_point() +geom_jitter(height = 0) +
  scale_color_discrete(name= 'Age')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))

title = 'Non-Indigenous Unemployment by Capital city'
g <- ggplot(ind %>% filter(ingp != "Indigenous") %>% 
              mutate(labour_force = labour_force/1000),
            aes(x = labour_force,y = unemployment_rate,colour = age)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(sex~capital) +
  scale_color_discrete(name= 'Age')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force 000s") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))


#education
edu <- master_social_good %>% 
  filter(!(age %in% seniors)) %>% 
  mutate(age = str_replace(age," years.*","")) %>% 
  group_by(sa4,age,education,sex,IRSAD_Decile) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)


title = 'Unemployment by IRSAD'
g <- ggplot(edu , 
            aes(x = IRSAD_Decile,y = unemployment_rate,colour = age)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(sex~education) +
  scale_color_discrete(name= 'Age')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="Index. Relative Socio-Economic Ad. & Disad. Decile") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))


#education by indegnous
index_ind <- master_social_good %>% 
  filter(!(age %in% seniors)) %>% 
  group_by(sa4,ingp,education,sex,IRSAD_Decile) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)


title = 'Indigenous Unemployment by IRSAD'
g <- ggplot(index_ind %>% filter(unemployment_rate <= 100),
            aes(x = IRSAD_Decile,y = unemployment_rate,colour = sex)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(ingp~education) +
  scale_color_discrete(name= 'Gender')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="IRSAD Deciles") 
print(g)
unlink(paste(dir,title,".png",sep = ""))
ggsave(paste(dir,title,".png",sep = ""))



###Seniors unemployment! (or why you should alwasy keep population in mind)####

senior_demo <- master_social_good %>% 
  select(-c(IRSAD_Decile:population)) %>% 
  filter((age %in% seniors)) %>% 
  filter(labour_force > 0)



#age_singluar

age <- senior_demo %>% 
  mutate(age = str_replace(age," years.*","")) %>% 
  group_by(sa4,age,capital) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)

title = 'Senior Unemployment Rate by Age Band'
g <- ggplot(age,aes(x = labour_force,y = unemployment_rate,colour = age)) + 
  geom_point() +geom_jitter(height = 0)+
  scale_color_discrete(name= 'Age Band')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))


g <- ggplot(age,aes(x = age,y = unemployment_rate)) + 
  geom_boxplot() +
  labs(title=title,
       y="Unemployment Rate(%)", 
       x="Age Band") 
print(g)
unlink(paste(dir,title,"_box.png",sep = ""))
ggsave(paste(dir,title,"_box.png",sep = ""))

#gender

gender <- senior_demo %>% 
  mutate(age = str_replace(age," years.*","")) %>% 
  group_by(sa4,sex,capital) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100) 

title = 'Senior Unemployment Rate by Gender'
g <- ggplot(gender,aes(x = labour_force,y = unemployment_rate,colour = sex)) + 
  geom_point() +geom_jitter(height = 0)+ facet_grid(~capital)+
  scale_color_discrete(name= 'Sex')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))



#All logical but capital

title = "Senior Unemployment"
logi <- senior_demo %>% 
  group_by(sa4,sex,education,ingp) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)

g <- ggplot(logi,aes(x = sex,y = unemployment_rate)) + 
  geom_boxplot() + facet_grid(education~ingp) +
  labs(title=paste(title),
       y="Unemployment Rate(%)", 
       x="Gender") 
print(g)
unlink(paste(dir,title,"_box.png",sep = ""))
ggsave(paste(dir,title,"_box.png",sep = ""))

#All logical by capital

title = "Senior Unemployment by Capital City"
logi_cap <- senior_demo %>% 
  group_by(sa4,capital,education) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100) 

g <- ggplot(logi_cap,aes(x = labour_force,y = unemployment_rate)) + 
  geom_point() + facet_grid(education~capital) +
  labs(title=paste(title),
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,".png",sep = ""))
ggsave(paste(dir,title,".png",sep = ""))


#aindigenous
ind <- senior_demo %>% 
  mutate(age = str_replace(age," years.*","")) %>% 
  group_by(sa4,age,ingp,capital,sex,remoteness_index) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100) 

#ignoreing wired 100yrs old lookign for work
title = 'Senior Indigenous Unemployment by Capital city'
g <- ggplot(ind %>% filter(age != "100" &ingp == "Indigenous"),
            aes(x = labour_force,y = unemployment_rate,colour = age)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(sex~capital) +
  scale_color_discrete(name= 'Age')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))

title = 'Senior Indigenous Unemployment by Labour force'
g <- ggplot(ind ,
            aes(x = labour_force,y = unemployment_rate,colour = ingp)) + 
  geom_point() +geom_jitter(height = 0) +
  scale_color_discrete(name= 'Age')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))

title = 'Senior Non-Indigenous Unemployment by Capital city'
g <- ggplot(ind %>% filter(ingp != "Indigenous") %>% 
              mutate(labour_force = labour_force/1000),
            aes(x = labour_force,y = unemployment_rate,colour = age)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(sex~capital) +
  scale_color_discrete(name= 'Age')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="No. in Labour Force") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))


#education
edu <- master_social_good %>% 
  filter(age %in% seniors) %>% 
  mutate(age = str_replace(age," years.*","")) %>% 
  group_by(sa4,age,education,sex,IRSAD_Decile) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100) 
  

title = 'Senior Unemployment by IRSAD'
g <- ggplot(edu, 
            aes(x = IRSAD_Decile,y = unemployment_rate,colour = age)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(sex~education) +
  scale_color_discrete(name= 'Age')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="IRSAD Deciles") 
print(g)
unlink(paste(dir,title,"_scatter.png",sep = ""))
ggsave(paste(dir,title,"_scatter.png",sep = ""))


#education by indegnous
index_ind <- master_social_good %>%
  filter(age %in% seniors) %>% 
  group_by(sa4,ingp,education,sex,IRSAD_Decile) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)


title = 'Senior Indigenous Unemployment by IRSAD'
g <- ggplot(index_ind %>% filter(unemployment_rate <= 100),
            aes(x = IRSAD_Decile,y = unemployment_rate,colour = sex)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(ingp~education) +
  scale_color_discrete(name= 'Gender')+
  labs(title= title,
       y="Unemployment Rate(%)", 
       x="IRSAD Deciles") 
print(g)
unlink(paste(dir,title,".png",sep = ""))
ggsave(paste(dir,title,".png",sep = ""))






