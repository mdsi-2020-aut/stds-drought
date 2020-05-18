library(tidyverse)
library(raustats)
library(lubridate)

load("data/master_data_tablebuilder.RData")
load("data/unemployment.RData")
seifa_dir <- "eda/SEIFA_charts/"
prof_dir <- "eda/prof_charts/"

master_social_good$education[master_social_good$education == "hs_finished"] = "Fin. HS"
master_social_good$education[master_social_good$education == "hs_not_finished"] = "Not Fin. HS"
capitals <- c("Adelaide","Australian","Brisbane","Darwin","Greater","Melbourne","Perth","Sydney")
seniors <- c("100 years and over","90-99 years","80-89 years","70-79 years")


seifa <- master_social_good %>% 
  select(c(sa4,IRSAD_Decile:IEO_Decile,remoteness_index)) %>% 
  mutate(City = sub(" .*", "", sa4)) %>% 
  distinct()
seifa$capital = seifa$City %in% capitals
seifa$capital[seifa$capital == T] = "Capital City"
seifa$capital[seifa$capital == F] = "Non-Capital City"
seifa$City = NULL

labour_abs <- abs_cat_stats("6291.0.55.001", tables="Table.+16b\\D")

unemployment <- labour_abs %>% 
  filter(grepl("Unemployment rate ;  Persons", data_item_description, ignore.case=TRUE)) %>% 
  select(date,value,data_item_description) %>% 
  rename(unemployment_rate = value) %>% 
  filter(grepl(">>>|Australian|Hobart", data_item_description, ignore.case=TRUE)) %>% # SA4 geographical divisions can be identified by ">>>"
  mutate(territory_sa4 = str_replace_all(word(data_item_description,1,sep = "\\;"),'>>> |>> |> ','')) %>% 
  select(date,unemployment_rate,territory_sa4)

unemployment_seifa<- master_social_good %>% 
  group_by(sa4) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  inner_join(seifa) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100 )

g <-  unemployment_seifa %>% 
  ggplot(aes(x = IRSAD_Decile, y = unemployment_rate))+
  geom_jitter(height = 0)+
  scale_color_discrete(name= 'Capital')+
  labs(title ="IRSAD vs Unemployment Rate",
       y="Unemployment Rate (%)", 
       x="ISRAD Deciles") +
  geom_smooth(method = "lm", se = FALSE)+
  theme(axis.title.x = element_text(vjust = -2))
print(g)
ggsave(paste(seifa_dir,"IRSAD vs Unemployment.png",sep = ""))


g <-  ggplot(unemployment_seifa,aes(x = IRSAD_Decile, y = unemployment_rate,color = capital))+
  geom_point()+
  geom_jitter(height = 0)+
  scale_color_discrete(name= 'Capital')+
  labs(title ="IRSAD vs Unemployment Rate",
       y="Unemployment Rate (%)", 
       x="ISRAD Deciles") +
  geom_smooth(aes(colour = NULL),method = "lm", se = FALSE)+
  theme(axis.title.x = element_text(vjust = -2),
        legend.position="bottom")
print(g)
ggsave(paste(seifa_dir,"IRSAD vs Unemployment v2.png",sep = ""))


#education by indegnous
index_ind <- master_social_good %>% 
  filter(!(age %in% seniors)) %>% 
  group_by(sa4,ingp,education,sex,IRSAD_Decile) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100)


g <- ggplot(index_ind %>% filter(ingp == "Indigenous"),
            aes(x = IRSAD_Decile,y = unemployment_rate,colour = sex)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(~education) +
  scale_color_discrete(name= 'Gender')+
  labs(title= 'Indigenous Unemployment by IRSAD',
       y="Unemployment Rate(%)", 
       x="IRSAD Deciles") +
  geom_smooth(aes(colour = NULL),method = "lm", se = FALSE)+
  theme(axis.title.x = element_text(vjust = -2),
        legend.position="bottom")
print(g)
unlink(paste(prof_dir,'Indigenous Unemployment by IRSAD.png',sep = ""))
ggsave(paste(prof_dir,'Indigenous Unemployment by IRSAD.png',sep = ""))

g <- ggplot(index_ind %>% filter(ingp != "Indigenous"),
            aes(x = IRSAD_Decile,y = unemployment_rate,colour = sex)) + 
  geom_point() +geom_jitter(height = 0)+facet_grid(~education) +
  scale_color_discrete(name= 'Gender')+
  labs(title= 'Non-Indigenous Unemployment by IRSAD',
       y="Unemployment Rate(%)", 
       x="IRSAD Deciles") +
  geom_smooth(aes(colour = NULL),method = "lm", se = FALSE,color = "#d8c26a")+
  theme(axis.title.x = element_text(vjust = -2),
        legend.position="bottom")
print(g)
unlink(paste(prof_dir,'Non-Indigenous Unemployment by IRSAD.png',sep = ""))
ggsave(paste(prof_dir,'Non-Indigenous Unemployment by IRSAD.png',sep = ""))

#Load ABS sa1 -> sa4 -> gccsa -> state rollup hierarchies 
sa1_4 <- read_csv("data/SA1_2016_AUST.csv")
glimpse(sa1_4) #Observations: 57,523
glimpse(u_r) #Observations: 15,390

#Create a 1-2-1 like territory rollup table
sa4_rollup <- sa1_4 %>%
  add_column(territory_nation = "Australia") %>%
  select(territory_nation, territory_sa4 = SA4_NAME_2016, territory_gccsa = GCCSA_NAME_2016, territory_state =STATE_NAME_2016) %>%
  distinct(territory_sa4, territory_gccsa, territory_state, territory_nation) 
glimpse(sa4_rollup)# Observations: 107 - interim SUCCESS!!!
count(distinct(sa4_rollup, territory_sa4)) #1   107
print(distinct(sa4_rollup, territory_sa4), n = "inf") # A tibble: 107 x 1
print(distinct(sa4_rollup, territory_gccsa), n = "inf") # A tibble: 34 x 1
print(distinct(sa4_rollup, territory_state), n = "inf") # A tibble: 9 x 1
print(distinct(sa4_rollup, territory_nation), n = "inf") # A tibble: 1 x 1

# Join unemployment data to territory rollups
u_r_rollup <- left_join(u_r, sa4_rollup) %>%
  select(territory_nation, territory_state, territory_gccsa, territory_sa4, remoteness_index, date, unemployed, population, unemploy_rate)

#Build a table for charting unemployment for the whole of Australia
u_aust <- u_r_rollup %>%
  select(territory_nation, date, unemployed, population) %>%
  group_by(territory_nation, date) %>%
  summarize(unemployed = sum(unemployed), population = sum(population)) %>%
  mutate(unemploy_rate = unemployed / population)

#Build a table for charting unemployment for the whole of Australia
u_aust_tline <- ggplot(data = u_aust) + 
  geom_smooth(color = "#6eb7a7",mapping = aes(x = date, y = unemploy_rate), se = FALSE)+ 
  ggtitle("Unemployment Rates Australia")+
  labs(y="Unemployment Rate", x = "Jan 2006 - Mar 2020")+ theme(axis.title.x = element_text(vjust = -2))
print(u_aust_tline + ylim(0.015,0.05) )
ggsave("RemoteEDA 6 Aust TLine.png")
print(u_aust_tline)
ggsave("RemoteEDA 6 Aust TLine org.png")

#Build a table to show unemployment variation in NSW and WA
u_states <- u_r_rollup %>%
  select(territory_state, date, unemployed, population) %>%
  group_by(territory_state, date) %>%
  filter(territory_state == "Western Australia" | territory_state == "New South Wales") %>% 
  summarize(unemployed = sum(unemployed), population = sum(population)) %>%
  mutate(unemploy_rate = unemployed / population)

#for charting unemployment in  NSW and WA
u_states_tline <- ggplot(data = u_states) + 
  geom_smooth(color = "#6eb7a7",mapping = aes(x = date, y = unemploy_rate, linetype = territory_state), se = FALSE)+ 
  ggtitle("Unemployment Rates NSW and WA")+
  labs(y="Unemployment Rate", x = "Jan 2006 - Mar 2020") + 
  theme(axis.title.x = element_text(vjust = -2),
                           legend.position="bottom")+
  scale_linetype(name = "State")
print(u_states_tline + ylim(0.015,0.05))
ggsave("RemoteEDA 8 AustStates TLine  NSWWA.png")
print(u_states_tline)
ggsave("RemoteEDA 8 AustStates TLine  NSWWA org.png ")



u_gccsa <- u_r_rollup %>%
  select(territory_gccsa, date, unemployed, population) %>%
  group_by(territory_gccsa, date) %>%
  filter(territory_gccsa == "Greater Sydney" | territory_gccsa == "Rest of NSW") %>% 
  summarize(unemployed = sum(unemployed), population = sum(population)) %>%
  mutate(unemploy_rate = unemployed / population)

#Build a table for charting unemployment in Greater Sydney and the Rest of NSW
u_gccsa_tline <- ggplot(data = u_gccsa) + 
  geom_smooth(color = "#6eb7a7",mapping = aes(x = date, y = unemploy_rate, linetype = territory_gccsa), se = FALSE) + 
  ggtitle("Unemployment Rates Greater Sydney\nand Rest of NSW")+
  labs(y="Unemployment Rate", x = "Jan 2006 - Mar 2020") + 
  theme(axis.title.x = element_text(vjust = -2),
                           legend.position="bottom")+
  scale_linetype(name = "")
print(u_gccsa_tline+ ylim(0.015,0.05) )
ggsave("RemoteEDA 9 AustGCCSA TLine  GSydNSW.png")
print(u_gccsa_tline)
ggsave("RemoteEDA 9 AustGCCSA TLine  GSydNSW org.png")


#Build a table for charting unemployment Sydney 
u_sa4 <- u_r_rollup %>%
  select(territory_sa4, date, unemployed, population) %>%
  group_by(territory_sa4, date) %>%
  filter(territory_sa4 == "Sydney - Eastern Suburbs" | territory_sa4 == "Sydney - South West") %>% 
  summarize(unemployed = sum(unemployed), population = sum(population)) %>%
  mutate(unemploy_rate = unemployed / population)


u_sa4_tline <- ggplot(data = u_sa4) + 
  geom_smooth(color = "#6eb7a7",mapping = aes(x = date, y = unemploy_rate, linetype = territory_sa4), se = FALSE) + 
  ggtitle("Unemployment Rates Sydney Eastern\nSuburbs and South West")+
  labs(y="Unemployment Rate", x = "Jan 2006 - Mar 2020") + theme(axis.title.x = element_text(vjust = -2),
                           legend.position="bottom")+
  scale_linetype(name = "")
print(u_sa4_tline+ ylim(0.015,0.05))
ggsave("RemoteEDA 10 Austsa4 TLine  SydEastAndSouthWest.png")
print(u_sa4_tline)
ggsave("RemoteEDA 10 Austsa4 TLine  SydEastAndSouthWest org.png")
#[add this end of ggfplot object]  +ylim(0.015,0.05) + theme(axis.title.x = element_text(vjust = -2),
#                          legend.position="bottom")
#[add to scale title?] + scale_line_type(name = "Whaterever") 
#[add to geom_smooth]colour = "red",se = F
