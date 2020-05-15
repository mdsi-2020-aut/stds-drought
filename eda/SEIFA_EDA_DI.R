library(tidyverse)
library(lubridate)
library(Amelia)
library(corrplot)
library(dlookr)
library(here)

# Research Questions
# What socio-economic, demographic, environmental and geospatial factors should be 
#included in an unemployment insurance premium model?
# Other questions
#   In what ways does unemployment differ between urban, regional and remote areas? 
#   What seasonal patterns are significant to provide an insurer better understanding 
#   and planning on fluctuating claims payments? (WHY IS THIS IMPORTANT TO THE CLIENT?)

#   How effective is it to consider environmental externalities to model unemployment seasonality?


# Load Data
#load("C:/Users/D/Desktop/UTS_MasterDataScience&InnovationSubjects/2020_autumn_stds/stds-drought/data/master_data.RData")
load("data/master_data_tablebuilder.RData")
dir <- "eda/SEIFA_charts/"

#SEIFA EDA
#Diagnostic report on data quality
#master_social_good %>%
#  diagnose_report(output_format = "html")

#nothing missing; all missing values removed before merging
missmap(master_social_good, main = "Missing values vs observed") #0% missing values

#Visualising distributions - continuous; use histogram

summary(master_social_good)
# sa4                age                sex             education             ingp          
# Length:7040        Length:7040        Length:7040        Length:7040        Length:7040       
# Class :character   Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# unemployment      labour_force       IRSAD_Decile    IRSD_Decile      IER_Decile      IEO_Decile   
# Min.   :   0.00   Min.   :    0.00   Min.   :1.557   Min.   :2.027   Min.   :2.001   Min.   :1.868  
# 1st Qu.:   0.00   1st Qu.:    2.25   1st Qu.:4.004   1st Qu.:4.049   1st Qu.:3.934   1st Qu.:3.684  
# Median :   5.00   Median :   69.00   Median :5.230   Median :5.106   Median :5.028   Median :5.067  
# Mean   : 110.17   Mean   : 1625.98   Mean   :5.676   Mean   :5.553   Mean   :5.169   Mean   :5.422  
# 3rd Qu.:  98.25   3rd Qu.: 1468.75   3rd Qu.:7.212   3rd Qu.:6.886   3rd Qu.:6.245   3rd Qu.:6.396  
# Max.   :4643.00   Max.   :52977.00   Max.   :9.833   Max.   :9.664   Max.   :9.337   Max.   :9.884  
# population     remoteness_index
# Min.   : 32703   Min.   :1.000   
# 1st Qu.:136388   1st Qu.:1.008   
# Median :202472   Median :1.146   
# Mean   :235760   Mean   :1.826   
# 3rd Qu.:306580   3rd Qu.:2.278   
# Max.   :694596   Max.   :8.016    

diagnose(master_social_good)
# # A tibble: 13 x 6
# variables        types     missing_count missing_percent unique_count unique_rate
# <chr>            <chr>             <int>           <dbl>        <int>       <dbl>
#   1 sa4              character             0               0           87    0.0124  
# 2 age              character             0               0           10    0.00142 
# 3 sex              character             0               0            2    0.000284
# 4 education        character             0               0            2    0.000284
# 5 ingp             character             0               0            2    0.000284
# 6 unemployment     numeric               0               0          779    0.111   
# 7 labour_force     numeric               0               0         2353    0.334   
# 8 IRSAD_Decile     numeric               0               0           87    0.0124  
# 9 IRSD_Decile      numeric               0               0           87    0.0124  
# 10 IER_Decile       numeric               0               0           87    0.0124  
# 11 IEO_Decile       numeric               0               0           87    0.0124  
# 12 population       numeric               0               0           87    0.0124  
# 13 remoteness_index numeric               0               0           73    0.0104 

#Consider Unique value : Variables with a unique value (unique_count = 1) are 
#considered to be excluded from data analysis.

numEDA <- diagnose_numeric(master_social_good)
catEDA <- diagnose_category(master_social_good)

capitals <- c("Adelaide","Australian","Brisbane","Darwin","Greater","Melbourne","Perth","Sydney")

seifa <- master_social_good %>% 
  select(c(sa4,IRSAD_Decile:IEO_Decile,remoteness_index)) %>% 
  mutate(City = sub(" .*", "", sa4)) %>% 
  distinct()
seifa$capital = seifa$City %in% capitals
seifa$capital[seifa$capital == T] = "Capital City"
seifa$capital[seifa$capital == F] = "Non-Capital City"
seifa$City = NULL


unemployment_seifa<- master_social_good %>% 
  group_by(sa4) %>% 
  summarise_at(vars(unemployment,labour_force),sum) %>% 
  inner_join(seifa) %>% 
  mutate(unemployment_rate = unemployment/labour_force*100 )
  


# Exploring the relationship with population and unemployment 
cols <-colnames(unemployment_seifa)[4:7]
indcies <- c("Index. Relative Socio-Econ. Ad. & Disad. Deciles",
             "Index. Relative Socio-Ecom. Disad. Deciles",
             "Index. Education & Occupation Deciles",
             "Index. Economic Resources Deciles"
             )
i <- 1
for(col in cols){
  title_col <- str_replace(col,'_Decile','')
  g <-  unemployment_seifa %>% 
    ggplot(aes_string(col, 'unemployment_rate', colour= 'labour_force'))+
    geom_jitter(height = 0)+
    scale_color_gradient(low="light blue", high="dark blue",name= 'Labour Force')+
    labs(title=paste(title_col,"vs. Unemployment Rate"),
         y="Unemployment Rate (%)", 
         x=indcies[i]) 
  print(g)
  ggsave(paste(dir,title_col,"_v_umeployed.png",sep = ""))
  i = i+1
}


i <- 1
for(col in cols){
  title_col <- str_replace(col,'_Decile','')
  g <-  unemployment_seifa %>% 
    ggplot(aes_string(col, 'unemployment_rate', colour= 'capital'))+
    geom_jitter(height = 0)+
    scale_color_discrete(name= 'Capital')+
    labs(title=paste(title_col,"vs. Unemployment Rate"),
         y="Unemployment Rate (%)", 
         x=indcies[i]) 
  print(g)
  ggsave(paste(dir,title_col,"_v_capital.png",sep = ""))
  i = i+1
}

g <-  unemployment_seifa %>% 
  ggplot(aes(unemployment, unemployment_rate, colour= capital))+
  geom_jitter(height = 0)+
  scale_color_discrete(name= 'Capital')+
  labs(title=paste("Labour Force vs. Unemployment Rate"),
       y="Unemployment Rate (%)", 
       x="No. Unemployed") 
print(g)
ggsave(paste(dir,"Unemployment_v_labour_force.png",sep = ""))


  