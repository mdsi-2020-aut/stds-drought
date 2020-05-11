library(tidyverse)
library(lubridate)
library(Amelia)
library(corrplot)
library(dlookr)

# Research Questions
# What socio-economic, demographic, environmental and geospatial factors should be 
#included in an unemployment insurance premium model?
# Other questions
#   In what ways does unemployment differ between urban, regional and remote areas? 
#   What seasonal patterns are significant to provide an insurer better understanding 
#   and planning on fluctuating claims payments? (WHY IS THIS IMPORTANT TO THE CLIENT?)

#   How effective is it to consider environmental externalities to model unemployment seasonality?


# Load Data
load("data/unemploy_seifa.RData")
dir <- "eda/prof_charts/"

#SEIFA EDA
#Diagnostic report on data quality
unemploy_seifa %>%
  diagnose_report(output_format = "html")

#nothing missing; all missing values removed before merging
missmap(unemploy_seifa, main = "Missing values vs observed") #0% missing values

#Visualising distributions - continuous; use histogram

summary(unemploy_seifa)
# IRSAD_Decile    IRSD_Decile      IER_Decile      IEO_Decile      Population           Year      SA4_NAME_2016     
# Min.   :1.557   Min.   :2.027   Min.   :2.001   Min.   :1.815   Min.   : 117397   Min.   :2006   Length:14877      
# 1st Qu.:4.499   1st Qu.:4.201   1st Qu.:4.020   1st Qu.:3.767   1st Qu.: 421048   1st Qu.:2006   Class :character  
# Median :5.498   Median :5.238   Median :5.242   Median :5.059   Median : 612936   Median :2011   Mode  :character  
# Mean   :5.929   Mean   :5.651   Mean   :5.382   Mean   :5.473   Mean   : 729570   Mean   :2011                     
# 3rd Qu.:7.448   3rd Qu.:6.944   3rd Qu.:6.649   3rd Qu.:6.492   3rd Qu.: 888805   3rd Qu.:2016                     
# Max.   :9.994   Max.   :9.751   Max.   :9.719   Max.   :9.963   Max.   :2689149   Max.   :2016                     
# date            unemployment_rate
# Min.   :2006-01-01   Min.   : 1.357   
# 1st Qu.:2009-07-01   1st Qu.: 4.134   
# Median :2013-02-01   Median : 5.207   
# Mean   :2013-01-30   Mean   : 5.339   
# 3rd Qu.:2016-09-01   3rd Qu.: 6.371   
# Max.   :2020-03-01   Max.   :14.963 

diagnose(unemploy_seifa)
# A tibble: 9 x 6
# variables         types     missing_count missing_percent unique_count unique_rate
# <chr>             <chr>             <int>           <dbl>        <int>       <dbl>
#   1 IRSAD_Decile      numeric               0               0          261    0.0175  
# 2 IRSD_Decile       numeric               0               0          261    0.0175  
# 3 IER_Decile        numeric               0               0          261    0.0175  
# 4 IEO_Decile        numeric               0               0          261    0.0175  
# 5 Population        numeric               0               0          261    0.0175  
# 6 Year              numeric               0               0            3    0.000202
# 7 SA4_NAME_2016     character             0               0           87    0.00585 
# 8 date              Date                  0               0          171    0.0115  
# 9 unemployment_rate numeric               0               0        14876    1.00  

#Consider Unique value : Variables with a unique value (unique_count = 1) are 
#considered to be excluded from data analysis.

numEDA <- diagnose_numeric(unemploy_seifa)
catEDA <- diagnose_category(unemploy_seifa)

unemploy_seifa %>% 
  filter(unemployment_rate < .1) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = unemploy_seifa$unemployment_rate), binwidth = 1)

#The following is a list of numeric variables with anomalies greater than 5%
outlierEDA <- diagnose_outlier(unemploy_seifa) %>% 
  filter(outliers_ratio > 5) %>% 
  mutate(rate = outliers_mean / with_mean) %>% 
  arrange(desc(rate)) %>% 
  select(-outliers_cnt)




# Exploring the relationship
unemploy_seifa %>%
  filter(Year == 2006) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IRSD_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2011) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IRSD_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2016) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IRSD_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2006) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IRSAD_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2011) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IRSAD_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2016) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IRSAD_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()


unemploy_seifa %>%
  filter(Year == 2006) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IER_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2011) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IER_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2016) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IER_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()


unemploy_seifa %>%
  filter(Year == 2006) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IEO_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2011) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IEO_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()

unemploy_seifa %>%
  filter(Year == 2016) %>%
  filter(unemployment_rate>10.0) %>%   ggplot(mapping = aes(x = IEO_Decile, y = unemployment_rate )) +
  geom_point()+
  geom_jitter()




