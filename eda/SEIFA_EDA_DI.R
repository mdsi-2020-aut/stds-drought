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
load("C:/Users/D/Desktop/UTS_MasterDataScience&InnovationSubjects/2020_autumn_stds/stds-drought/data/master_data.RData")
dir <- "eda/prof_charts/"

#SEIFA EDA
#Diagnostic report on data quality
#master_social_good %>%
#  diagnose_report(output_format = "html")

#nothing missing; all missing values removed before merging
missmap(master_social_good, main = "Missing values vs observed") #0% missing values

#Visualising distributions - continuous; use histogram

summary(master_social_good)
# date              unemployed      population     territory_sa4     
# Min.   :2006-01-01   Min.   :  570   Min.   : 27801   Length:15390      
# 1st Qu.:2009-07-01   1st Qu.: 3989   1st Qu.:127171   Class :character  
# Median :2013-02-01   Median : 6060   Median :182056   Mode  :character  
# Mean   :2013-01-30   Mean   : 7280   Mean   :210216                     
# 3rd Qu.:2016-09-01   3rd Qu.: 9319   3rd Qu.:262458                     
# Max.   :2020-03-01   Max.   :37202   Max.   :699356                     
# IRSAD_Decile    IRSD_Decile      IER_Decile      IEO_Decile   
# Min.   :1.557   Min.   :2.027   Min.   :2.001   Min.   :1.815  
# 1st Qu.:4.514   1st Qu.:4.251   1st Qu.:4.028   1st Qu.:3.767  
# Median :5.475   Median :5.208   Median :5.213   Median :5.010  
# Mean   :5.908   Mean   :5.621   Mean   :5.365   Mean   :5.420  
# 3rd Qu.:7.309   3rd Qu.:6.930   3rd Qu.:6.608   3rd Qu.:6.455  
# Max.   :9.994   Max.   :9.751   Max.   :9.719   Max.   :9.963  
# Population      remoteness_index  census_census_year      females      
# Min.   : 117397   Min.   :1.000    Min.   :2006   Min.   :0.4146  
# 1st Qu.: 420687   1st Qu.:1.008    1st Qu.:2006   1st Qu.:0.5011  
# Median : 600561   Median :1.161    Median :2011   Median :0.5066  
# Mean   : 720660   Mean   :1.785    Mean   :2011   Mean   :0.5035  
# 3rd Qu.: 876639   3rd Qu.:2.318    3rd Qu.:2016   3rd Qu.:0.5115  
# Max.   :2689149   Max.   :4.548    Max.   :2016   Max.   :0.5214  
# males         english_only    other_lang_and_english_very_well
# Min.   :0.4786   Min.   :0.3478   Min.   :0.01243                 
# 1st Qu.:0.4885   1st Qu.:0.7330   1st Qu.:0.02808                 
# Median :0.4934   Median :0.8401   Median :0.05542                 
# Mean   :0.4965   Mean   :0.7970   Mean   :0.08082                 
# 3rd Qu.:0.4989   3rd Qu.:0.9005   3rd Qu.:0.11173                 
# Max.   :0.5854   Max.   :0.9464   Max.   :0.30387                 
# other_lang_and_english_well other_lang_and_english_not_well
# Min.   :0.00340             Min.   :0.0003843              
# 1st Qu.:0.01178             1st Qu.:0.0043032              
# Median :0.02482             Median :0.0105213              
# Mean   :0.03949             Mean   :0.0181160              
# 3rd Qu.:0.05656             3rd Qu.:0.0211983              
# Max.   :0.16050             Max.   :0.1121619              
# other_lang_and_english_not_atall language_and_proficiency_not_stated
# Min.   :0.0002796                Min.   :0.02668                    
# 1st Qu.:0.0012737                1st Qu.:0.04166                    
# Median :0.0028002                Median :0.04933                    
# Mean   :0.0051851                Mean   :0.05739                    
# 3rd Qu.:0.0059292                3rd Qu.:0.06697                    
# Max.   :0.0350101                Max.   :0.17752                    
# language_stated_proficiency_not_stated    age_0_4          age_10_14      
# Min.   :0.0001619                      Min.   :0.04150   Min.   :0.02407  
# 1st Qu.:0.0008543                      1st Qu.:0.05899   1st Qu.:0.06062  
# Median :0.0014423                      Median :0.06359   Median :0.06640  
# Mean   :0.0020510                      Mean   :0.06456   Mean   :0.06589  
# 3rd Qu.:0.0025518                      3rd Qu.:0.06964   3rd Qu.:0.07285  
# Max.   :0.0111439                      Max.   :0.09195   Max.   :0.08947  
# age_100_over         age_15_19         age_20_24         age_25_29      
# Min.   :0.000e+00   Min.   :0.03833   Min.   :0.03682   Min.   :0.03612  
# 1st Qu.:9.272e-05   1st Qu.:0.06061   1st Qu.:0.05451   1st Qu.:0.05187  
# Median :1.333e-04   Median :0.06526   Median :0.06433   Median :0.06247  
# Mean   :1.427e-04   Mean   :0.06495   Mean   :0.06470   Mean   :0.06561  
# 3rd Qu.:1.892e-04   3rd Qu.:0.06949   3rd Qu.:0.07162   3rd Qu.:0.07251  
# Max.   :4.398e-04   Max.   :0.08352   Max.   :0.12230   Max.   :0.15598  
# age_30_34         age_35_39         age_40_44         age_45_49      
# Min.   :0.03976   Min.   :0.04343   Min.   :0.05423   Min.   :0.05956  
# 1st Qu.:0.05615   1st Qu.:0.06247   1st Qu.:0.06747   1st Qu.:0.06755  
# Median :0.06512   Median :0.06891   Median :0.07149   Median :0.07097  
# Mean   :0.06765   Mean   :0.06917   Mean   :0.07094   Mean   :0.07067  
# 3rd Qu.:0.07456   3rd Qu.:0.07495   3rd Qu.:0.07458   3rd Qu.:0.07404  
# Max.   :0.13580   Max.   :0.09830   Max.   :0.08820   Max.   :0.08293  
# age_5_9          age_50_54         age_55_59         age_60_64      
# Min.   :0.03032   Min.   :0.05172   Min.   :0.04432   Min.   :0.02632  
# 1st Qu.:0.06160   1st Qu.:0.06485   1st Qu.:0.05776   1st Qu.:0.04718  
# Median :0.06568   Median :0.06754   Median :0.06279   Median :0.05329  
# Mean   :0.06575   Mean   :0.06739   Mean   :0.06294   Mean   :0.05465  
# 3rd Qu.:0.07043   3rd Qu.:0.07063   3rd Qu.:0.06772   3rd Qu.:0.06160  
# Max.   :0.09556   Max.   :0.08363   Max.   :0.08954   Max.   :0.08809  
# age_65_69         age_70_74          age_75_79         age_80_84       
# Min.   :0.01457   Min.   :0.007344   Min.   :0.00372   Min.   :0.002408  
# 1st Qu.:0.03639   1st Qu.:0.028009   1st Qu.:0.02151   1st Qu.:0.015448  
# Median :0.04422   Median :0.033486   Median :0.02758   Median :0.020787  
# Mean   :0.04488   Mean   :0.034637   Mean   :0.02719   Mean   :0.020141  
# 3rd Qu.:0.05198   3rd Qu.:0.041020   3rd Qu.:0.03298   3rd Qu.:0.024999  
# Max.   :0.08709   Max.   :0.070221   Max.   :0.05096   Max.   :0.035714  
# age_85_89          age_90_94           age_95_99        
# Min.   :0.001055   Min.   :0.0003382   Min.   :0.0000634  
# 1st Qu.:0.009271   1st Qu.:0.0035670   1st Qu.:0.0008248  
# Median :0.012169   Median :0.0049509   Median :0.0011310  
# Mean   :0.012050   Mean   :0.0049335   Mean   :0.0011657  
# 3rd Qu.:0.015372   3rd Qu.:0.0062635   3rd Qu.:0.0014661  
# # Max.   :0.023778   Max.   :0.0100969   Max.   :0.0024522   

diagnose(master_social_good)
# A tibble: 41 x 6
# variables        types     missing_count missing_percent unique_count unique_rate
# <chr>            <chr>             <int>           <dbl>        <int>       <dbl>
#   1 date             Date                  0               0          171     0.0111 
# 2 unemployed       numeric               0               0         8968     0.583  
# 3 population       numeric               0               0        14500     0.942  
# 4 territory_sa4    character             0               0           87     0.00565
# 5 IRSAD_Decile     numeric               0               0          261     0.0170 
# 6 IRSD_Decile      numeric               0               0          261     0.0170 
# 7 IER_Decile       numeric               0               0          261     0.0170 
# 8 IEO_Decile       numeric               0               0          261     0.0170 
# 9 Population       numeric               0               0          261     0.0170 
# 10 remoteness_index numeric               0               0           74     0.00481
# # ... with 31 more rows 

#Consider Unique value : Variables with a unique value (unique_count = 1) are 
#considered to be excluded from data analysis.

numEDA <- diagnose_numeric(master_social_good)
catEDA <- diagnose_category(master_social_good)

master_social_good %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = master_social_good$unemployed), binwidth = 1000)


# Exploring the relationship 
# and age

master_social_good %>% 
  ggplot(aes(IRSAD_Decile, unemployed, colour=IRSAD_Decile))+
  geom_jitter(height = 0)+
  labs(title="IRSAD vs. Unemployed by Census years",
       y="No. Unemployed", 
       x="Index. Relative Socio-Economic Advantage & Disadvantage") +
  facet_wrap(~ master_social_good$census_year)

master_social_good %>% 
  ggplot(aes(IRSD_Decile, unemployed, colour=IRSD_Decile))+
  geom_jitter(height = 0)+
  labs(title="IRSD vs. Unemployed by Census years",
       y="No. Unemployed", 
       x="Index. Relative Socio-Economic Disadvantage") +
  facet_wrap(~ master_social_good$census_year)

master_social_good %>% 
  ggplot(aes(IEO_Decile, unemployed, colour=IEO_Decile))+
  geom_jitter(height = 0)+
  labs(title="IEO vs. Unemployed by Census years",
       y="No. Unemployed", 
       x="Index. Education & Occupation") +
  facet_wrap(~ master_social_good$census_year)

master_social_good %>% 
  ggplot(aes(IER_Decile, unemployed, colour=IER_Decile))+
  geom_jitter(height = 0)+
  labs(title = "IER vs. Unemployed by Census years",
       y="No. Unemployed", 
       x="Index. Economic Resources") +
  facet_wrap(~ master_social_good$census_year)

#curious about boxplot but not as expressive
master_social_good %>% 
  ggplot(aes(IER_Decile,unemployed))+
  geom_boxplot()+
  labs(title = "IER vs. Unemployed",
       y="No. Unemployed", 
       x="Index. Economic Resources") +
  facet_wrap(~ master_social_good$census_year)



  