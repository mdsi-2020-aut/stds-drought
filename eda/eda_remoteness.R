#################################################################################
# SCRIPT OVERVIEW
# Exploratory data analysis (incl. data quality review of remoteness data)
#################################################################################

library(tidyverse)
library(raustats)
library(ggplot2)

# load data
load("data/unemployment_remote.RData")
UR <- unemployment_RAPopWtd
glimpse(UR)

# summarise data
summary(UR)
# date            unemployment_rate territory_sa4      PopWtdRA_rank  
#Min.   :1999-09-01   Min.   : 1.357    Length:21489       Min.   :1.000  
#1st Qu.:2004-10-01   1st Qu.: 4.292    Class :character   1st Qu.:1.005  
#Median :2009-12-01   Median : 5.477    Mode  :character   Median :1.082  
#Mean   :2009-11-30   Mean   : 5.640                       Mean   :1.625  
#3rd Qu.:2015-02-01   3rd Qu.: 6.708                       3rd Qu.:2.181  
#Max.   :2020-03-01   Max.   :14.980                       Max.   :4.548  

scatter <- ggplot(data = UR) + 
  geom_point(mapping = aes(x = PopWtdRA_rank, y = unemployment_rate))
print(scatter + labs(caption = "Suggests no linear relationship or one obscured by larger variation in unemployment over time", y = "Unemployment Rate", x = "Remoteness Rank"))
ggsave("RemoteEDA 1 Unemp+Remote Scatter All.pdf")

UR_200708_202001 <- UR %>%
  filter(date == "2007-08-01" | date == "2020-01-01")
glimpse(UR_200601_09111419_01)
scatter <- ggplot(data = UR_200601_09111419_01) + 
  geom_point(mapping = aes(x = PopWtdRA_rank, y = unemployment_rate, colour = date))
print(scatter + ggtitle("Aug 2007 (12.1%)  and Jan 2020 (4.6%)") + labs(caption = "Superficial glance suggests no linear relationship", y = "Unemployment Rate", x = "Remoteness Rank"))
ggsave("RemoteEDA 4 Unemp+Remote Scatter 2007008+20200101 Colour.pdf")

#try a facet??????????????????????????????????????????
scatter <- ggplot(data = UR_200708_202001) + 
  geom_point(mapping = aes(x = PopWtdRA_rank, y = unemployment_rate)) + 
  facet_wrap(~date, nrow = 1)
print(scatter + ggtitle("Aug 2007 (12.1%)  and Jan 2020 (4.6%)") + labs(caption = "Superficial glance suggests no linear relationship", y = "Unemployment Rate", x = "Remoteness Rank"))
ggsave("RemoteEDA 5 Unemp+Remote Scatter 2007008+20200101 Facet.pdf")

ggplot(data = UR, mapping = aes(y = unemployment_rate)) + 
  geom_boxplot()
ggsave("RemoteEDA 3 Unemp+Remote Boxplot.pdf")

summary(UR$unemployment_rate)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.357   4.292   5.477   5.640   6.708  14.980 

UR_outliers <- UR %>%
  filter(unemployment_rate > 1.5 * 6.708)
glimpse(UR_outliers)
#Observations: 546
#Variables: 4
#$ date              <date> 1999-09-01, 1999-10-01, 1999-11-01, 1999-12-01, 1999...
#$ unemployment_rate <dbl> 11.18449, 11.02701, 10.52270, 10.15838, 10.12754, 10....
#$ territory_sa4     <chr> "Sydney - South West", "Sydney - South West", "Sydney...
#$ PopWtdRA_rank     <dbl> 1.002162, 1.002162, 1.002162, 1.002162, 2.182397, 2.1...

n_distinct(UR_outliers$territory_sa4)
#[1] 23
UR_outliers_sa4 <- UR_outliers %>%
  group_by(territory_sa4) %>%
  summarise(mean(unemployment_rate), mean(PopWtdRA_rank))
UR_outliers_sa4
print(UR_outliers_sa4, n = "inf")
# A tibble: 23 x 3
#territory_sa4                     `mean(unemployment_rate)` `mean(PopWtdRA_rank)`
#<chr>                                                 <dbl>                 <dbl>
#  1 Adelaide - North                                       10.1                  1.02
#2 Adelaide - West                                        10.1                  1   
#3 Ballarat                                               10.9                  2.00
#4 Bendigo                                                10.2                  2.03
#5 Coffs Harbour - Grafton                                10.7                  2.18
#6 Hunter Valley exc Newcastle                            10.1                  1.63
#7 Latrobe - Gippsland                                    10.1                  2.20
#8 Logan - Beaudesert                                     10.9                  1.10
#9 Mandurah                                               11.1                  1.04
#10 Mid North Coast                                        11.4                  2.18
#11 Moreton Bay - North                                    10.9                  1.15
#12 Murray                                                 11.9                  2.23
#13 Newcastle and Lake Macquarie                           10.3                  1.01
#14 Queensland - Outback                                   12.3                  4.55
#15 Richmond - Tweed                                       10.9                  1.68
#16 South Australia - Outback                              10.3                  3.71
#17 Southern Highlands and Shoalhaven                      10.3                  2.00
#18 Sunshine Coast                                         11.0                  1.21
#19 Sydney - South West                                    10.7                  1.00
#20 Tasmania - South East                                  10.8                 NA   
#21 Tasmania - West and North West                         10.6                 NA   
#22 Townsville                                             10.9                  3.04
#23 Wide Bay                                               11.5                  2.10

