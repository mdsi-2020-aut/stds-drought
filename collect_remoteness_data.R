###########################################################################
# Script to collect, clean and merge remoteness data with unemployment data
###########################################################################

# Assumptions 
#
# 1) When rolling up SA1 area Remoteness type/rank to SA4 areas
# the population denominator vale for each SA1 area is based on 2016
# population levels for the entire 1999-2019 range of dates captured
# within the unemployment dataset
#
# 2) Mapping of SA1 Remoteness areas to SA4 Unemployment levels is done based
# on 2016 ASGS SA definitions and mapping and does not take into account any 
# possible changes to this mapping that might have taken place as part of the
# 2011 ABS recalibration of ASGS areas
#
# 3) Incidence of 1,262 SA1 areas with 0 population does not materially impact
# proof of concept of merge of Remoteness data to Unemployment data. Suggest 
# an invesitigation to ratify this assumption takes place before analysis is
# advanced significantly

library(readxl)
library(tidyverse)
library(raustats)

# Read (from data) ABS excel download of 2016 Remoteness data
RA_2016_AUST <- read_excel("Data/RA_2016_AUST.xlsx")
glimpse(RA_2016_AUST)

# Read (from data) ABS excel download of 2011-19 population data
POP_AUST <- read_excel("Data/POP_AUST.xlsx")
glimpse(POP_AUST)

# Read (from data) ABS SA1-to-SA4 csv data
SA1_4 <- read_csv("Data/SA1_2016_AUST.csv")
glimpse(SA1_4)
distinct(SA1_4, SA4_NAME_2016)

# Read (from data) ABS Uneployment data
# Note: package tutorial in https://cran.r-project.org/web/packages/raustats/vignettes/raustats_introduction.html 
load("data/unemployment.RData")

# Tidy RA data
RA <- RA_2016_AUST %>%
  select(SA1_7DIGITCODE_2016, RA_NAME_2016) %>%
  group_by(SA1_7DIGITCODE_2016, RA_NAME_2016) %>%
  summarize()
glimpse(RA)

# Tidy population data
glimpse(POP_AUST)
is.na("POP_AUST")
# [1] FALSE
test <- select(POP_AUST, "2016")
names(test) <- sub("2016", "Pop", names(test))
any(test==0)
#[1] TRUE
test_a <- arrange(test, Pop)
glimpse(test_a)
count0 <- length(which(test_a == 0))
count0
#[1] 1295
# Not sure why 2016 contains 1,295 zero values - leave for now and reinvestigate 
# on refined data later

POP <- select(POP_AUST, SA1, "2016")
names(POP) <- sub("^2016", "Pop", names(POP))
glimpse(POP)


# Tidy SA1-to-SA4 data
SA1_4 <- select(SA1_4, SA1_7DIGITCODE_2016, SA4_NAME_2016) # Reduce the Number of columns
glimpse(SA1_4)


  # Amnd a handful of SA4 exeption names SA1_4 so they'll match SA4 names in the Unemployment data
amnd_unemploy <- c("Greater Hobart", "New South Wales - Central West", "Victoria - North West", "Western Australia - Outback (North and South)", "Western Australia - Outback (North and South)", "Tasmania - South East", "Tasmania - West and North West")
amnd_sa4 <- c("Hobart", "Central West", "North West", "Western Australia - Outback (North)", "Western Australia - Outback (South)", "South East", "West and North West")
for(i in 1:length(amnd_sa4)){    SA1_4$SA4_NAME_2016[SA1_4$SA4_NAME_2016 == amnd_sa4[i]] <- amnd_unemploy[i]   }
filter(SA1_4, SA4_NAME_2016 == "Greater Hobart"|SA4_NAME_2016 == "New South Wales - Central West"|SA4_NAME_2016 == "Victoria - North West"|SA4_NAME_2016 == "Western Australia - Outback (North and South)"|SA4_NAME_2016 == "Western Australia - Outback (North and South)"|SA4_NAME_2016 == "Tasmania - South East"|SA4_NAME_2016 == "Tasmania - West and North West") %>%
  distinct(SA4_NAME_2016)

#Tidy Unemployment - (already doen by virtue of Ana's code - no further coding necessary)

# Merge SA1 Population data to SA1 Remoteness data
SA1_POP_RA <- left_join(POP, RA, by = c("SA1" = "SA1_7DIGITCODE_2016"))
names(SA1_POP_RA) <- sub("RA_NAME_2016", "RA_type", names(SA1_POP_RA))
glimpse(SA1_POP_RA)

# Add Remoteness Rank value to merged SA Pop. & Remoteness data
SA1_POP_RA <- SA1_POP_RA %>% mutate(RA_type_rank = case_when(
  RA_type ==  "Major Cities of Australia" ~ 1,
  RA_type ==  "Inner Regional Australia" ~ 2, 
  RA_type ==  "Outer Regional Australia" ~ 3, 
  RA_type ==  "Remote Australia" ~ 4, 
  RA_type ==  "Very Remote Australia" ~ 5,
  TRUE ~ 0)) %>%
  filter(RA_type_rank != 0)
glimpse(SA1_POP_RA)

# Merge SA1 Population and Remoteness data to SA4 rollup levels
SA1_4_POP_RA <- left_join(SA1_POP_RA, SA1_4, by = c("SA1" = "SA1_7DIGITCODE_2016")) %>%
  select(SA1, Pop, RA_type, RA_type_rank, SA4_NAME_2016)
names(SA1_4_POP_RA) <- sub("SA4_NAME_2016", "SA4_name", names(SA1_4_POP_RA))
glimpse(SA1_4_POP_RA)
SA1_4_POP_RA %>% print(n = Inf)


test_a <- SA1_4_POP_RA
select(test_a, SA4_name, Pop)
count0 <- length(which(test_a == 0))
count0
# [1] 1262 zero values in "Pop" - why?
test_a <- SA1_4_POP_RA
select(test_a, SA4_name, RA_type_rank)
count0 <- length(which(test_a == 0))
count0
# [1] 1262 zero values in "RA_Type_rank" - why?
test_a <- select(SA1_4_POP_RA, SA4_name, Pop, RA_type, RA_type_rank)
test_a <- filter(test_a, Pop == 0)
glimpse(test_a)
#Observations: 1,262
#Variables: 4
#$ SA4_name     <chr> "Capital Region", "Capital Region", "Capital Region", "Capital Region", "Capital Region...
#$ Pop          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
#$ RA_type      <chr> "Major Cities of Australia", "Major Cities of Australia", "Major Cities of Australia", ...
#$ RA_type_rank <dbl> 1, 1, 1, 1, 3, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, ...
test_a %>% print(n = 500)
# Will need to access a file with SA1 names and merge with Pop data to resolve
# Unanswered question of why there is so many 1,262 SA1 areas with 0 Pop. values

#Add a population weighting factor to each SA1 to the Population and Remoteness SA1 and SA4 dataframe
SA1_4_POP_RA_W <- SA1_4_POP_RA %>%
  group_by(SA4_name) %>%
  summarize(SA4_Pop = sum(Pop))
SA1_4_POP_RA_W
SA1_4_POP_RA_SumPop <- left_join(SA1_4_POP_RA, SA1_4_POP_RA_W)
SA1_4_POP_RA_SumPop_PopW <- mutate(SA1_4_POP_RA_SumPop, SA1_Pop_Wght = Pop / SA4_Pop)
SA1_4_POP_RA_SumPop_PopW 

# Sample check that SA1_Pop_wght sum to 1 for an SA4_Name
glimpse(SA1_4_POP_RA)
SA1_4_POP_RA_test <- SA1_4_POP_RA %>%
  filter(SA4_name == "Adelaide - Central and Hills") %>%
  group_by(SA4_name) %>%
  summarise(totL = sum(Pop))
SA1_4_POP_RA_test
# A tibble: 1 x 2
#SA4_name                       totL
#<chr>                         <dbl>
#  1 Adelaide - Central and Hills 297617
# Checks out correctly

SA1_4_POP_RA_SumPop_PopW_test <- SA1_4_POP_RA_SumPop_PopW %>%
  filter(SA4_name == "Adelaide - Central and Hills") %>%
  group_by(SA4_name) %>%
  summarise(totL = sum(SA1_Pop_Wght))
SA1_4_POP_RA_SumPop_PopW_test

#Add a RA_type_rank_Wghtd for each SA1 to the Population and Remoteness SA1 and SA4 dataframe
SA1_4_POP_RA_SumPop_PopW_RATypeRankWghtd <- SA1_4_POP_RA_SumPop_PopW %>%
  mutate(RA_type_rank_wghtd = RA_type_rank * SA1_Pop_Wght)
SA1_4_POP_RA_SumPop_PopW_RATypeRankWghtd

# Construct final SA4_name X PopWtdRA_rank dataframe
SA4_RAPopWd <- SA1_4_POP_RA_SumPop_PopW_RATypeRankWghtd %>%
  group_by(SA4_name) %>%
  summarize(PopWtdRA_rank = sum(RA_type_rank_wghtd))
SA4_RAPopWd
SA4_RAPopWd %>% print(n = 100)

#Join PopWtdRA_rank to Unemployment data
unemployment$territory_sa4 = str_trim(unemployment$territory_sa4, side = "both")
unemployment_RAPopWtd <- left_join(unemployment, SA4_RAPopWd, by = c("territory_sa4" = "SA4_name"))
glimpse(unemployment_RAPopWtd)

glimpse(unemployment_RAPopWtd)
distinct(unemployment_RAPopWtd)

save(unemployment_RAPopWtd, file = "data/unemployment_remote.RData")
     