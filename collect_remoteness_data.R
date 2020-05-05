library(readxl)
library(dplyr)
library(tidyverse)
library(raustats)
library(stringr)

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
U_AUST_orig <- abs_cat_stats("6291.0.55.001", tables="Table.+16b\\D")
glimpse(U_AUST_orig)

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
arrange(test, "2016")
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
SA1_4 <- select(SA1_4, SA1_7DIGITCODE_2016, SA4_NAME_2016) 
glimpse(SA1_4)

#Tidy Unemployment data (with adaptation of Ana's code)
# Most granular geographical level can be identified by ">>>"
U_AUST_geo <- U_AUST_orig %>%
  filter(grepl(">>>", data_item_description, ignore.case=TRUE))  %>%
  mutate(territory = str_replace_all(word(data_item_description,1,sep = "\\;"),'>>> ','')) %>%
  mutate(upper_territory = word(territory,1,sep = "\\-"))
U_AUST_geo$territory = str_trim(U_AUST_geo$territory, side = "both")
glimpse(U_AUST_geo)

#Tidy Unemployment data (with adaptation of Ana's code)(Cont.)
# Filter by the variables to be used
U_AUST <- U_AUST_geo %>% 
  filter(grepl("Unemployment rate ;  Persons", data_item_description, ignore.case=TRUE)) %>%
  arrange(territory)
glimpse(U_AUST)
distinct(U_AUST, territory)

# Merge SA1 Population data to SA1 Remoteness data
SA1_POP_RA <- left_join(POP, RA, by = c("SA1" = "SA1_7DIGITCODE_2016"))
names(SA1_POP_RA) <- sub("RA_NAME_2016", "RA_type", names(SA1_POP_RA))
glimpse(SA1_POP_RA)

# Add Remoteness Rank value to merged SA Pop. & Remoteness data  
RA_type_rank <- function(a) {
  if(a == "Major Cities of Australia"){1} else
    if(a == "Inner Regional Australia") {2} else
      if(a == "Outer Regional Australia") {3} else
        if(a == "Remote Australia") {4} else
          if(a == "Very Remote Australia") {5} else
          {0}
}

SA1_POP_RA <- SA1_POP_RA %>%
  rowwise() %>%
  mutate(RA_type_rank = RA_type_rank(a = RA_type)) %>%
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

test_a <- filter(SA1_4_POP_RA, RA_type_rank == 0)
glimpse(test_a)
# 0 observations - deosn't make sense why?
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

#Add a population weighting factor to each SA1 to the Population and Remoteness SA1 and SA4 dataframe
SA1_4_POP_RA_W <- SA1_4_POP_RA %>%
  group_by(SA4_name) %>%
  summarize(SA4_Pop = sum(Pop))
SA1_4_POP_RA_W
SA1_4_POP_RA_SumPop <- left_join(SA1_4_POP_RA, SA1_4_POP_RA_W)
SA1_4_POP_RA_SumPop_PopW <- mutate(SA1_4_POP_RA_SumPop, SA1_Pop_Wght = Pop / SA4_Pop)
SA1_4_POP_RA_SumPop_PopW # Would be good to check weights sum to 1 for each SA4_Name

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
U_RA_AUST <- left_join(U_AUST, SA4_RAPopWd, by = c("territory" = "SA4_name"))
glimpse(U_RA_AUST)

glimpse(U_AUST)
distinct(U_AUST, territory)

glimpse(SA4_RAPopWd)
