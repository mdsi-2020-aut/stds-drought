#Just a script to get 

library(tidyverse)


#downloaded at https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument#Data
SA2_SA4<-read_csv("SA2_2016_AUST.csv")

#https://data.gov.au/dataset/ds-dga-1646f764-82ad-4c21-b49c-63480f425a4a/details?q=Coding%20Indexes
post_SA2<-read_csv("2016_LOCALITY_2016_SA2_CODING_INDEX.csv")

post_SA2 <- post_SA2 %>%
    select(c('POSTCODE','SA2_MAINCODE')) %>%
    distinct()

SA2_SA4 <-   SA2_SA4 %>%
    select(c('SA2_MAINCODE_2016','SA4_NAME_2016'))%>%
    distinct()
  
colnames(SA2_SA4)[1] <- 'SA2_MAINCODE'

post_sa2_sa4<-inner_join(post_SA2,SA2_SA4)
save(post_sa2_sa4, file="data/post_sa2_sa4.RData")
