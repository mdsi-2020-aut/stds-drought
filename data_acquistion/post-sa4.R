#Just a script to get a table with postcodes mapped to sa4 (or vice versa)

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
# #Post         Unemploy
# Hobart        Greater Hobart
# Central West  New South Wales - Central West
# North West    Victoria - North West
# Western Australia - Outback (North) Western Australia - Outback (South)    Western Australia - Outback (North and South)
# South East  Tasmania - South East
# West and North West  Tasmania - West and North West

unemploy <- c("Greater Hobart","New South Wales - Central West","Victoria - North West",
              "Western Australia - Outback (North and South)","Western Australia - Outback (North and South)",
              "Tasmania - South East","Tasmania - West and North West")
sa4 <- c("Hobart","Central West","North West","Western Australia - Outback (North)",
         "Western Australia - Outback (South)","South East","West and North West")

for(i in 1:length(sa4)){
  post_sa2_sa4$SA4_NAME_2016[post_sa2_sa4$SA4_NAME_2016 == sa4[i]] <- unemploy[i]
}



save(post_sa2_sa4, file="data/post_sa2_sa4.RData")





#to load for use do 
load("data/post_sa2_sa4.RData")
