library(RJSDMX)
library(rsdmx)
library(readabs)
library(tidyverse)

org <- "ABS"
Sys.setenv(R_READABS_PATH = "data/")

#use RJSDMX to explore the structure
#get SEIFA flows ID
flows <- getFlows(org,"*SEIFA*")
seifa_pos_dataflow <- "ABS_SEIFA2016_POA" 

#get list of dimensions of "SEIFA per post code" DSD
seifa_pos_dim <- getDimensions(org, seifa_pos_dataflow)

#get all the codelists
seifa_pos_codelist <- map(.x = names(flatten(seifa_pos_dim)),
                          flow = seifa_pos_dataflow,
                          provider = org,
                          .f = getCodes) %>% set_names(names(flatten(seifa_pos_dim)))

#get the entire dataseries at once will return error
#do looping per SEIFAINDEXTYPE
seifa_data <- data.frame()
for (idx in seifa_pos_codelist[2]){
  seifa_data <- rbind(seifa_data, as.data.frame(readSDMX(providerId = "ABS", 
                                                         resource = "data", 
                                                         flowRef = seifa_pos_dataflow,
                                                         key = list(NULL, names(idx), NULL), 
                                                         dsd = TRUE), 
                                                labels = TRUE))
}

#use readabs package
test <- read_abs(cat_no = "6227.0",tables = 1)
