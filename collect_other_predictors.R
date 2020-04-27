Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_251') # Comment out if RJDMX works out of teh box for you
library(rJava) #this too

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

library(raustats)
# set this directory to place you wan to store teh data files
dir <- "C:\\Users\\HP\\Documents\\Uni Stuff\\2020\\36103 Statistical Thinking for Data Science\\AT2\\stds-drought"

#Remoteness data by category and postcode
rem_tab <- abs_cat_tables("1270.0.55.005",include_urls = T)
rem_url <- rem_tab$path_zip[7] 
rem_file <-  abs_cat_unzip(abs_cat_download(rem_url,dir),dir)
remotness <- read_xls(rem1,sheet = 'Table 3')

#SEFIA 1986 - 2006
#teh method of storgae for all of them is diffrent for some bizzare reason

tab2006 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '2006')

url_2006 <- tab2006$path_xls[3] 
file2006 <- abs_cat_download(url_2006,dir)
seifa2006 <- read_xls(r1,sheet = 'Table 1',skip =4)

tab2001 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '2001')
url_2001 <- tab2001$path_zip
folder2001<-abs_cat_unzip(abs_cat_download(url_2001,dir),dir)
seifa2001 <- read_xls(folder2001[5,1])

tab1996 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '1996')
url_1996 <- tab1996$path_zip
folder1996<-abs_cat_unzip(abs_cat_download(url_1996,dir),dir)

seifa1996 <- NULL
for(i in 1:(length(folder1996)-1)){
  xls<-read_xls(folder1996[i])
  seifa1996 <- bind_rows(seifa1996,xls)
}

tab1991 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '1991')
url_1991 <- tab1991$path_zip
folder1991<-abs_cat_unzip(abs_cat_download(url_1991,dir),dir)
seifa1991 <- read_xls(folder1991[1])

tab1986 <- abs_cat_tables("2033.0.55.001",include_urls = T,releases = '1986')
url_1986 <- tab1986$path_zip
folder1986<-abs_cat_unzip(abs_cat_download(url_1986,dir),dir)

seifa1986 <- read_xls(folder1986[1])


