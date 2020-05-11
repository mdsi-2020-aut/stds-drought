### Get data directly from the ABS (Australian Bureau of Statistics) directory
# NOTE: package tutorial in https://cran.r-project.org/web/packages/raustats/vignettes/raustats_introduction.html 

#---------------------------------------------------------------------------------------------------------------------------------------------------#
###### R Environment - Load Data ######
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#### ENVIRONMENT ####

### Clean current environment and set the working directory
rm(list = ls())
#setwd('~/UTS/STDS/AT2/stds-drought')

### Install and/or load libraries to be used
#install.packages(c('raustats','ggplot2','dplyr','stringr','scales','ASGS') #To install all required libraries
library(raustats)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ASGS)

#### GET DATA ####

### Download table
# Catalogue: 6291.0.55.001 - Labour Force, Australia, Detailed - Electronic Delivery, Mar 2020
# Table: Table 16b. Labour force status by Labour market region (ASGS) and Sex, Annual averages of the previous 12 months
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6291.0.55.001Mar%202020?OpenDocument
labour_abs <- abs_cat_stats("6291.0.55.001", tables="Table.+16b\\D")


#---------------------------------------------------------------------------------------------------------------------------------------------------#
###### unemployment with required information ######
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#### FILTER DATA ####
### Getting unemployed total
### Target variable: "Unemployed total ;  Persons"
unemployment <- labour_abs %>% 
  filter(grepl("Unemployed total ;  Persons", data_item_description, ignore.case=TRUE)) 

### unemployment description
warning(paste0("UNEMPLOYMENT unemployment INFORMATION","\n",
               "Source: ABS","\n",
               "Catalogue ID: ",unique(unemployment$catalogue_no),"\n",
               "Publication title: ",unique(unemployment$publication_title),"\n",
               "Table number: ",unique(unemployment$table_no),"\n",
               "Table title: ",unique(unemployment$table_title),"\n",
               "Variable: Unemployment rate - Persons","\n",
               "Series type: ",unique(unemployment$series_type),"\n",
               "Start: ",unique(unemployment$series_start),"\n",
               "End: ",unique(unemployment$series_end),"\n",
               "Number of observations: ",unique(unemployment$no_obs),"\n",
               "Freq: ",unique(unemployment$freq),"\n",
               "Unit: ",unique(unemployment$unit)))

### Relevant variables -we trunacate the unemployed value
unemployment <- unemployment %>% 
  select(date,value,data_item_description) %>% 
  mutate(unemployed = trunc(value*1000)) %>% 
  filter(grepl(">>>|Australian|Hobart", data_item_description, ignore.case=TRUE)) %>% # SA4 geographical divisions can be identified by ">>>"
  mutate(territory_sa4 = str_replace_all(word(data_item_description,1,sep = "\\;"),'>>> |>> |> ',''))

### Territories list
uniques_sa4 <- unemployment %>% select(data_item_description,territory_sa4) %>% unique()
uniques_sa4$territory_sa4
nrow(uniques_sa4)


### unemployment
unemployment <- unemployment %>% 
  select(date,unemployed,territory_sa4)



### Getting population
### First get employemnt to population ratio
### Target variable: "Employed to population ratio ;  Persons"
employ_rat <- labour_abs %>% 
  filter(grepl("Employed to population ratio ;  Persons", data_item_description, ignore.case=TRUE)) %>% 
  select(date,value,data_item_description) %>% 
  mutate(employment_pop_ratio = value/100) %>% 
  filter(grepl(">>>|Australian|Hobart", data_item_description, ignore.case=TRUE)) %>% # SA4 geographical divisions can be identified by ">>>"
  mutate(territory_sa4 = str_replace_all(word(data_item_description,1,sep = "\\;"),'>>> |>> |> ','')) %>% 
  select(date,employment_pop_ratio,territory_sa4)


### Now get employment numbers
### Target variable: "Employed total ;  Persons"
employ_pop <- labour_abs %>% 
  filter(grepl(" Employed total ;  Persons", data_item_description, ignore.case=TRUE)) %>% 
  select(date,value,data_item_description) %>% 
  mutate(employed = trunc(value*1000)) %>% 
  filter(grepl(">>>|Australian|Hobart", data_item_description, ignore.case=TRUE)) %>% # SA4 geographical divisions can be identified by ">>>"
  mutate(territory_sa4 = str_replace_all(word(data_item_description,1,sep = "\\;"),'>>> |>> |> ','')) %>% 
  select(date,employed,territory_sa4)


###Join tables together and divide to get pop
pop <- employ_pop %>% 
  inner_join(employ_rat) %>% 
  mutate(population = trunc(employed/employment_pop_ratio))

###And join all tables
unemployment <- unemployment %>% 
  inner_join(pop) %>% 
  select(date,unemployed,population,territory_sa4)

### Save R object
save(unemployment,file="data/unemployment.RData")

#### VISUALISATION ####

# Unemployment rate time series - Visualisation
to_plot <- unemployment %>% 
  filter(grepl("Australian", territory_sa4, ignore.case=TRUE))

x11()
ggplot(data=to_plot) +
  geom_line(aes(x=date, y=unemployment_rate, colour=territory_sa4)) +
  scale_x_date(date_labels="%b\n%Y") +
  scale_y_continuous(limits=c(0, NA)) +
  labs(title="Unemployment rate - SA4",
       y="Unemployment rate", x=NULL) +
  guides(colour = guide_legend(title=NULL)) + 
  theme(plot.title = element_text(hjust=0.5),
        legend.box = "horizontal",
        legend.position = "bottom",
        axis.text.x=element_text(angle=0, size=8))


#---------------------------------------------------------------------------------------------------------------------------------------------------#
###### Exploratory Data Analysis (EDA) ######
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#### TABLES ####

### Check structure 
str(unemployment)
#> Set territory_sa4 as factor
unemployment$territory_sa4 <- factor(unemployment$territory_sa4,levels = sort(unique(unemployment$territory_sa4)))
levels(unemployment$territory_sa4)
str(unemployment)

### Check initial records
head(unemployment)

### Duplicates
sum(duplicated(unemployment))
sum(duplicated(unemployment[,c("date","territory_sa4")]))
#>>No duplicated rows
#>>No duplicated combinations of date*territory_as4

### Variables summary
summary(unemployment)
#> Check number of observations per combinations of date*territory_as4
count_obs <- unemployment %>% select(territory_sa4) %>% group_by(territory_sa4) %>% tally()
summary(count_obs)
#>> 87 combinations of date*territory_as4, 247 observations for each

### Missing values
sapply(unemployment, function(x) sum(is.na(x)))
#>>No missing values

### AS4 with higher rates
high_rates <- unique(unemployment[which(unemployment$unemployment_rate>11),'territory_sa4'])
high_rates

#### VISUALISATIONS ####

### Time series per AS4
init <- 1
for (i in 1:(round(nrow(uniques_sa4)/3,0))) {
  g <- ggplot(unemployment[which(unemployment$territory_sa4 %in% levels(unemployment$territory_sa4)[init:(init+2)]),], aes(date, unemployment_rate)) +
        geom_line() +
        ggtitle("Unemployment rate time series, by AS4") +
        xlab("Date") + ylab("Unemployment rate") +
        scale_x_date(labels=date_format("%m-%y"))+
        theme(plot.title = element_text(lineheight=.8,face="bold",size = 20)) +
        theme(text = element_text(size=18)) + facet_grid(. ~ territory_sa4)
      
  init <- init + 3
  
  print(g)
}

### Time series per AS4 - HIGH RATES
init <- 1
for (i in 1:(round(length(high_rates)/5,0))) {
  g <- ggplot(unemployment[which(unemployment$territory_sa4 %in% levels(unemployment$territory_sa4)[init:(init+4)]),], aes(date, unemployment_rate)) +
    geom_line() +
    ggtitle("Unemployment rate time series, by AS4") +
    xlab("Date") + ylab("Unemployment rate") +
    scale_x_date(labels=date_format("%m-%y"))+
    theme(plot.title = element_text(lineheight=.8,face="bold",size = 20)) +
    theme(text = element_text(size=18)) + facet_grid(. ~ territory_sa4)
  
  init <- init + 5
  
  print(g)
}
### Lat, Lng to AS4 example
# The Australian Statistical Geography Standard ('ASGS') is a set of shapefiles by the Australian Bureau of Statistics.
# https://cran.r-project.org/web/packages/ASGS.foyer/ASGS.foyer.pdf
# https://rdrr.io/cran/ASGS.foyer/src/R/latlon2SA.R
# https://rdrr.io/github/HughParsonage/ASGS.foyer/man/install_ASGS.html
# install_ASGS(temp.tar.gz = tempfile(fileext = ".tar.gz"), overwrite = FALSE, #To get the dictionary
#              lib = .libPaths()[1], repos = getOption("repos"),
#              type = getOption("pkgType", "source"), .reinstalls = 4L,
#              verbose = FALSE)

#View(STE_2016_simple)

drought_example <- data.frame(station = c(123,456,789,963), 
                              city = c('Sydney','Melbourne','Perth','Darwin'), 
                              lat = c(-33.865143,-37.840935,-31.953512, -12.462827),
                              lng = c(151.209900,144.946457,115.857048,130.841782))
drought_example$territory_sa4 <- latlon2SA(drought_example$lat, drought_example$lng, to = "SA4", yr = "2016")
