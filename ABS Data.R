### Get data directly from the ABS (Australian Bureau of Statistics) directory

## Libraries
library(raustats)
library(dplyr)
library(stringr)

## Download a specific table
# NOTE: package tutorial in https://cran.r-project.org/web/packages/raustats/vignettes/raustats_introduction.html 
capex_q <- abs_cat_stats("6291.0.55.001", tables="Table.+16b\\D")

# Most granular geographical level can be identified by ">>>"
capex_q <- capex_q %>%
  filter(grepl(">>>", data_item_description, ignore.case=TRUE))  %>%
  mutate(territory = str_replace_all(word(data_item_description,1,sep = "\\;"),'>>> ','')) %>%
  mutate(upper_territory = word(territory,1,sep = "\\-"))

unique(capex_q$territory) #85 territories
unique(capex_q$upper_territory) #51-ish when territories from a main city are aggregated

# Filter by the variables to be used
unemployed <- capex_q %>% 
  filter(grepl("Unemployment rate ;  Persons", data_item_description, ignore.case=TRUE))
  

# Example of visualisation
library(ggplot2)

to_plot <- unemployed %>% 
  filter(grepl("Sydney", territory, ignore.case=TRUE))

ggplot(data=to_plot) +
  geom_line(aes(x=date, y=value/10^3, colour=territory)) +
  scale_x_date(date_labels="%b\n%Y") +
  scale_y_continuous(limits=c(0, NA)) +
  labs(title="Australian unemployment rate, by SA4",
       y="Unemployment rate", x=NULL) +
  guides(colour = guide_legend(title=NULL)) + 
  theme(plot.title = element_text(hjust=0.5),
        legend.box = "horizontal",
        legend.position = "bottom",
        axis.text.x=element_text(angle=0, size=8))

