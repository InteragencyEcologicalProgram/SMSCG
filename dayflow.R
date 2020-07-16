#organize dayflow data

library(tidyverse)
load("~/salinity control gates/SMSCG/dayflowdata.RData")


dayflow = merge(dayflow, wytypes, by= "Year")

dayflow = filter(dayflow, Year %in% c(2005, 2006, 2017, 2002, 2009, 2016, 2018), 
                 Month %in% c(7:10), Region == "Sacramento Valley")

dayflow = rename(dayflow, Wateryear = Year)
write.csv(dayflow, "Delta_outflow_2018.csv")
