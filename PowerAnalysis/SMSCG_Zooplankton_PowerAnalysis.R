#Suisun Marsh Salinity Control Gate
#Zooplankton
#Power analysis to determine amount of required sampling

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(lubridate) #working with dates
library(zooper) #Bay-Delta zooplankton data
library(pwr) #power analysis

#read in data--------------

#results from the modeling that compared SMSCG action and non-action years
modout <- read_csv("./PowerAnalysis/zooplanktonmodel.csv") %>% 
  clean_names() %>% 
  rename("yes_action" = smsc_gaction) %>% 
  glimpse()

#format the zoop modeling data------------
#results are divided by month and taxa
#presumably the values are estimated biomass of carbon
#combine taxa within month but for now keep months separate

#summarize action and no action results by month
modout_sum_mo <- modout %>% 
  group_by(month) %>% 
  summarise()






#get data from zooper package-----------------------








