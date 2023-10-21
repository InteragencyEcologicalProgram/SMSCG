#Suisun Marsh Salinity Control Gate Action
#Phyto community composition across space and time

#Nick Rasmussen
#2023-10-20

#to do list-------
#keep in mind that 2018-2019, don't have enhanced sampling; it's just EMP and they don't sample east marsh
#stacked bar plots of algal groups by date and station
#stacked bar plot of algal groups by month and region
#maybe drop a few samples based on the quality check column
#need to assign samples to survey numbers; especially important for deciding when EMP and DFW samples should be combined
#for now, just group samples by region and month
#look at AWCA plots to see how I did this

#Packages------------------
library(tidyverse)
library(lubridate)
library(janitor)

#read in data------------

#read in phyto abundance data from SMSCG data package on EDI
abund <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=8c283ea7d1823824ccfa2f05d8056027")

#read in phyto taxonomy data from SMSCG data package on EDI
taxon <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=4c514654c71a7b8b930bdd031701c359")

#read in phyto station metadata from SMSCG data package on EDI
region <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=08de2a97cf2a3743af06e3ff6e0e9b39")

#format the station metadata file----------------

#drop lat/long because those are already in abundance data set
region_format <- region %>% 
  select(station, region) %>% 
  glimpse()

#format taxonomy data set------------------

#there are three variations for this one taxon; remove the ones we don't want
taxon_format <- taxon %>% 
  filter(!(taxon == "Teleaulax amphioxeia" & species=="prolonga")
         & !(taxon == "Teleaulax amphioxeia" & is.na(taxon_original))
         )

#add the taxonomy and station metadata to abundance data----------

#add taxonomy
at <- left_join(abund,taxon_format)

#add regions
atr <- left_join(at,region_format) %>% 
  #add month and year columns
  mutate(
    year = year(date),
    month = month(date)
  ) %>% 
  glimpse()

#summarize biovolume by algal group---------------

alg_grp_biov <- atr %>% 
  group_by(region,year,month,algal_group) %>% 
  summarise(total_biovolume = sum(biovolume_per_ml)) %>% 
  arrange(year,month,region) %>% 
  glimpse()

#stacked barplots of biovolume by station and date-------------

(plot_alg_grp_rm <- ggplot(alg_grp_biov, aes(x = month, y = total_biovolume, fill = algal_group))+
   geom_bar(position = "stack", stat = "identity") + 
   facet_wrap(year~region,ncol = 4)
   )
#needs work
#should use a survey number instead of date 
#add a year column
#should start by plotting just the stations in a given region
#then maybe group bars by year

#log transformed
(plot_alg_grp_rm <- ggplot(alg_grp_biov, aes(x = month, y = log(total_biovolume), fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)
  
  
  
  
  
  
  
  
  
  




