#Suisun Marsh Salinity Control Gate (SMSCG) Action

#Prepare data associated with project for publishing 
#in repository on Environmental Data Initiative

#This script prepares data from the United States Fish and Wildlife Service
#Enhanced Delta Smelt Monitoring (EDSM) surveys
#https://www.fws.gov/lodi/juvenile_fish_monitoring_program/jfmp_index.htm

#EDSM already has a repository on EDI with all their data
#https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=415&revision=12

#This code creates the subset relevant to the SMSCG action
#Years: 2017 and later
#Months: July - October
#Survey regions: Suisun Marsh, Suisun Bay, Lower Sacramento

#required packages
library(tidyverse)
library(EDIutils) #download EDI data

#use EDIutils package to read in all file names and download the ones you want to use
#https://docs.ropensci.org/EDIutils/index.html

#list all data files from EMP benthic inverts EDI package
edsm_pkg <- read_data_entity_names(packageId = "edi.415.12")
edsm_main <- read_csv(read_data_entity(packageId = "edi.415.12", entityId= edsm_pkg$entityId[2])) %>% 
  glimpse()

#filter to just the needed years, months, regions, and columns
edsm_sub <- edsm_main %>% 
  #create week, month, and year columns from date column
  mutate(Date = as_date(SampleDate)
          ,Year = year(Date)
         ,Month = month(Date)
         ,Week = week(Date)
         ) %>% 
  #filter to only keep desired years, months, and strata
  filter(Year >= "2017" 
         & Month %in% c(7,8,9,10)         
         & !is.na(TowNumber)
         & Stratum %in% 
           c("Cache Slough LI","Lower Sacramento","Sac DW Ship Channel","Suisun Bay/Marsh","Lower San Joaquin","Suisun Bay","Suisun Marsh")
         ) %>% 
  #only keep necessary columns
  select(Stratum,StationCode,Date,Week,Month,Year,TowNumber,OrganismCode,Count) %>% 
  arrange(Year,Week,Stratum) %>% 
  glimpse()

#summarize number of tows per stratum and week
edsm_sum_tow <- edsm_sub %>% 
  distinct(Stratum,StationCode,Date,Week,Month,Year,TowNumber) %>% 
  group_by(Stratum, Week, Month, Year) %>%
  summarize(Tows = length(TowNumber),.groups = "drop"
            ) %>% 
  arrange(Year,Week,Stratum)

#summarize number of stations per stratum and week
edsm_sum_station <- edsm_sub %>% 
  distinct(Stratum,StationCode,Week,Month,Year) %>% 
  group_by(Stratum, Week, Month, Year) %>%
  summarize(Stations = length(StationCode),.groups = "drop") %>% 
  arrange(Year,Week,Stratum)

#summarize number of delta smelt per stratum and week
edsm_sum_dsm <- edsm_sub %>% 
  group_by(Stratum, Week, Month, Year) %>%
  summarize(DSM = sum(Count[which(OrganismCode == "DSM")]),.groups = "drop") %>% 
  arrange(Year,Week,Stratum)

#combine the summarized data for tows, station, and delta smelt counts
#then calculate number of delta smelt per tow
list_df = list(edsm_sum_tow,edsm_sum_station,edsm_sum_dsm)
edsm_comb <- list_df %>% 
  reduce(left_join, by=c("Stratum","Year","Week","Month")) %>% 
  mutate(DSMcpue = DSM/Tows) %>% 
  glimpse()

#export data for publishing on EDI
#write_csv(edsm_comb,"./EDI/data_output/EDSM_2017-2024_SummerFall_DSM.csv")
