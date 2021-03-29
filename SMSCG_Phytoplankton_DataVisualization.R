#SMSCG
#phytoplantkon
#data visualizations

#Note: refer to DSRS plotting code
#also compare DFW and EMP samples where possible

#total organisms per ml
#total biovolume per ml
#both responses by major taxonomic group (likely phylum)
#maybe nmds by region

#NOTE: don't forget to compare data with notes about some samples not looking not well preserved

#load packages
library(tidyverse)
library(ggplot2)

# 1. Read in the Data----------------------------------------------
# Dataset is on SharePoint site for the SMSCG action

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data/Phytoplankton"
  )
)  

phyto_vis<-read_csv("SMSCG_phytoplankton_formatted_2020.csv")
#looks like column types are all correct, even date and time

#look at station names
sort(unique(phyto_vis$station))
#names are mostly, but not completely, unique
#fix this one because it's needed: "STN 610" vs "STN610"
#the rest will be filtered out: 
#"C3A" "C3A-Hood" "C3A-HOOD"
#"D16"  "D16-Twitchell"
#"E26" vs "EZ6" 

#remove spaces from all station names, which will remove the redundant versions of 610
phyto_vis$station2<-str_replace_all(phyto_vis$station, " ", "")

#EMP stations: Subset to just the four relevant to SMSCG
#Two different naming systems for stations: EMP (DFW)
#D22 (NZ064), D4 (NZ060), NZ032 (NZ032), NZS42 (NZS42)

#create data frame that groups names of stations that represent the same general location and adds the three broad regions
#RV = Sacramento River, ME = East Suisun Marsh, MW = West Suisun Marsh
station_key <- data.frame(
  station2 = c("STN706", "FMWT706", "NZ064", "D22","STN704","FMWT704","STN801", "FMWT802", "NZ060", "D4","STN609","MONT","STN610","FMWT605", "STN606","FMWT606","NZ032","NZS42"),
  station_comb = c(rep("PHY706", 4), rep("PHY704",2), rep("PHY801", 4),"STN609","MONT","STN610", "FMWT605", rep("PHY606",3),"NZS42"),
  region = c(rep("RV",10), rep("ME",3), rep("MW",5))
)
glimpse(station_key)

#join the sample data and the new station/region data
#by using inner_join, only the SMSCG relevant stations are kept
phyto_gates<- inner_join(phyto_vis,station_key) 

#look at number of samples per station
s_samp_count<-phyto_gates %>% 
  distinct(station_comb, date) %>% 
  group_by(station_comb) %>% 
  summarize(count = n())  
#generally 7-8 as expected 

#look at number of samples per region
r_samp_count<-phyto_gates %>% 
  distinct(region, date) %>% 
  group_by(region) %>% 
  summarize(count = n())  
#twice as many samples in river as east marsh
#west marsh intermediate in sample number

#summarize density and biovolume data by sample (station x date combo)
#so sum all the taxon specific densities and biovolumes within a sample
phyto_sum<-phyto_gates %>% 
  group_by(region, station_comb, station2, date) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol = sum(biovolume_per_ml)
  )


#plot total phytoplankton density by station and region
(plot_st_tot_den <-ggplot(phyto_sum, aes(x=date, y=tot_den))+ #specified what to plot on the x and y axes
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
  )

#plot total phytoplankton biovolume by station and region
(plot_st_tot_bvol <-ggplot(phyto_sum, aes(x=date, y=tot_bvol))+ #specified what to plot on the x and y axes
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
)



