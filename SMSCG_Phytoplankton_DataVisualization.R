#SMSCG
#phytoplantkon
#data visualizations

#Note: refer to DSRS plotting code

#total organisms per ml
#total biovolume per ml
#both responses by major taxonomic group (likely phylum)
#maybe nmds by region

#load packages
library(tidyverse)

#set working directory
setwd("C:/Users/nrasmuss/OneDrive - California Department of Water Resources/SMSCG/Phyto/Data_2020")

phyto_vis<-read_csv("SMSCG_phytoplankton_formatted_2020.csv")
#looks like column types are all correct, even date and time

#look at station names
unique(phyto_vis$station2)
#two versions of STN 610: STN 610, STN610

#remove spaces from all station names, which will remove the redundant versions of 610
phyto_vis$station2<-str_replace_all(phyto_vis$station, " ", "")

#EMP stations: Subset to just the four relevant to SMSCG
#Two different naming systems for stations: EMP = DFW
#D22 (NZ064), D4 (NZ060), NZ032 (NZ032), NZS42 (NZS42)

#create data frame that groups names of stations that represent the same general location and adds the three broad regions
station_key <- data.frame(
  station = c("STN706", "FMWT706", "NZ064", "D22","STN704","FMWT704","STN801", "FMWT802", "NZ060", "D4","STN609","MONT","STN610","FMWT605", "STN606","FMWT606","NZ032","NZS42"),
  station_comb = c(rep("PHY706", 4), rep("PHY704",2), rep("PHY801", 4),"STN609","MONT","STN610", "FMWT605", rep("PHY606",3),"NZS42"),
  region = c(rep("river",10), rep("east_marsh",3), rep("west_marsh",5))
)

#







