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
#also note that EMP data starts in June but DFW data starts in July

#load packages
library(tidyverse)
library(ggplot2)
library(lubridate)

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

#add a column for month
phyto_gates$month<-month(phyto_gates$date)
#just extracts the month from the date
#could consider rounding date to nearest month too

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


#plot time series of density and biovolume by station-----------

#summarize density and biovolume data by sample (station x date combo)
#so sum all the taxon specific densities and biovolumes within a sample
s_phyto_sum<-phyto_gates %>% 
  group_by(region, station_comb, station2, date, month) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol = sum(biovolume_per_ml)
  )


#plot total phytoplankton density by station 
(plot_st_tot_den <-ggplot(s_phyto_sum, aes(x=date, y=tot_den))+ #specified what to plot on the x and y axes
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
  )

#plot total phytoplankton biovolume by station
(plot_st_tot_bvol <-ggplot(s_phyto_sum, aes(x=date, y=tot_bvol))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
)

#plot time series of density and biovolume by region-----------

#so sum all the taxon specific densities and biovolumes within a sample
r_phyto_sum<-s_phyto_sum %>% 
  group_by(region, month) %>% 
  summarize(
    tot_den_avg = mean(tot_den)
    ,tot_bvol_avg = mean(tot_bvol)
  )

#plot mean total phytoplankton density by region
(plot_rg_tot_den <-ggplot(r_phyto_sum, aes(x=month, y=tot_den_avg))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~region)
)

#plot mean total phytoplankton biovolume by region
(plot_rg_tot_bvol <-ggplot(r_phyto_sum, aes(x=month, y=tot_bvol_avg))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~region)
)

#boxplots of total density and biovolume by region-------

(plot_rg_tot_den_bx<-ggplot(data=s_phyto_sum, aes(x = region, y = tot_den)) + 
   geom_boxplot()+
   geom_jitter() #adds all points to plot, not just outliers
)

(plot_rg_tot_bvol_bx<-ggplot(data=s_phyto_sum, aes(x = region, y = tot_bvol)) + 
    geom_boxplot()+
    geom_jitter() #adds all points to plot, not just outliers
)


#plot number of genera by station and region-------

#determine how many species typically within each genera
#if generally one, then easier to just use genus for plots/analysis
ssp_count<-phyto_gates %>% 
  distinct(genus, taxon) %>% 
  group_by(genus) %>% 
  summarize(count = n())  

#use histogram to visualize distribution of species per genera
(plot_spp_per_genus_hist<-ggplot(data=ssp_count, aes(x = count)) + 
    geom_histogram()
)
#most genera contain 1-2 species but some contain up to 15

#look closer at genera with many species
genera_rich<-phyto_gates %>% 
  distinct(genus, taxon) %>% 
  filter(genus=="Nitzschia" | genus =="Navicula") %>% 
  arrange(genus)
#not just unique because of variants of same name
#actually quite a few different species within these genera present

#create df that counts genera by sample
phyto_genera_sum<-phyto_gates %>% 
  distinct(region, station_comb,date, month,genus) %>% 
  group_by(region, station_comb,date, month) %>% 
  summarize(genera = n())

#use histogram to visualize distribution of genera per sample
(plot_genera_per_sample_hist<-ggplot(data=phyto_genera_sum, aes(x = genera)) + 
    geom_histogram()
)
#generally 1-7 but some up to 15 genera per sample

#plot time series of number of genera by station
(plot_st_genera <-ggplot(phyto_genera_sum, aes(x=date, y=genera))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb)
)

#number of genera per region
phyto_genera_sum_rg<-phyto_gates %>% 
  distinct(region,date, month,genus) %>% 
  group_by(region,date, month) %>% 
  summarize(genera = n())

#plot time series of number of genera by region
(plot_st_genera <-ggplot(phyto_genera_sum_rg, aes(x=date, y=genera))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~region)
)

#stacked bar plot showing phyla by station-------


#stacked bar plot showing phyla by region-------




