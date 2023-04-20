#Suisun Marsh Salinity Control Gate Action
#creating metadata file for clam stations

#Nick Rasmussen 4/20/2023

#To do list-----------
#for Betsy's habitat column the two stations in Grizzly Bay are labeled "river" which seems a little odd

#required packages
library(sf)
library(tidyverse)
library(deltamapr)
library(janitor)
library(lubridate)

#read in the data---------

#read in data from EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.876.5
stn_clam <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.5&entityid=c169596f76bde5357fb3ad94734383b0") %>% 
  clean_names() %>% 
  glimpse()

#read in matrix of distances among stations 
#SM14 is at the SMSCG
clam_dist <- read_csv("./StationDistances/station_distances_m_clam.csv")

#read in clam station crosswalk file
clam_cross <- read_csv("./EDI/data_input/clams/SMSCG_clam_station_crosswalk.csv")

#format the distance matrix--------

#just keep the needed columns
clam_dist_trunc <- clam_dist %>% 
  select(station
         ,smscg_distance = SM14
         ) %>% 
  #round distances to nearst m
  mutate(smscg_distance_m = round(smscg_distance,0)) %>% 
  select(station
         ,smscg_distance_m
  ) %>% 
  glimpse()

#format the station data from the EDI sample data file----------

#reduce df to just the needed columns
clam_trunc <- stn_clam %>% 
  select(year:west_decimal_degrees,habitat:done_all_years) %>% 
  glimpse()

#look at summary of samples by station and year
clam_samp_sum <- clam_trunc %>% 
  group_by(station,year,done_all_years) %>% 
  count()

#any stations not sampled twice in a year?
clam_incomp <- clam_samp_sum %>% 
  filter(n!=2)
#all stations were sampled both times in the years it was sampled

#look at summary of samples by station
clam_samp_sum_tot <- clam_trunc %>% 
  group_by(station,done_all_years) %>% 
  count() %>% 
  arrange(done_all_years,station)
#as indicated in our study plan, 48 stations were sampled in 2018
#in 2019 and beyond, only 28 stations were sampled

#format data set for publication

clam_cleaner <- clam_trunc %>% 
  #add distance between SMSCG and station
  left_join(clam_dist_trunc) %>% 
  #create columns with start and end dates for station sampling
  group_by(station,station_alias,north_decimal_degrees,west_decimal_degrees,smscg_distance_m, habitat,habitat_type,done_all_years) %>% 
  summarize(date_start = min(date)
            ,date_end = max(date),.groups="drop") %>% 
  #add or modify some columns
  mutate(
    #indicates who collects the data
    program = "DWR"
    #indicate that no stations were active in 2022
    ,active = 0
    #convert date time to date
    ,date_start2 = ymd(date_start)
    ,date_end2 = ymd(date_end)
    #column that indicates how many missing years of data
    ,missing_years = 2022-year(date_end)
    #create region column from habitat_type
    ,region = case_when(station =="SM01" | station == "SM16" ~ "Bay"
                        #SM52 is other river station but is correctly labeled already
                        ,station=="SM15"~"River"
                        ,TRUE~habitat_type) 
  ) %>% 
  #order and rename columns
  select(
    program
    ,station_clam = station
    ,longitude = west_decimal_degrees
    ,latitude = north_decimal_degrees
    ,location = station_alias
    ,smscg_distance_m
    ,region
    ,habitat
    ,active
    ,date_start = date_start2
    ,date_end = date_end2
    ,missing_years
  )
  
#now add the station crosswalk info 
clam_format <- clam_cleaner %>% 
  left_join(clam_cross) %>% 
  glimpse()

#write
#write_csv(clam_format,"./EDI/data_output/smscg_stations_clams.csv")

