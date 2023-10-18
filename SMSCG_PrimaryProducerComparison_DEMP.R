#Suisun Marsh Salinity Control Gate Action
#Comparison of phytoplankton biovolume, sonde fluorescence, and chlor-a

#Nick Rasmussen
#2023-10-18

#Packages------------------
library(tidyverse)
library(lubridate)
library(janitor)

#read in data------------

#read in EMP WQ data from EDI (1975 to 2022)
wq <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.9&entityid=cf231071093ac2861893793517db26f3") %>% 
  clean_names() %>% 
  glimpse()

#read in EMP phyto data from EDI
phyto <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1320.6&entityid=67b9d4ee30d5eee6e74b2300426471f9") %>% 
  clean_names() %>% 
  glimpse()

#read in SMSCG phyto station metadata from EDI
stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=08de2a97cf2a3743af06e3ff6e0e9b39") %>% 
  glimpse()

#read in file that summarizes gate operations
#includes info on when gates were operating or closed (otherwise open)
gates <- read_csv("./Data/gate_operations_2017-2022.csv")

#format wq data------------

#make vector of target stations
stn_emp <- stn %>% 
  #just keep the EMP stations
  filter(program == "EMP") %>% 
  #drop the program prefix from the station names
  mutate(station2 = str_remove(station,"EMP_")) %>% 
  select(
    station = station2
    ,distance_smscg_m
    ,region
    ) %>% 
  glimpse()
stn_emp_vec <-stn_emp$station

wq_recent <- wq %>% 
  #add a month and year column
  mutate(
    month = month(date)
    ,year = as.factor(year(date))
         ) %>% 
  #filter to just the years and months of interest
  filter(date >= "2017-06-01" & month >5 & month < 11 & station %in% stn_emp_vec) %>% 
  #keep just the needed columns
  select(
    station
    ,date
    ,month
    ,year
    ,time
    ,flag
    ,flag_description
    ,field_notes
    ,north_lat
    ,west_long
    ,chla_sign
    ,chla
    ,pheophytin
    ,pheophytin_sign
  ) %>% 
  glimpse()

#check date range
range(wq_recent$date) #"2017-06-14" "2022-10-19"; looks good

#check stations
unique(wq_recent$station)
#"D22"   "D4"    "NZ068" "D10"   "D8"    "EZ2"   "EZ6"   "D7"    "NZ032" "NZS42"
#looks good

#plot chlor-a data by station
(plot_chlora <- ggplot(wq_recent, aes(x = date, y = chla, color = year))+
    geom_line()+
    facet_wrap(.~station)
  )
#hig values in July 2017

#format phyto data----------------------

phyto_recent <- phyto %>% 
  #add a month and year column
  mutate(
    month = month(date)
    ,year = as.factor(year(date))
  ) %>% 
  #filter to just the years and months of interest
  filter(date >= "2017-06-01" & month >5 & month < 11 & station %in% stn_emp_vec & quality_check!="Degraded") %>% 
  #keep just the needed columns
  select(
    station
    ,date
    ,month
    ,year
    ,time
    ,taxon
    ,algal_group
    ,units_per_m_l
    ,cells_per_m_l
    ,average_biovolume_per_m_l
    ,gald
    ,phyto_form
    ,quality_check
    ,latitude
    ,longitude
  ) %>% 
  glimpse()

#look at quality_check codes
table(phyto_recent$quality_check)
#virtually all good; leave the fragmented ones; went back up and dropped the degraded one

#calculate total cells and biovolume by sample
phyto_sum <- phyto_recent %>% 
  group_by(station,date) %>% 
  summarise(total_cells = sum(cells_per_m_l)
            ,total_biovolume = sum(average_biovolume_per_m_l)
            ) %>% 
  glimpse()
#biovolume is in cubic microns per mL

#combine WQ and phyto--------------
#should just match by station and date
#both df have 298 rows which is good sign

wp <- full_join(wq_recent,phyto_sum)
#not a perfect match because 314 rows instead of 298

#look for mismatches
wp_mis <-anti_join(wq_recent,phyto_sum) %>% 
  select(station
         ,date_wq = date
         ,month
         ,year
         )
#16 rows as expected

#next filter the phyto data set to these station, month, year combos
#create a date_phyto column
#then combine the wq and phyto info to see if there are date mismatches or missing samples

#plot relationship between chlor-a and phyto biovolume
(plot_cb <- ggplot(wp, aes(x = chla, y = total_biovolume))+
    geom_point()
)
#no relationship

#maybe try looking at pheophytin too

#ask sarah perry for the fluorescence data

#


  
  
  