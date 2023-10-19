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

#read in EMP phyto data from EDI (2008-2022)
phyto <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1320.6&entityid=67b9d4ee30d5eee6e74b2300426471f9") %>% 
  clean_names() %>% 
  glimpse()

#read in SMSCG phyto station metadata from EDI
stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=08de2a97cf2a3743af06e3ff6e0e9b39") %>% 
  glimpse()

#read in file that summarizes gate operations
#includes info on when gates were operating or closed (otherwise open)
gates <- read_csv("./Data/gate_operations_2017-2022.csv")

#all: format wq data-----------
#for now retain all stations and dates

wq_format <- wq %>% 
  #add a month and year column
  mutate(
    month = month(date)
    ,year = as.factor(year(date))
  ) %>% 
  #drop chlor-a samples that are below the detection limit
  filter(chla_sign!="<") %>% 
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

#all: format phyto data----------------------
#for now retain all stations and dates

phyto_format <- phyto %>% 
  #add a month and year column
  mutate(
    month = month(date)
    ,year = as.factor(year(date))
  ) %>% 
  #filter out bad data
  filter(!grepl("Degraded|BadData|PoorlyPreserved",quality_check)) %>% 
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
#table(phyto_format$quality_check)
#virtually all good; leave the fragmented ones; went back up and dropped the degraded or poorly preserved ones

#calculate total cells and biovolume by sample
#NOTE: biovolume data not available for many of the earlier years
phyto_sum <- phyto_format %>% 
  group_by(station,date,month,year) %>% 
  summarise(total_cells = sum(cells_per_m_l)
            ,total_biovolume = sum(average_biovolume_per_m_l)
  ) %>% 
  glimpse()
#biovolume is in cubic microns per mL

#all: combine WQ and phyto--------------
#should just match by station and date
#NOTE: phyto data set starts 2008, much later than WQ data set which is 1975

wp_all <- left_join(phyto_sum, wq_format)

#look for mismatches
wp_mis <-anti_join(phyto_sum,wq_format)  
  # select(station
  #        ,date_wq = date
  #        ,month
  #        ,year
  # )
#215 phyto samples that didn't have a WQ match
#after dropping chlor-a samples below detection limit there are now 252 missing matches
#could try matching by month and year instead of date

#next filter the phyto data set to these station, month, year combos
#create a date_phyto column
#then combine the wq and phyto info to see if there are date mismatches or missing samples

#all: look at relationship between phyto biovolume and chlor-a--------------------

#plot relationship between chlor-a and phyto biovolume
# (plot_cb_all <- ggplot(wp_all, aes(x = chla, y = total_biovolume))+
#     geom_point()
# )

#plot relationship between chlor-a and phyto biovolume with log transformations
(plot_cb_all_log <- ggplot(wp_all, aes(x = log(chla), y = log(total_biovolume)))+
    geom_point()
)
#probably the better way to look at data; can maybe make out a relationship

#look at correlation 
#cor.test(wp_all$chla,wp_all$total_biovolume)
#cor = 0.59 which isn't bad but has to be driven by a few high points

#look at correlation after log transformation
cor.test(log(wp_all$chla),log(wp_all$total_biovolume))
#cor = 0.42 which is so-so

#why is x axis so long (over 250)?
#I don't see values over 100
# chlora_high <- wp_all %>% 
#   filter(chla>100)
#there are 5 cases
#x axis can accommodate these high points but there's no biovolume to match them, which is why they aren't on plot


#all: look at relationship between phyto cells and chlor-a--------------------


#plot relationship between chlor-a and phyto cell counts
# (plot_cc_all <- ggplot(wp_all, aes(x = chla, y = total_cells))+
#     geom_point()
# )

#plot relationship between chlor-a and phyto cell counts using log transformed data
(plot_cc_all_log <- ggplot(wp_all, aes(x = log(chla), y = log(total_cells)))+
    geom_point()
)
#probably the better way to look at data; probably not a significant relationship

#look at correlation
cor.test(log(wp_all$chla),log(wp_all$total_cells))
#cor = 0.16 which is pretty low

#make vector of target EMP stations------------
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

#smscg: format wq data------------

wq_recent <- wq_format %>% 
  #filter to just the years and months of interest
  filter(date >= "2017-06-01" & month >5 & month < 11 & station %in% stn_emp_vec) %>% 
  glimpse()

#check date range
#range(wq_recent$date) #"2017-06-14" "2022-10-19"; looks good

#check stations
#unique(wq_recent$station)
#"D22"   "D4"    "NZ068" "D10"   "D8"    "EZ2"   "EZ6"   "D7"    "NZ032" "NZS42"
#looks good

#plot chlor-a data by station
(plot_chlora <- ggplot(wq_recent, aes(x = date, y = chla, color = year))+
    geom_line()+
    facet_wrap(.~station)
  )
#hig values in July 2017

#smscg: format phyto data----------------------

#filter data set to just the target stations and dates
phyto_recent <- phyto_sum %>% 
  #filter to just the years and months of interest
  filter(date >= "2017-06-01" & month >5 & month < 11 & station %in% stn_emp_vec) %>% 
  glimpse()


#smscg: combine WQ and phyto--------------
#should just match by station and date
#both df have 298 rows which is good sign

wp_recent <- full_join(wq_recent,phyto_recent)
#not a perfect match because 314 rows instead of 298
#could try matching by month and year instead if it is just a mismatch of dates due to typos
# a few of these are because of bad chlor-a or phyto data I dropped

#look for mismatches
wp_mis_recent <-anti_join(wq_recent,phyto_sum) %>% 
  select(station
         ,date_wq = date
         ,month
         ,year
         )


#next filter the phyto data set to these station, month, year combos
#create a date_phyto column
#then combine the wq and phyto info to see if there are date mismatches or missing samples

#smscg: look at relationship between chlor-a and phyto biovolume-------------

(plot_cb_recent <- ggplot(wp_recent, aes(x = log(chla), y = log(total_biovolume)))+
    geom_point()
)

cor.test(log(wp_recent$chla),log(wp_recent$total_biovolume))
#cor = 0.25 which is pretty low

#smscg: look at relationship between chlor-a and phyto cells----------
(plot_cc_recent <- ggplot(wp_recent, aes(x = log(chla), y = log(total_cells)))+
    geom_point()
)
#one really low value for cells

cor.test(log(wp_recent$chla),log(wp_recent$total_cells))
#cor = 0.23 which is pretty low

#look at outlier
wp_recent_drop <- wp_recent %>% 
  mutate(total_cells_log = log(total_cells)) %>% 
  filter(total_cells_log > 5)
#there is a sample where total cells = 1 which must be wrong
#lets remove it and redo analysis

(plot_cc_recent_drop <- ggplot(wp_recent_drop, aes(x = log(chla), y = log(total_cells)))+
    geom_point()
)
#one really low value for cells

cor.test(log(wp_recent_drop$chla),log(wp_recent_drop$total_cells))
#cor = 0.22 which is pretty low

#maybe try looking at pheophytin too

#ask sarah perry for the fluorescence data

#look at relationships by region and maybe water year


  
  
  