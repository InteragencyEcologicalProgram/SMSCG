#Suisun Marsh Salinity Control Gate Action
#Comparison of phytoplankton biovolume, sonde fluorescence, and chlor-a

#Nick Rasmussen
#2023-10-18

#to do list-------
#maybe try looking at pheophytin too

#ask sarah perry for the fluorescence data

#look at relationships by region and maybe water year

#look at relationships at different parts of the turbidity range

#could focus on samples with little to no blue-green algae biovolume

#check assumptions for analyses more thoroughly

#Packages------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(ggpubr) #qqplots
library(DEGreport) #adds corr and p to plots


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
#NOTE: initially looked at relationships between pairs of raw data but log transformation of both is better
#NOTE: looked at total cells vs chlor-a and relationship was much worse, as expected

#check assumptions

#normal distribution of each of the two datasets
#in summary, neither are normal even after log transform

#chlor-a
hist(wp_all$chla) #strongly clustered at low end with a few high values 
hist(log(wp_all$chla)) #looks much closer to normal but still long right tail
shapiro.test(wp_all$chla) #W = 0.22154, p-value < 2.2e-16
shapiro.test(log(wp_all$chla)) #W = W = 0.9182, p-value < 2.2e-16
ggqqplot(log(wp_all$chla),ylab = "chlor-a") #not great even after log transformation

#biovolume
hist(wp_all$total_biovolume) #strongly clustered at low end with a few high values 
hist(log(wp_all$total_biovolume)) #looks much more normal 
shapiro.test(wp_all$total_biovolume) #W = 0.29101, p-value < 2.2e-16
shapiro.test(log(wp_all$total_biovolume)) #W = 0.99472, p-value = 1.839e-07
ggqqplot(log(wp_all$total_biovolume),ylab = "biovolume") #doesn't look too bad

#plot relationship between chlor-a and phyto biovolume with log transformations
(plot_cb_all_log <- ggplot(wp_all, aes(x = log(chla), y = log(total_biovolume)))+
    geom_point()+
    geom_smooth(method = "lm")  +
    geom_cor(method = "spearman") 
)

#look at pearson correlation after log transformation
#probably not appropriate because didn't meet normality assumption
#cor.test(log(wp_all$chla),log(wp_all$total_biovolume))
#cor = 0.42 which is so-so

#why is x axis so long (over 250)?
#I don't see values over 100
# chlora_high <- wp_all %>% 
#   filter(chla>100)
#there are 5 cases
#x axis can accommodate these high points but there's no biovolume to match them, which is why they aren't on plot

#look at kendall rank correlation after log transformation
cor.test(log(wp_all$chla),log(wp_all$total_biovolume),method="kendall")
#tau =0.2287946
#much worse than pearson

#look at spearman correlation after log transformation
cor.test(log(wp_all$chla),log(wp_all$total_biovolume),method="spearman")
#rho =0.3345833
#between pearson and kendall

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
  #filter to just the years and months and stations of interest
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
  #filter to just the years and months and stations of interest
  filter(date >= "2017-06-01" & month >5 & month < 11 & station %in% stn_emp_vec) %>% 
  glimpse()


#smscg: combine WQ and phyto--------------
#NOTE: tried looking at cell counts vs chlor-a and relationship was worse

#wq and phyto should just match by station and date
#then station metadata should match by station (adds region and distance from gates)

wp_recent <- full_join(wq_recent,phyto_recent)
#not a perfect match between wq and phyto because 314 rows instead of 298
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
    geom_point()+
   geom_smooth(method = "lm")  +
   geom_cor(method = "spearman") 
)

#pearson correlation
#probably not appropriate because probably doesn't meet normality assumption
#cor.test(log(wp_recent$chla),log(wp_recent$total_biovolume))
#cor = 0.25 which is pretty low

#spearman correlation
cor.test(log(wp_recent$chla),log(wp_recent$total_biovolume),method = "spearman")
#rho = 0.1305046 

#smscg: look at chlor-a vs phyto biovolume by region---------------

#first add region categories
wp_region <- left_join(wp_recent,stn_emp)

#plot data
(plot_cb_region <- ggplot(wp_region, aes(x = log(chla), y = log(total_biovolume)))+
    geom_point()+
    geom_smooth(method = "lm")  +
    geom_cor(method = "spearman") +
    facet_wrap(~region)
)
#only floating stations are significant and probably driven by just two points

  
  
  