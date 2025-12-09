#Suisun Marsh Salinity Control Gate Action
#Comparison of phytoplankton biovolume, sonde fluorescence, and chlor-a

#Nick Rasmussen
#2023-10-18

#to do list-------

#plot D-EMP extracted chlor-a vs estimated carbon based on phyto biovolume
#maybe try looking at pheophytin too

#ask perry for the fluorescence data

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
  #estimate carbon from biovolume
  mutate(
    #add mean cell biovolume (um^3/cell) column (biovolume_per_ml/cells_per_ml)
    mean_cell_biovolume = average_biovolume_per_m_l/cells_per_m_l,.before = gald
    #Calculate Biomass (pg-C per cell) from Biovolume (um^3 per cell)
    #Use one equation for diatoms and another equation for everything else based on Menden-Deuer & Lessard 2000
    ,biomass_pg_c = case_when((algal_group == "Centric Diatoms" | algal_group=="Pennate Diatoms") ~ 0.288 * (mean_cell_biovolume^0.811)
                              ,!(algal_group == "Centric Diatoms" | algal_group=="Pennate Diatoms") ~ 0.216 * (mean_cell_biovolume^0.939))
    #now convert pg C per cell to ug C per cell
    ,biomass_ug_c = biomass_pg_c / 10^6
    #Calculate Biomass Density (ug-C per mL)
    ,biomass_ug_c_ml = biomass_ug_c * cells_per_m_l
    #convert from ug/ml to ug/l because final units for biomass and LCEFA will be per liter
    ,biomass_ug_c_l = biomass_ug_c_ml*1000
  ) %>% 
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
    ,biomass_ug_c_l
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

#calculate total cells, biovolume, and carbon by sample
#NOTE: biovolume data not available for many of the earlier years
phyto_sum <- phyto_format %>% 
  group_by(station,month,year) %>% 
  summarise(total_cells = sum(cells_per_m_l)
            ,total_biovolume = sum(average_biovolume_per_m_l)
            ,total_carbon = sum(biomass_ug_c_l)
  ) %>% 
  #drop samples without biovolume; can't use those anyway
  filter(!is.na(total_biovolume)) %>% 
  glimpse()
#biovolume is in cubic microns per mL


#all: combine WQ and phyto--------------
#should just match by station and date
#NOTE: phyto data set starts 2008 (and biovolume even later), much later than WQ data set which is 1975

wp_all <- left_join(phyto_sum, wq_format)

#look for mismatches
wp_mis <-anti_join(phyto_sum,wq_format)  
  
#185 phyto samples that didn't have a WQ match when trying to match by station and date
#only 41 mismatches when just using station, month, year (not date)

#next filter the phyto data set to these station, month, year combos
#create a date_phyto column
#then combine the wq and phyto info to see if there are date mismatches or missing samples

#all: look at relationship between phyto biovolume, cell count, and carbon vs chlor-a--------------------
#NOTE: initially looked at relationships between pairs of raw data but log transformation is better
#NOTE: biovolume and carbon have similar results; total cells was much worse than the others, as expected

#check assumptions

#normal distribution of each of the datasets
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

#carbon
hist(wp_all$total_carbon) #strongly clustered at low end with a few high values 
hist(log(wp_all$total_carbon)) #looks much more normal 
shapiro.test(wp_all$total_carbon) #W = 0.33735, p-value < 2.2e-16
shapiro.test(log(wp_all$total_carbon)) #W = 0.99387, p-value = 2.321e-08
ggqqplot(log(wp_all$total_carbon),ylab = "carbon") #doesn't look too bad


#plot relationship between chlor-a and phyto biovolume with log transformations
(plot_cb_all_log <- ggplot(wp_all, aes(x = log(chla), y = log(total_biovolume)))+
    geom_point()+
    geom_smooth(method = "lm")  +
    geom_cor(method = "spearman") 
)
#ggsave(plot=plot_cb_all_log,"Plots/Phytoplankton/smscg_phyto_biovol_chlora.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#plot relationship between chlor-a and phyto carbon with log transformations
(plot_cc_all_log <- ggplot(wp_all, aes(x = log(chla), y = log(total_carbon)))+
    geom_point()+
    geom_smooth(method = "lm")  +
    geom_cor(method = "spearman") 
)
#ggsave(plot=plot_cc_all_log,"Plots/Phytoplankton/smscg_phyto_carbon_chlora.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)


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

#biovolume: look at kendall rank correlation after log transformation
cor.test(log(wp_all$chla),log(wp_all$total_biovolume),method="kendall")
#tau =0.2287946
#much worse than pearson

#biovolume: look at spearman correlation after log transformation
cor.test(log(wp_all$chla),log(wp_all$total_biovolume),method="spearman")
#rho =0.3345833
#between pearson and kendall

#carbon: look at kendall rank correlation after log transformation
cor.test(log(wp_all$chla),log(wp_all$total_carbon),method="kendall")
#tau =0.2432245  

#carbon: look at spearman correlation after log transformation
cor.test(log(wp_all$chla),log(wp_all$total_carbon),method="spearman")
#rho =0.3542864 
#better than kendall

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
#ggsave(plot=plot_cb_region,"Plots/Phytoplankton/smscg_phyto_biovol_chlora_regions.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#only floating stations are significant and probably driven by just two points

  
  
  