#Suisun Marsh Salinity Control Gate Action
#UC-Davis Suisun Marsh Fish Survey
#Otter trawl
#Submerged aquatic vegetation data

#To do list
#what are the volume units for SAV?

#required packages
library(tidyverse)
library(janitor)
library(lubridate)
#library(readxl) #importing data from excel files



# Read in the Data----------------------------------------------
# Datasets are on SharePoint site for the SMSCG action

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data"
  )
)  

#read in submerged aquatic vegetation data
sav_data<-read_csv(file = paste0(sharepoint_path,"./UC-Davis Suisun Marsh Fish Survey/SAV_Data_2014-2020.csv"))

#read in water quality data from the National Estuarine Research Reserve System (NERRS) stations
#for this analysis, just need SFBFMWQ 
nerr_data<-read_csv(file = paste0(sharepoint_path,"./2020 data for USBR/NERRS_data/SFBSMWQ_SFBFMWQ_2014-01-01_2021-04-01.csv"))


#format SAV data-----------

#remove unneeded columns
#MethodCode = OTR for all samples because all from same type of gear
#StandardLength and Count are all zeros because Volume is unit of measurement for SAV

sav_cleaner <- sav_data %>% 
  #use janitor package to simplify column names
  clean_names() %>% 
  #format date
  mutate(date = dmy(sample_date)) %>% 
  #remove unneeded columns and reorder remaining columns
  select(station_code
         ,date
         ,year
         ,month
         ,sample_time
         ,organism_code
         ,volume) 
  

#examine SAV data columns-------------

#generate list of the column names
names(sav_cleaner)

#how many stations in data set?
unique(sav_cleaner$station_code)
#n=16

#range of dates?
range(sav_cleaner$date)
#2014-04-17" "2020-12-23"

#which SAV species?
unique(sav_cleaner$organism_code)
#Nine categories
#"ELCA" = Elodea canadensis = American waterweed   
#"EGDE" = Egeria densa = Brazilian waterweed   
#"CEDE" = Ceratophyllum demersum = coontail   
#"MYSP" = Myriophyllum spicatum = Eurasian watermilfoil   
#"ALG" = filamentous green algae    
#"POCR" = Potamogeton crispa = curlyleaf pondweed   
#"STSPP" = Stuckenia spp = Sago pondweed  
#"SAVSPP" = unidentified submerged aquatic plant species
#"CACA" = Cabomba caroliniana = Carolina Fanwort    

#distribution of SAV volume
range(sav_cleaner$volume) #1 - 102206
hist(sav_cleaner$volume)
#vast majority are small but at least one or two that are very large

#create data frame that categorizes station by region
#and identifies the closest water quality station
#E = eastern marsh, C = central marsh, W = western marsh
rgwq <- data.frame(
  station_code = c("MZ1", "MZ2", "MZ6", "NS3", "NS2", "NS1", "DV2", "DV1","MZN3","CO1", "SU2", "SB1", "BY3","SD2", "SU4", "SU3"),
  region = c(rep("E",9), rep("C",5), rep("W",2)),
  wq = c("MSL", "NSL", rep("BLL",7),rep("SFBFMWQ",5),rep("GOD",2))
)

#fish stations that seem to be missing
#East marsh: DV3
#Central marsh: CO2, SB2, SU1, BY1, PT1, PT2
#West marsh: GY2, GY1, GY3
#all of these are active stations according to data base
#two stations that aren't on my map of the study: 
#"MZN3": Montezuma Slough - at side channel, 38.1503740699, -121.916719863 (East marsh)
#"SD2": Sheldrake Slough - at Suisun Slough, no coordinates in database (West Marsh)

#add region categories to main data set
savr<- inner_join(sav_cleaner,rgwq) %>% 
  mutate_at(vars(station_code, organism_code,region,wq),factor)
#glimpse(savr)

#look at samples by station and year
#vast majority of samples are in eastern Montezuma Slough (MZ1, MZ2, MZ6)
#note there are multiple entries per month in some cases because of multiple species
savt1<-savr %>% 
  tabyl(station_code)

savt2<-savr %>% 
  tabyl(station_code, year)

#create new data frame that sums all SAV by station 
sav_sum_tot <- savr %>% 
  group_by(station_code,region, wq) %>% 
  summarize(
    sav_tot = sum(volume))
glimpse(sav_sum_tot)

#create new data frame that sums all SAV by station and date
sav_sum_dt <- savr %>% 
  group_by(station_code,region, wq, date) %>% 
  summarize(
    sav_tot = sum(volume))

#create new data frame that sums volume by species for each station
sav_sum_stn_spp <- savr %>% 
  group_by(station_code,region, wq, organism_code) %>% 
  summarize(
    sav_spp_tot = sum(volume))

#create subset with just MZ1 which has by far the veg
mz1 <- savr %>% 
  filter(station_code == "MZ1")

#plots-----------

#total SAV volume by station
(plot_tot_vol_stn <-ggplot(sav_sum_tot, aes(x=station_code, y=sav_tot))+ 
   geom_bar(stat="identity")
 )

#total SAV volume time series by station
(plot_tot_vol_stn_dt <-ggplot(sav_sum_dt, aes(x=date, y=sav_tot))+ 
   geom_line() + 
   geom_point() + 
   #ylim(0,13000)+ #exclude a high end outlier in MZ1
   facet_wrap(~station_code)
)

#stacked bar plots showing composition by station
(plot_tot_vol_stn_spp <-ggplot(sav_sum_stn_spp
      , aes(x=station_code, y= sav_spp_tot,  fill = organism_code))+
    geom_bar(position = "stack", stat = "identity") + 
    #ylim(0,25000)+ #zooms in on lower volume stations (MZ1 bar not accurate this way)
    ylab("Volume") + xlab("Station")
)

#stacked bar plots showing composition by month for MZ1
(plot_tot_vol_mz1_spp <-ggplot(mz1
            , aes(x=date, y= volume,  fill = organism_code))+
    geom_bar(position = "stack", stat = "identity") + 
    #ylim(0,25000)+ #zooms in on lower volume stations (MZ1 bar not accurate this way)
    ylab("Volume") + xlab("Date")
)


#format NERR water quality data--------------

nerr_cleaner <- nerr_data %>% 
  #just want the date-time column and the SFBFMWQ station columns
  select(c( DateTimeStamp | contains("SFBFMWQ"))) %>%
  #simplify column names for remaining station
  rename_at(.vars = vars(starts_with("SFBFMWQ_")),
            .funs = funs(sub("SFBFMWQ_", "", .))) %>% 
  #format date-time column
  mutate(date_time = mdy_hm(DateTimeStamp))
  #rename columns

#glimpse(nerr_cleaner)
#temperature (C)
#Specific conductivity (mS/cm)
#salinity (ppt)
#DO (%)
#DO (mg/L)
#sonde depth (m) 
#pH
#turbidity (NTU)
#fluorescence (ug/L)

#explore NERR data--------------

#look at QC flag codes for specific conductance
unique(nerr_cleaner$F_SpCond)
#lots of categories
#keep the ones that include <0> which indicates pass of initial QAQC checks

#filter to only keep the rows that include specific conductance values 
#that passed initial QAQC checks
nerr_filter<-nerr_cleaner %>% 
  filter(grepl("<0>",F_SpCond))

#unique(nerr_filter$F_SpCond)
#looks like the filter worked correctly

#histogram of remaining salinity values
hist(nerr_filter$SpCond)

#histogram of remaining temperature values
hist(nerr_filter$Temp)
#looks OK

#how do flags for temperature look in filtered data set
unique(nerr_filter$F_Temp)
#all remaining temp data passed initial QAQC

#histogram of remaining turbidity values
hist(nerr_filter$Turb)
#turbidity still has problems

#for now, just focus on specific conductance and temperature
#turbidity probably matters too but isn't available across all WQ stations
nerr_sub<-nerr_filter %>% 
  #use janitor package to get rid of capital letters in column names
  clean_names() %>% 
  #create a month column which will be used to calculate monthly means
  mutate(month = month(date_time)) %>% 
  #subset columns and reorder them
  select(station_code, date_time, month, temp, sp_cond) %>% 
  #rename station_code to wq to then join with sav data set
  rename(wq = station_code)

#calculate monthly means for specific conductance and temperature
  
#make boxplots by month
(plot_nerr_bx<-ggplot(data=nerr_sub, aes(x = month, y = sp_cond)) + 
    geom_boxplot()+
    geom_jitter() #adds all points to plot, not just outliers
)
  


#additional data sets needed for analysis----------
#survey effort: need to know SAV volume per unit survey effort (though maybe this is standardized)
#water quality measurements: salinity is key but turbidity would be nice too
#need to get WQ data from nearby sondes: NSL, BDL
#some of this WQ data is on the SharePoint site
#fish survey does collect some kind of depth data













  
  
  
  
  
  
  
  



