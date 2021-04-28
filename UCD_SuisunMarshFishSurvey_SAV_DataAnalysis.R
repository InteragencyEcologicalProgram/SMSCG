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
  #remove unneeded columns and reorder remaining columns
  select(StationCode
         ,SampleDate
         ,Year
         ,Month
         ,SampleTime
         ,OrganismCode
         ,Volume) %>% 
  #format date
  mutate(date = dmy(SampleDate))

#examine SAV data columns-------------

#generate list of the column names
names(sav_cleaner)

#how many stations in data set?
unique(sav_cleaner$StationCode)
#n=16

#range of dates?
range(sav_cleaner$date)
#2014-04-17" "2020-12-23"

#which SAV species?
unique(sav_cleaner$OrganismCode)
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
range(sav_cleaner$Volume) #1 - 102206
hist(sav_cleaner$Volume)
#vast majority are small but at least one or two that are very large

#create data frame that categorizes station by region
#and identifies the closest water quality station
#E = eastern marsh, C = central marsh, W = western marsh
rgwq <- data.frame(
  StationCode = c("MZ1", "MZ2", "MZ6", "NS3", "NS2", "NS1", "DV2", "DV1","MZN3","CO1", "SU2", "SB1", "BY3","SD2", "SU4", "SU3"),
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
savr<- inner_join(sav_cleaner,rgwq) 

#plots-----------

#total SAV volume time series by station
#exclude algae
#need to generate new data frame with all sav categories with a date and station summed

#stacked bar plots showing composition by month across years within station


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













  
  
  
  
  
  
  
  



