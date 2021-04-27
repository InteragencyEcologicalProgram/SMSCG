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


# Read in the Data----------------------------------------------
# Dataset is on SharePoint site for the SMSCG action

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data/UC-Davis Suisun Marsh Fish Survey"
  )
)  

#read in data
sav_data<-read_csv(file = paste0(sharepoint_path,"/SAV_Data_2014-2020.csv"))

#format data-----------

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

#examine columns-------------

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
#E = eastern marsh, C = central marsh, W = western marsh
#O = other: a temporary category for two stations that I can't place on map; look up coordinates in database
region <- data.frame(
  StationCode = c("MZ1", "MZ2", "MZ6", "NS3", "NS2", "NS1", "DV2", "DV1","CO1", "SU2", "SB1", "BY3","SU4", "SU3", "MZN3", "SD2"),
  region = c(rep("E",8), rep("C",4), rep("W",2), rep("O",2))
)
#stations that seem to be missing
#East marsh: DV3
#Central marsh: CO2, SB2, SU1, BY1, PT1, PT2
#West marsh: GY2, GY1, GY3
#two stations that aren't on my map of the study: "MZN3", "SD2" 

#add region categories to main data set
savr<- inner_join(sav_cleaner,region) 

#plots-----------

#total SAV volume time series by station
#exclude algae
#need to generate new data frame with all sav categories with a date and station summed

#stacked bar plots showing composition by month across years within station

#additional data sets needed for analysis----------
#survey effort: need to know SAV volume per unit survey effort (though maybe this is standardized)
#water quality measurements: salinity is key but turbidity would be nice too













  
  
  
  
  
  
  
  



