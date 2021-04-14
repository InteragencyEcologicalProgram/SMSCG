#SMSCG
#phytoplantkon

#comparisons among data types:
#Organisms/mL
#Biovolume (microns^3)/mL
#Carbon/mL
#Extracted Chlor-a
#Sonde Fluorescence

#perhaps calculate and compare means by region and week of the year 
#this would be a rough first step
#sample sizes underlying the means will likely be quite different
#and phytoplankton can probably vary a lot among days within a week

#load packages
library(tidyverse) #suite of packages for data science
library(readxl) #reading in data from excel spreadsheets
library(ggplot2) #making plots
library(lubridate) #formatting dates

# 1. Read in the Data----------------------------------------------
# Dataset is on SharePoint site for the SMSCG action

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data"
  )
)  

#read in phytoplankton community composition summary data
#includes organisms/mL and biovolume/mL
phyto_com<-read_csv(file = paste0(sharepoint_path,"./Phytoplankton/SMSCG_phytoplankton_sample_summary_2020.csv"))
#looks like columns are formatted correctly, even the date and time

#read in the sonde data, including fluorescence used to estimate primary productivity
sonde<-read_csv(file = paste0(sharepoint_path,"./2020 data for USBR/des_data.csv"))
#columns mostly formatted correctly but need to format the date-time column

#read in extracted chlorophyll-a concentrations
chlora<-read_excel(path = paste0(sharepoint_path,"./2020 data for USBR/DWR SuisunMarsh Chl All Stations WY2020.xlsx"))
#glimpse(chlora)
#looks like columns are formatted correctly even the date column; oddly there's no time data
#lots of columns I won't need, so should pare down the data set

#format the extracted chlorophyll-a data set-----------------

#look at list of station names 
#unique(chlora$QST_NAME)
#unique(chlora$QST_FULL_STATION_NAME)
#looks like they are probably all unique names
#but many are long, so maybe keep the station number too

#look at station number
#unique(chlora$QST_NUMBER)
#same number of stations as with station name
#so confirms that the station names are all truly unique

#look at reporting limit
#unique(chlora$RES_REPORTING_LIMIT)
#0.5 1.0 2.5
#there are three values so keep this column

#look at sample depth
#unique(chlora$SAM_DEPTH)
#all collected 1 m from surface
#so don't need to keep this column

#look at sample type column
#unique(chlora$SPP_DESCRIPTION)
#"Normal Sample"    "Duplicate Sample"
#makes sense

#look at sample description
#unique(chlora$SAM_DESCRIPTION)
#"Chlorophyll A Grab Sample"
#all the same so don't need this column

#look at units column
#unique(chlora$UNS_NAME)
#"ug/L"
#all the same so can drop this column

#reduce the data set to just the needed columns
#QST_NAME
#QST_NUMBER
#SAM_COLLECTION_DATE
#ANL_ANALYTE_NAME
#RES_RESULT
#RES_REPORTING_LIMIT
#UNS_NAME
#SPP_DESCRIPTION
chlora_sub <- chlora %>% 
  #simplify column names
  rename(station_name = QST_NAME
    ,station_code = QST_NUMBER
    ,date = SAM_COLLECTION_DATE
    ,analyte = ANL_ANALYTE_NAME
    ,result = RES_RESULT
    ,report_limit = RES_REPORTING_LIMIT
    ,sample_type = SPP_DESCRIPTION) %>% 
  #subset and reorder columns to just those needed
  select(station_name 
         ,station_code 
         ,date
         ,analyte 
         ,result 
         ,report_limit 
         ,sample_type)
#glimpse(chlora_sub)

#are there values below the reporting limit in this data set?
#chlora_sub$diff<-chlora_sub$result-chlora_sub$report_limit
#replim<-chlora_sub %>% 
#  filter(diff<0)
#there is one case where result is zero 
#so it looks like values below the reporting limit are very rare
#and those values are included as zeros

#create data frame that pairs stations with regions
#RV = Sacramento River, ME = East Suisun Marsh, MW = West Suisun Marsh, GZ = Grizzly Bay
#not 100% sure about region assignments
#was pretty difficult to match names in this data set to those in the study plan (Table 3)
stations_rand<-unique(chlora_sub[,c("station_name","station_code")])
stations<-stations_rand[order(stations_rand$station_code),]
stations$region<-c(rep("GZ",2), "RV","MW",rep("ME",2), rep("MW",3),"ME" )
#could get GPS coordinates from CDEC

#use join to add region names to main extracted chlorophyll data set
chlora_reg<-left_join(chlora_sub,stations)
#glimpse(chlora_reg)

#add a week of the year column based on date
chlora_reg$week <- week(chlora_reg$date)

#summarize by region and week
chlora_samp_mean<-chlora_reg %>% 
  group_by(region,week,analyte) %>% 
  summarize(samp_mean = mean(result))
#note that both normal and duplicate samples are included in these averages 

#break chlorophyll and Pheophytin into separate columns
lab_pigment<-chlora_samp_mean %>%
  pivot_wider(names_from = analyte, values_from = samp_mean)
#glimpse(lab_pigment)

#format the sonde data------------------------

#look at analyte column 
#unique(sonde$analyte_name)
#four analytes: "Specific Conductance", "Temperature", "Turbidity", "Fluorescence"

#format the sonde data
fluor<-sonde %>% 
  #filter to just the fluorescence data
  filter(analyte_name == "Fluorescence") %>% 
  #format the date-time column
  mutate(date_time = mdy_hm(time))
 
#look at station column
#unique(fluor$cdec_code)
#six stations: "NSL" "BDL" "HUN" "RVB" "MAL" "GZL"

#look at unit name
#unique(fluor$unit_name)
#"FU"

#look at qaqc flags
#unique(fluor$qaqc_flag_id)
#"Approved"    "Provisional"
#keep this column for now because some data are provisional

#how many provisional values for each station?
prov_count<-fluor %>% 
  #filter(qaqc_flag_id == "Provisional") %>% 
  group_by(cdec_code,qaqc_flag_id) %>% 
  summarize(count = n())  
#lots of provisional data across all stations
#all data for GZL  and RVB are provisional

#create data frame that matches CDEC names and regions
cdec<-data.frame(
  cdec_code = c("NSL", "BDL", "HUN", "RVB", "MAL", "GZL"),
  region = c("ME", "ME","MW","RV", "RV","GZ")
)
#glimpse(cdec)

#use join to add region names to main fluorescence data set
fluor_reg<-left_join(fluor,cdec)
glimpse(fluor_reg)

#add a column for week of the year by extracting it from date
fluor_reg <- fluor_reg %>% 
  mutate(week = week(date_time)) 

fluor_week_sum <- fluor_reg %>% 
  #calculate mean fluorescence by week and region
  group_by(region, week) %>% 
  summarize(fluorescence = mean(value,na.rm=T))
#need the na.rm=T because some of BDL data missing

#look closer at BDL during weeks 27-31
#bdl <- fluor_reg %>% 
#  filter(cdec_code == "BDL" & week > 26 & week < 32)
#all these data are NAs; perhaps the probe malfunctioned
#but it could be some sort of data download error or something too
  
#summarize the phytoplankton community data by region and week---------

phyto_com_sum <- phyto_com %>% 
  mutate(week = week(date)) %>% 
  group_by(region, week) %>% 
  summarize(tot_den = mean(tot_den),
            tot_bvol = mean(tot_bvol)
            )

#combine the three data set and plot correlations by region-------

#create list of data sets to combine
data_list <- list(phyto_com_sum, lab_pigment, fluor_week_sum)

#join data sets by region and week
all_data <- data_list %>% 
  reduce(full_join, by = c("region","week"))  





