#SMSCG
#phytoplantkon
#comparisons among data types:
#Organisms/mL
#Biovolume (microns^3)/mL
#Carbon/mL
#Extracted Chlor-a
#Sonde Fluorescence

#load packages
library(tidyverse) #suite of packages for working with data
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
glimpse(chlora)
#looks like columns are formatted correctly even the date column; oddly there's no time data
#lots of columns I won't need, so should pare down the data set

#format the extracted chlorophyll-a data set-----------------

#look at list of station names 
unique(chlora$QST_NAME)
unique(chlora$QST_FULL_STATION_NAME)
#looks like they are probably all unique names
#but many are long, so maybe keep the station number too

#look at station number
unique(chlora$QST_NUMBER)
#same number of stations as with station name
#so confirms that the station names are all truly unique

#look at reporting limit
unique(chlora$RES_REPORTING_LIMIT)
#0.5 1.0 2.5
#there are three values so keep this column

#look at sample depth
#are all samples collected at same depth?
unique(chlora$SAM_DEPTH)
#yes, all collected 1 m from surface
#so don't need to keep this column

#look at sample type column
unique(chlora$SPP_DESCRIPTION)
#"Normal Sample"    "Duplicate Sample"
#makes sense

#look at sample description
unique(chlora$SAM_DESCRIPTION)
#"Chlorophyll A Grab Sample"
#all the same so don't need this column

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
    ,units = UNS_NAME
    ,sample_type = SPP_DESCRIPTION) %>% 
  #subset and reorder columns to just those needed
  select(station_name 
         ,station_code 
         ,date
         ,analyte 
         ,result 
         ,report_limit 
         ,units 
         ,sample_type)
glimpse(chlora_sub)

#are there values below the reporting limit in this data set?
chlora_sub$diff<-chlora_sub$result-chlora_sub$report_limit
replim<-chlora_sub %>% 
  filter(diff<0)
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

#at least for now, just average the normal and duplicate samples from a given site/date
chlora_samp_mean<-chlora_reg %>% 
  group_by("station_name","station_code","region","date","analyte","units") %>% 
  summarize(samp_mean = mean(result))


#make sure all data frame columns are formatted correctly

#rough regional comparisons (would be fairly quick to do)
#assign stations to regions for continuous and discrete WQ samples
#summarize these data by region and month
#summarize phyto community data by region and month (see data visualization script for code to do this)
#plot correlations between pairs of metrics

#more sophisticated approach (would take much more work)
#try to pair specific sampling stations across sample types
   #need to match based on distances and also times
#keep track of how large the differences in space and time are between samples compared
#could then plot correlation coefficents against things like time and space differences
#would indicate how much mismatches in space and time affect similarities among metrics






