#Suisun Marsh Salinity Control Gate
#Continuous water quality data
#Final QAQC and formatting for EDI
#updates dataset 

#Note: this is based on the WQ file that Morgan Battey created for making the graphs in the 2024 report

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #tools for data cleaning

#read in the data
#NOTE: this file was too big for github; it's now on the SMSCG sharepoint site instead
# Define path on SharePoint site 
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data Package 2024"
  )
) 

#full WQ data set
wq<-read_csv(file = paste0(sharepoint_path,"./SMSCG_wq_data_2017-2024_final.csv")) %>% 
  arrange(station,date_time_pst) %>% 
  glimpse()

#look range of dates
#should be 1/1/2017 - 12/31/2024
range(wq$date_time_pst)
#"2017-01-01 08:00:00 UTC" "2024-11-18 18:30:00 UTC"
#not quite the full range but probably good enough
#note that time is 8 h off; need to set time zone to correct this
#we will update with the 2025 data soon

#look at number of data points by station
wq_stn_table <- wq %>% 
  group_by(station) %>% 
  tally()
#all stations have data
#a few stations have much fewer points but they are the ones where I would expect that

#format df for publishing on EDI
wq_ft <- wq %>% 
  #format date-time as character for export
  mutate(date_time_pst = with_tz(date_time_pst,tzone="Etc/GMT+8")) %>% 
  #just keep the needed columns
  select(
    station
    ,year
    ,date_time_pst
    , fluorescence_ugL =  fluorescence
    ,dissolved_oxygen_mgL = dissolvedoxygen
    ,specific_conductance_uScm = spc
    ,temperature_C = watertemperature
    ,pH = ph
    ,turbidity_FNU = turbidity
  ) %>% 
  arrange(station,date_time_pst) %>% 
  glimpse()

#write data file for publishing on EDI
#use write_excel_csv() because write_csv() will mess up date-time by converting to UTC 
#write_excel_csv(wq_ft,file = paste0(sharepoint_path,"./SMSCG_wq_data_2017-2024_final_edi.csv"))
