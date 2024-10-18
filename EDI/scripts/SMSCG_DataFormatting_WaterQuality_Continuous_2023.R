#Suisun Marsh Salinity Control Gate
#Continuous water quality data
#Final QAQC and formatting for EDI
#updates dataset with 2023 data


#required packages
library(tidyverse) #suite of data science tools
library(janitor) #tools for data cleaning
library(lubridate) #working with dates

#read in the data
#NOTE: this file was too big for github; it's now on the SMSCG sharepoint site instead
# Define path on SharePoint site 
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data Package 2023"
  )
) 

#full WQ data set
wq<-read_csv(file = paste0(sharepoint_path,"./SMSCG_wq_data_2017-2023_clean.csv")) %>% 
  arrange(date_time_pst,station) %>% 
  glimpse()
#everything looks good

#station metadata tables to combine
wqm<-read_csv(file = paste0(sharepoint_path,"./smscg_stations_wq_updated.csv")) %>% 
  glimpse()

wqg<-read_csv(file = paste0(sharepoint_path,"./wq_stations_regions_group.csv")) %>% 
  glimpse()

#integrate the two metadata files
wqmeta <-left_join(wqm,wqg) %>% 
  #move group up closer to front
  relocate(group,.after=station) %>% 
  glimpse()

#write data file for publishing on EDI
#write_csv(wqmeta,file = paste0(sharepoint_path,"./smscg_stations_wq_combined.csv"))


#make a few minor edits
#correct time zone
#adjust turbidity value column name
#reorder turbidity columns
wqz <- wq %>% 
  mutate(date_time_pst = with_tz(date_time_pst,tzone="Etc/GMT+8")) %>% 
  rename(turbidity_FNU_value = turb_FNU_value) %>% 
  relocate(turbidity_FNU_value,.after = turbidity_FNU_code) %>% 
  glimpse()

#check time zone
tz(wqz$date_time_pst)
#looks good now

#look at range of dates
#some date-time are NA
range(wqz$date_time_pst,na.rm = T)
# "2017-01-01 00:00:00 -08" "2023-11-09 06:30:00 -08"

#look at stations included
unique(wqz$station)
#"BDL" "CSE" "GOD" "GZL" "HON" "HUN" "MAL" "MSL" "NSL" "RVB" "RYC" "SSI" "VOL" "GZB" "TRB" "GZM"

#look at codes used
unique(wqz$pH_code)
unique(wqz$turbidity_FNU_code)
unique(wqz$dissolved_oxygen_mgL_code)
unique(wqz$fluorescence_ugL_code)

#write data file for publishing on EDI
#use write_excel_csv() because write_csv() will mess up date-time by converting to UTC 
#write_excel_csv(wqz,file = paste0(sharepoint_path,"./SMSCG_wq_data_2017-2023_clean_tzone.csv"))
