#Suisun Marsh Salinity Control Gate
#Continuous water quality data
#Final QAQC and formatting for EDI

#required packages
library(tidyverse) #suite of data science tools
library(lubridate) #working with dates

#read in the data
#NOTE: this file was too big for github; it's now on the SMSCG sharepoint site instead
# Define path on SharePoint site 
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data Package 2022"
  )
) 

wq<-read_csv(file = paste0(sharepoint_path,"./SMSG Data 2018-2022 (Flagged Bad Removed).csv"))

#make sure all the stations are in the file
stn_names <- unique(wq$cdec_code)
#"CSE" "NSL" "MSL" "HUN" "GOD" "BDL" "VOL" "GZB" "GZM" "TRB" "MAL" "GZL" "HON" "RYC" "SSI"
#looks good

#check that all parameters are in file
parameter <- unique(wq$analyte_name)
# "Water Temperature"    "Specific Conductance" "Dissolved Oxygen"     "Chlorophyll"          "Turbidity"            "pH"          
#looks good

#check that the full range of dates are in file
range(wq$time)
#"2018-01-01 UTC" "2023-01-01 UTC"
#range looks good; might need to work on time zone though

#look at unit names
unique(wq$unit_name)
#"\xb0C"        "\xb5S/cm"     "mg/L"         "\xb5g/L"      "NTU"          "pH Units"     "% saturation" "FNU"     
#a few of these need work; degree and micro symbols didn't translate

#find and replace micro with "u" and drop degrees from Celsius
wq_final <- wq %>% 
  mutate(unit_name2 = case_when(grepl("\xb5S/cm", unit_name) ~ "uS/cm"
                                ,grepl("\xb0C", unit_name) ~ "C"
                                ,grepl("\xb5g/L", unit_name) ~ "ug/L" 
                                ,TRUE ~ unit_name)
         ,date_time_pst = ymd_hms(time,tz="Etc/GMT+8")
         ,year = year(date_time_pst)
         )  %>% 
  select(cdec_code    
         ,analyte_name 
         ,unit_name = unit_name2 
         #,time 
         ,date_time_pst
         ,year
         ,value       
         ,qaqc_flag_id) %>% 
  #drop a few stray 2023 values
  filter(year < 2023) %>% 
  glimpse()

#check units again after fixing them
unique(wq_final$unit_name)
#"C"            "uS/cm"        "mg/L"         "ug/L"         "NTU"          "pH Units"     "% saturation" "FNU"
#looks good now

#check that the date/time again after setting time zone
range(wq_final$date_time_pst)
#""2018-01-01 -08" "2023-01-01 -08"
#formatting looks odd with this output but looks fine when viewing df

#look at summary of data points by station, parameter
wq_summary_sp <- wq_final %>% 
  group_by(cdec_code,analyte_name) %>% 
  count() %>% 
  ungroup() %>% 
  glimpse()

#make df with all combos of station and parameter
#NOTE: had to make sure to ungroup above to get this to work
stn_par_combo <- wq_summary_sp %>% 
  select(cdec_code,analyte_name) %>% 
  expand(cdec_code,analyte_name)
#15 stations x 6 parameters = 90 so looks right

#determine which parameters are missing from stations 
stn_par_missing <- anti_join(stn_par_combo,wq_summary_sp)
#came up with 10 missing station-parameter combos as expected
#based on CDED metadata, none of these pH and chlorophyll parameters should be missing

#quick check to see if these are missing from original df
hon_ph <- wq %>% 
  filter(cdec_code=="HON" & analyte_name=="pH")
#it is true that this combo is missing from the data set

#look at summary of data points by station, parameter, and year
wq_summary_spy <- wq_final %>% 
  group_by(cdec_code,analyte_name,year) %>% 
  count() %>% 
  ungroup() %>% 
  glimpse()
#seems like some parameters are missing

#quick plot of data points
#NOTE: this isn't a very good way to plot the data because of overlap of points/lines
(plot_wq <- ggplot(wq_summary_spy,aes(x = year, y = n, group = analyte_name, color=analyte_name)) +
  geom_line()+
  geom_point()+
  facet_wrap(~cdec_code)
)

#write final file for publishing on EDI
#write_csv(wq_final,file = paste0(sharepoint_path,"./smscg_data_water_quality.csv"))
