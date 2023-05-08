#Suisun Marsh Salinity Control Gate
#Continuous water quality data
#Final QAQC and formatting for EDI
#Just need to find and replace the micro symbol with "u"
#because micro is apparently not allowed

#required packages
library(tidyverse) #suite of data science tools

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
unique(wq$cdec_code)
#"CSE" "NSL" "MSL" "HUN" "GOD" "BDL" "VOL" "GZB" "GZM" "TRB" "MAL" "GZL" "HON" "RYC" "SSI"
#looks good

#check that all parameters are in file
unique(wq$analyte_name)
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
                                ,TRUE ~ unit_name))  %>% 
  select(-unit_name) %>% 
  rename(unit_name = unit_name2) %>% 
  select("cdec_code"    
         ,"analyte_name" 
         ,"unit_name"    
         ,"time"         
         ,"value"       
         ,"qaqc_flag_id") %>% 
  glimpse()

unique(wq_final$unit_name)
#"C"            "uS/cm"        "mg/L"         "ug/L"         "NTU"          "pH Units"     "% saturation" "FNU"
#looks good now
  
#write_csv(wq_final,file = paste0(sharepoint_path,"./smscg_data_water_quality.csv"))
