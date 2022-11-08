#Suisun Marsh Salinity Control Gate
#Continuous water quality data
#Final formatting for EDI
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
    "California Department of Water Resources/SMSCG - Summer Action - Data Package 2021"
  )
) 

wq<-read_csv(file = paste0(sharepoint_path,"./WQdata_smscg_2018-2021 (Flagged Bad Removed).csv"))

#find and replace micro with "u"
wq_final <- wq %>% 
  mutate(unit_name2 = case_when(grepl("S/cm", unit_name) ~ "uS/cm"
                                ,grepl("g/L", unit_name) ~ "ug/L"
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
#"uS/cm" "ug/L"  "NTU"   "FNU"  
  
#write_csv(wq_final,file = paste0(sharepoint_path,"./WQdata_smscg_2018-2021_FlaggedBadRemoved_clean.csv"))
