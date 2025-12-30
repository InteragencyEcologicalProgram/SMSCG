#Suisun Marsh Salinity Control Gate
#EDI data files

#Just need to format date for a few files
#zoop and gate log

#required packages
library(tidyverse) #suite of data science tools
library(lubridate) #working with dates

#read in data

#all mesozooplankton data
zoop_cb <- read_csv("./EDI/data_input/zooplankton/SMSCG_CBNet_2018to2024CPUE_14Apr2025.csv") 

#2018-2019 mysid data
#zoop_mysid <- read_csv("./EDI/data_input/zooplankton/SMSCG_MysidNet_CPUE_2018to2019_24Jan2022.csv") 
#glimpse(zoop_mysid)

#read in zoop station metadata file to make sure the metadata and data files match up
#zoop_stn <- read_csv("./EDI/data_output/smscg_stations_zoop.csv")

#gates operations log
gatelog <- read_csv("./EDI/data_input/SMSCG_GateDailyLog_1988-2024.csv") 
#glimpse(gatelog)

#format dates-------------------

#format CB zoop data date
zoop_cb2 <- zoop_cb %>% 
  mutate(Date = mdy(Date)
         ,Date = as.character(Date)
         ) %>% 
  glimpse()

#format mysid data date
# zoop_mysid2 <- zoop_mysid %>%
# mutate(Date = mdy(Date)) %>%
# glimpse()

#look at unique mysid stations
#unique(zoop_mysid2$Station)
# "Honk" "Mont" "405"  "411"  "501"  "504"  "416"  "602"  "606"  "802"  "508"  "513"  "519"  "704" 
#"706"  "707"  "520"  "609"  "610"  "MONT" "HONK"
#need to format HONK and MONT so they are consistent

#format mysid stations
# zoop_mysid3 <- zoop_mysid2 %>% 
#   mutate(Station = case_when(Station == "Mont" ~ "MONT"
#                              ,Station == "Honk" ~ "HONK"
#                              ,TRUE ~ Station))
#unique(zoop_mysid3$Station)

#try combining data and metadata and see if any mismatches
#comb <- left_join(zoop_mysid3, zoop_stn)
#station matched fine between dfs


#format gates operations data
gatelog2 <- gatelog %>% 
  #format date
  mutate(Date = mdy(Date)
         ,Date = as.character(Date)
         ) %>% 
  #drop a row of NAs
  #filter(!is.na(Date)) %>% 
  glimpse()

#write updated files----------

#write_csv(gatelog2,"./EDI/data_output/SMSCG_GateDailyLog_1988-2024.csv")

#write_csv(zoop_cb2,"./EDI/data_output/SMSCG_Zooplankton_CBNet_2018-2024.csv")

#write_csv(zoop_mysid3,"./EDI/data_output/SMSCG_Zooplankton_Mysid_2018to2019.csv")




