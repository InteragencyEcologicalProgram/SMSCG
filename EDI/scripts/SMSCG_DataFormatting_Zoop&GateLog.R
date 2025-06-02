#Suisun Marsh Salinity Control Gate
#EDI data files

#Just need to format date for a few files
#zoop and gate log

#required packages
library(tidyverse) #suite of data science tools
library(lubridate) #working with dates

#read in data

#all mesozooplankton data
zoop_cb <- read_csv("./EDI/data_input/zooplankton/SMSCG_CBNet_2018to2023CPUE_07Feb2024.csv") 

#2018-2019 mysid data
#zoop_mysid <- read_csv("./EDI/data_input/zooplankton/SMSCG_MysidNet_CPUE_2018to2019_24Jan2022.csv") 
#glimpse(zoop_mysid)

#gates operations log
gatelog <- read_csv("./EDI/data_input/SMSCG_GateDailyLog_1988-2023.csv") 
glimpse(gatelog)

#format dates-------------------

#format CB zoop data date
zoop_cb2 <- zoop_cb %>% 
  mutate(Date = mdy(Date)
         ,Date = as.character(Date)
         ) %>% 
  glimpse()

#format mysid data date
#zoop_mysid2 <- zoop_mysid %>% 
 # mutate(Date = mdy(Date)) %>% 
  #glimpse()

#format gates operations data
gatelog2 <- gatelog %>% 
  #format date
  mutate(Date = mdy(Date)
         ,Date = as.character(Date)
         ) %>% 
  #drop a row of NAs
  filter(!is.na(Date)) %>% 
  glimpse()

#write updated files----------

#write_csv(gatelog2,"./EDI/data_output/SMSCG_GateDailyLog_1988-2023.csv")

#write_csv(zoop_cb2,"./EDI/data_output/SMSCG_Zooplankton_CBNet_2018-2023.csv")

#write_csv(zoop_mysid2,"./EDI/data_output/SMSCG_Zooplankton_Mysid_2018to2019.csv")




