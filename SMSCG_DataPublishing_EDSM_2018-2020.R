#Suisun Marsh Salinity Control Gate (SMSCG) Action

#Prepare data associated with project for publishing 
#in repository on Environmental Data Initiative

#This file prepares data from the United States Fish and Wildlife Service
#Enhanced Delta Smelt Monitoring (EDSM) surveys
#https://www.fws.gov/lodi/juvenile_fish_monitoring_program/jfmp_index.htm

#EDSM already has a repository on EDI with all their data
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.415.3

#This code creates the subset relevant to the SMSCG action
#Years: 2018, 2019, 2020 
#Months: June - October
#Survey regions: Suisun Marsh, Suisun Bay, Lower Sacramento

#required packages
library(tidyverse)
library(lubridate)

#get most of EDSM kodiak trawl data from EDI
#doesn't include most of 2020 data as of 2021-03-16
#provisional version of 2020 data stored on SMSCG SharePoint site (received 2020-11-17)
edsm_main<-read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.3&entityid=4d7de6f0a38eff744a009a92083d37ae") 
#get a warning when importing data
#Warning: 154432 parsing failures

#look at parsing failures from EDI data set--------------

#The problems() function can be used to create a data frame of the parsing failures
parse_failures<-problems(edsm_main)

#look at columns with failures
unique(parse_failures$expected) 
#"1/0/T/F/TRUE/FALSE"

unique(parse_failures$col)
#"SpecialStudy"   "PairedStudy"    "Tide"           "SpecialStudyID" "StartDepth" 
#In short, R couldn't accurately determine a column type from the first 1,000 rows 
#for these four columns because they were NAs
#Could easily specify different column types for these columns if necessary

#look at structure of EDI data frame-----------

glimpse(edsm_main)
#most of the column types look correct
#importantly this appears true for the 'Date' column

#look at range of dates for the data set
range(edsm_main$Date)
#"2016-12-15" "2020-03-26"
#doesn't yet include bulk of 2020 data

#Combine 2020 data set with rest of data-------


#Create subset of data set relevant to SMSCG action------

#Create data just for summer-fall (June to October)
edsm_sub<-edsm_main %>% 
  filter(
    month(Date) %in% c(6:10)
         )
















