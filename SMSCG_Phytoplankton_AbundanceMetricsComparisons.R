#SMSCG
#phytoplantkon
#comparisons among data types:
#Organisms/mL
#Biovolume (microns^3)/mL
#Carbon/mL
#Extracted Chlor-a
#Sonde Fluorescence

#load packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)

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

#read in the sonde data, including fluorescence used to estimate primary productivity
sonde<-read_csv(file = paste0(sharepoint_path,"./2020 data for USBR/des_data.csv"))

#read in extracted chlorophyll-a concentrations
chlora<-read_excel(path = paste0(sharepoint_path,"./2020 data for USBR/DWR SuisunMarsh Chl All Stations WY2020.xlsx"))









