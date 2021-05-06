#Suisun Marsh Salinity Control Gate Action
#UC-Davis Suisun Marsh Fish Survey
#Otter trawl
#Submerged aquatic vegetation 
#Relevant water quality data

#required packages
library(tidyverse)
library(janitor) #function for data frame clean up
library(lubridate) #format dates

# Read in the Data----------------------------------------------
# Datasets are on SharePoint site for the SMSCG action

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data/WaterQuality"
  )
)  

#read in DWR water quality station data files and combine them
#NOTE: something wrong with formatting of time column for BLL file so read it separately
dwr_files <- dir(sharepoint_path, pattern = "data.csv", full.names = T)
dwr <- map_df(dwr_files, ~ read_csv(.x, col_types = "dcccdTdcdd")) %>% 
  bind_rows()
str(dwr)

#read in BLL station data
bll<-read_csv(file = paste0(sharepoint_path,"/BLL River.csv"), col_types = "dcccdcdcdd")
str(bll)

#format BLL data a bit before combining with rest of DWR data
bll_cleaner <- bll %>% 
  #format date-time
  mutate(time = mdy_hm(time)) %>% 
  #replace some analyte names to match counterparts in rest of DWR data set
  mutate(across("analyte_name", str_replace,"Temp River","Temperature")) %>% 
  mutate(across("analyte_name", str_replace,"SC River","Specific Conductance"))  
str(bll_cleaner)

#look at time range for BLL station data
range(bll_cleaner$time)
#"2014-01-01 00:00:00 UTC" "2019-07-31 23:45:00 UTC"
#so this station isn't active

#combine all DWR station data
dwr_all <- rbind(dwr,bll_cleaner)
str(dwr_all)

#read in water quality data from the National Estuarine Research Reserve System (NERRS) stations
#for this analysis, just need SFBFMWQ 
nerr_data<-read_csv(file = paste0(sharepoint_path,"./NERRS_Data/SFBSMWQ_SFBFMWQ_2014-01-01_2021-04-01.csv"))

#DWR: explore data set---------------

#look at QAQC flag types
#need to figure out what they mean
unique(dwr_all$qaqc_flag_id)
#"G" "X" "M" "A" "U" "B"

#look closer at flag types
tabyl(dwr_all$qaqc_flag_id)
#68% are G, which is probably the data I want

#look at station names
unique(dwr_all$cdec_code)
#"GOD" "MSL" "NSL" "BLL"
#looks good

#look at list of analytes
unique(dwr_all$analyte_name)
#"Specific Conductance" "Temperature", "Turbidity", "Stage River" "DO River" 
#just subset to keep specific conductance and temperature
#none of the other analytes are consistently available across stations and dates

#look unique combinations of station and analyte
unique(dwr_all[,c('cdec_code',"analyte_name")])

#look at date range
range(dwr_all$time)
#"2014-01-01 UTC" "2021-04-01 UTC"

#DWR: format data set--------------

dwr_cleaner <- dwr_all %>%
  #filter to keep only temperature and specific conductance data
  #and only data with "G" for the QAQC code
  filter((analyte_name == "Specific Conductance" | analyte_name == "Temperature") 
         & qaqc_flag_id == "G")
unique(dwr_cleaner$analyte_name)
unique(dwr_cleaner$qaqc_flag_id)

#DWR: plot time series of data sets by station and analyte------------



#DWR: calculate monthly means for temperature and specific conductance-------



#format NERR data set--------------

nerr_cleaner <- nerr_data %>% 
  #just want the date-time column and the SFBFMWQ station columns
  select(c( DateTimeStamp | contains("SFBFMWQ"))) %>%
  #simplify column names for remaining station
  rename_at(.vars = vars(starts_with("SFBFMWQ_")),
            .funs = funs(sub("SFBFMWQ_", "", .))) %>% 
  #format date-time column
  mutate(date_time = mdy_hm(DateTimeStamp))
#rename columns

#glimpse(nerr_cleaner)
#temperature (C)
#Specific conductivity (mS/cm)
#salinity (ppt)
#DO (%)
#DO (mg/L)
#sonde depth (m) 
#pH
#turbidity (NTU)
#fluorescence (ug/L)

#explore NERR data--------------

#look at QC flag codes for specific conductance
unique(nerr_cleaner$F_SpCond)
#lots of categories
#keep the ones that include <0> which indicates pass of initial QAQC checks

#filter to only keep the rows that include specific conductance values 
#that passed initial QAQC checks
nerr_filter<-nerr_cleaner %>% 
  filter(grepl("<0>",F_SpCond))

#unique(nerr_filter$F_SpCond)
#looks like the filter worked correctly

#histogram of remaining salinity values
hist(nerr_filter$SpCond)

#histogram of remaining temperature values
hist(nerr_filter$Temp)
#looks OK

#how do flags for temperature look in filtered data set
unique(nerr_filter$F_Temp)
#all remaining temp data passed initial QAQC

#histogram of remaining turbidity values
hist(nerr_filter$Turb)
#turbidity still has problems

#for now, just focus on specific conductance and temperature
#turbidity probably matters too but isn't available across all WQ stations
nerr_sub<-nerr_filter %>% 
  #use janitor package to get rid of capital letters in column names
  clean_names() %>% 
  #create a month column which will be used to calculate monthly means
  mutate(month = month(date_time)) %>% 
  #create a year column 
  mutate(year = year(date_time)) %>% 
  #subset columns and reorder them
  select(station_code, date_time, year, month, temp, sp_cond) %>% 
  #rename station_code to wq to then join with sav data set
  rename(wq = station_code)

#calculate monthly means for specific conductance and temperature
nerr_month<-nerr_sub %>% 
  group_by(wq,year,month) %>% 
  summarize(
    temp_avg = mean(temp),
    sp_cond_avg = mean(sp_cond)) %>% 
  #make year and month character for plotting
  mutate_at(vars(year, month),factor)

glimpse(nerr_month)

#specific conductance: make boxplots by month
(plot_nerr_bx<-ggplot(data=nerr_month, aes(x = month, y = sp_cond_avg)) + 
    geom_boxplot()+
    geom_jitter() #adds all points to plot, not just outliers
)

#temperature: make boxplots by month
(plot_nerr_bx<-ggplot(data=nerr_month, aes(x = month, y = temp_avg)) + 
    geom_boxplot()+
    geom_jitter() #adds all points to plot, not just outliers
)