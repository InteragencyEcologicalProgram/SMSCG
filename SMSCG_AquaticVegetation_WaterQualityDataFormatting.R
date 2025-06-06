#Suisun Marsh Salinity Control Gate Action
#UC-Davis Suisun Marsh Fish Survey
#Otter trawl
#Submerged aquatic vegetation 
#Relevant water quality data from DWR and NERR

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
#str(dwr)

#read in BLL station data
bll<-read_csv(file = paste0(sharepoint_path,"/BLL River.csv"), col_types = "dcccdcdcdd")
#str(bll)

#format BLL data a bit before combining with rest of DWR data
bll_cleaner <- bll %>% 
  #format date-time
  mutate(time = mdy_hm(time)) %>% 
  #replace some analyte names to match counterparts in rest of DWR data set
  mutate(across("analyte_name", str_replace,"Temp River","Temperature")) %>% 
  mutate(across("analyte_name", str_replace,"SC River","Specific Conductance"))  
#str(bll_cleaner)

#look at time range for BLL station data
range(bll_cleaner$time)
#"2014-01-01 00:00:00 UTC" "2019-07-31 23:45:00 UTC"
#so this station isn't active

#combine all DWR station data
dwr_all <- rbind(dwr,bll_cleaner)
#str(dwr_all)

#read in water quality data from the National Estuarine Research Reserve System (NERRS) stations
#for this analysis, just need SFBFMWQ 
nerr_data<-read_csv(file = paste0(sharepoint_path,"./NERRS_Data/SFBSMWQ_SFBFMWQ_2014-01-01_2021-04-01.csv"))

#DWR: explore data set---------------

#look at QAQC flag types
#need to figure out what they mean
#unique(dwr_all$qaqc_flag_id)
#"G" "X" "M" "A" "U" "B"

#look closer at flag types
tabyl(dwr_all$qaqc_flag_id)
#68% are G, which is probably the data I want

#look at station names
#unique(dwr_all$cdec_code)
#"GOD" "MSL" "NSL" "BLL"
#looks good

#look at list of analytes
#unique(dwr_all$analyte_name)
#"Specific Conductance" "Temperature", "Turbidity", "Stage River" "DO River" 
#just subset to keep specific conductance and temperature
#none of the other analytes are consistently available across stations and dates

#look unique combinations of station and analyte
#unique(dwr_all[,c('cdec_code',"analyte_name")])

#look at date range
range(dwr_all$time)
#"2014-01-01 UTC" "2021-04-01 UTC"

#DWR: format data set--------------

dwr_cleaner <- dwr_all %>%
  #rename station column
  rename(wq = cdec_code) %>% 
  #filter to only keep with "G" (good) which are mainly what I want
  #also kept "U" (unchecked) because otherwise there are big time gaps in data
  filter(qaqc_flag_id == "G" | qaqc_flag_id == "U") %>% 
  #subset and reorder columns
  select(wq
         ,time
         ,analyte_name
         ,value) 

#create subset with just temperature data
dwr_temp <- dwr_cleaner %>% 
  filter(analyte_name == "Temperature" 
         #remove some high outliers
         & value < 40)  %>% 
  rename(temp = value) %>% 
  select(wq
         ,time
         ,temp)
#hist(dwr_temp$value)
  
dwr_sc <- dwr_cleaner %>% 
  filter(analyte_name == "Specific Conductance"
         #remove some high outliers
         & value < 27000)%>% 
  rename(sp_cond = value) %>% 
  select(wq
         ,time
         ,sp_cond)

#join temp and specific conductance data sets
dwr_ts <- full_join(dwr_temp, dwr_sc) %>% 
  mutate(month = month(time)) %>% 
  #create a year column 
  mutate(year = year(time))  

#glimpse(dwr_ts)


#DWR: plot time series of data sets by station and analyte------------

#temperature plot
(plot_d_temp <- ggplot(dwr_ts, aes(x=time, y=temp))+
  geom_line() + 
  labs(x = "Time", y = "Temperature (C)") + 
  theme_minimal() +
  facet_wrap(~wq)
)
#there are chunks of missing data for GOD and NSL
#could be due to data not flagged as "G" or "U"

#specific conductance plot
(plot_d_sc <- ggplot(dwr_ts, aes(x=time, y=sp_cond))+
    geom_line() + 
    labs(x = "Time", y = "Specific Conductance (uS/cm") + 
    theme_minimal() +
    facet_wrap(~wq)
)
#chunks of data missing for GOD NS  also some clearly bad data for GOD 

#DWR: calculate monthly means by analyte-------

#calculate monthly means for specific conductance and temperature
dwr_month <- dwr_ts %>% 
  group_by(wq,year,month) %>% 
  summarize(
    temp_avg = mean(temp,na.rm=T),
    sp_cond_avg = mean(sp_cond,na.rm=T)) %>% 
  #make year and month character for plotting
  mutate_at(vars(year, month),factor)


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
#Specific conductivity (mS/cm); change to uS/cm!
#salinity (ppt)
#DO (%)
#DO (mg/L)
#sonde depth (m) 
#pH
#turbidity (NTU)
#fluorescence (ug/L)

#explore NERR data--------------

#look at QC flag codes for specific conductance
#unique(nerr_cleaner$F_SpCond)
#lots of categories
#keep the ones that include <0> which indicates pass of initial QAQC checks

#filter to only keep the rows that include specific conductance values 
#that passed initial QAQC checks
#also convert from mS/cm to uS/com
nerr_filter<-nerr_cleaner %>% 
  filter(grepl("<0>",F_SpCond)) %>% 
  mutate(SpCond2 = SpCond*1000)

#unique(nerr_filter$F_SpCond)
#looks like the filter worked correctly

#histogram of remaining salinity values
#hist(nerr_filter$SpCond2)

#histogram of remaining temperature values
#hist(nerr_filter$Temp)
#looks OK

#how do flags for temperature look in filtered data set
#unique(nerr_filter$F_Temp)
#all remaining temp data passed initial QAQC

#histogram of remaining turbidity values
#hist(nerr_filter$Turb)
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
  select(station_code, date_time, year, month, temp, sp_cond2) %>% 
  #rename station_code to wq to then join with sav data set
  rename(wq = station_code)

#specific conductance plot
(plot_n_sc <- ggplot(nerr_sub, aes(x=date_time, y=sp_cond2))+
    geom_line() + 
    labs(x = "Time", y = "Specific Conductance (uS/cm)") + 
    theme_minimal() +
    facet_wrap(~wq)
)

#calculate monthly means for specific conductance and temperature
nerr_month<-nerr_sub %>% 
  group_by(wq,year,month) %>% 
  summarize(
    temp_avg = mean(temp),
    sp_cond_avg = mean(sp_cond2)) %>% 
  #make year and month character for plotting
  mutate_at(vars(year, month),factor)

#glimpse(nerr_month)

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

#combine DWR and NERR data sets-----------
#glimpse(dwr_month)
#glimpse(nerr_month)

final <- rbind(dwr_month,nerr_month)

#write_csv(final,file = paste0(sharepoint_path,"/AquaticVegetation_WQ_SummarizedData.csv"))






