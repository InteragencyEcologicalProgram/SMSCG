#Suisun Marsh Salinity Control Gate
#Continuous water quality data
#Final QAQC and formatting for EDI

#To do-----
#make sure to make time character before exporting data
#add RVB station data
#this data set still needs some QAQC to deal with outliers for at least some stations for 
#most analytes
#compare range of dates for each parameter within each station to CDEC metadata
#track down data for tule red station (TRB) prior to 2021-08-24 15:42:00)
#maybe use PSU instead of EC

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #tools for data cleaning
library(lubridate) #working with dates

#read in the data
#NOTE: this file was too big for github; it's now on the SMSCG sharepoint site instead
# Define path on SharePoint site 
sharepoint_path_input <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data Package 2022/wq_data_input"
  )
) 

sharepoint_path_output <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data Package 2022"
  )
) 

#full data set
wq<-read_csv(file = paste0(sharepoint_path_input,"./SMSG Data 2018-2022 (Flagged Bad Removed).csv")) %>% 
  glimpse()
#everything looks good

#new data to replace some of CSE data
#in full data set 
cse_ec<-read_csv(file = paste0(sharepoint_path_input,"./CSE_SpC_05_15_2018-06_12_2018.csv"))%>% 
  glimpse()
cse_temp<-read_csv(file = paste0(sharepoint_path_input,"./CSE_Temp_05_15_2018-06_12_2018.csv"))%>% 
  glimpse()

#new data to replace some of MAL data
mal_sc<-read_csv(file = paste0(sharepoint_path_input,"./MAL_SC.csv"))%>% 
  glimpse()
mal_temp<-read_csv(file = paste0(sharepoint_path_input,"./MAL_Temp.csv"))%>% 
  glimpse()

#add RVB data
#eventually just get this from EDI
rvb<-read_csv(file = paste0(sharepoint_path_input,"./RVB_data_final.csv"))%>% 
  glimpse()

#read in the previous version of SMSCG WQ data from EDI
#wq_smscg <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.6&entityid=aae1f07aefeb88eef5eea4c6f3dc1bf0")

#integrated discrete WQ data set
#iwq <-read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.7&entityid=6c5f35b1d316e39c8de0bfadfb3c9692")

#make sure all the stations are in the main file
#stn_names <- unique(wq$cdec_code)
#"CSE" "NSL" "MSL" "HUN" "GOD" "BDL" "VOL" "GZB" "GZM" "TRB" "MAL" "GZL" "HON" "RYC" "SSI"
#looks good

#check that all parameters are in file
#parameter <- unique(wq$analyte_name)
# "Water Temperature"    "Specific Conductance" "Dissolved Oxygen"     "Chlorophyll"          "Turbidity"            "pH"          
#looks good

#check that the full range of dates are in file
#range(wq$time)
#"2018-01-01 UTC" "2023-01-01 UTC"
#range looks good; need to work on time zone though

#look at unit names
#unique(wq$unit_name)
#"\xb0C"        "\xb5S/cm"     "mg/L"         "\xb5g/L"      "NTU"          "pH Units"     "% saturation" "RFU"          "FNU"     
#a few of these need work; degree and micro symbols didn't translate


#look at all combos of parameter and units
#par_unit <- wq %>% 
 # distinct(analyte_name,unit_name) %>% 
  #arrange(analyte_name)
#chlorophyll: two types of units (RFU, ug/L)
#turbidity: two types of units (NTU, FNU); I think they're considered pretty interchangeable
#DO: two types of units (mg/L, % saturation)
#for these, do we have two sets of values for each date, a change through time in units, 
#or a mix of the two?

#look at all combos of station, parameter, and units
#par_unit_stn <- wq %>% 
 # distinct(cdec_code, analyte_name,unit_name) %>% 
  #arrange(analyte_name,cdec_code)

#par_unit_stn_odd <- par_unit_stn %>% 
 # group_by(cdec_code,analyte_name) %>% 
  #count() %>% 
  #ungroup() %>% 
  #filter(n>1)
#5 stations have more than one unit type for at least one analyte

#stn_odd <- par_unit_stn_odd %>%
  #select(cdec_code) %>% 
 # distinct(cdec_code) %>% 
  #pull(cdec_code)

#look at how many data points per station,analyte,unit
#par_unit_stn_sum <- wq %>% 
 # filter(cdec_code %in% stn_odd) %>% 
  #group_by(cdec_code,analyte_name,unit_name) %>% 
  #count() %>% 
  #arrange(cdec_code,analyte_name)
#Except for GZL, can ignore the RFU and % saturation units because only one each 
#need to look closer at FNU vs NTU for turbidity; probably just changed at some point
#look at date range for each unit type by station

#turbidity units
#turb_odd <- wq %>% 
 # filter(
    #cdec_code %in% stn_odd & 
  #  analyte_name=="Turbidity") %>% 
  #group_by(cdec_code,unit_name) %>% 
  #summarise(min = min(time)
   #         ,max = max(time)
    #        ,.groups = 'drop') %>% 
  #arrange(
   # cdec_code
    #,unit_name
    #min
   # )
#so looks like probably NTU was old unit
#then some stations switched to FNU with a period of overlap for unit comparison
#some stations have stayed on NTU so only one unit type
#let's just keep both types of data in the data set

#now look closer at GZL chlorophyll
#chlor_odd <- wq %>% 
 # filter(cdec_code == "GZL" & analyte_name == "Chlorophyll")

#chlor_odd_summary <- chlor_odd %>% 
 # group_by(unit_name) %>% 
  #summarise(min = min(time)
   #         ,max = max(time)) 

#quick plot of GZL Chlorophyll by unit
#(p_gzl_chlor <- ggplot(chlor_odd, aes(x = time,y=value,color=unit_name)) + 
 # geom_line()
#)
#so always ug/L but in recent years also RFU
#overall, just keep ug/L for all stations and drop RFU

#now look at DO for GZL too
#do_odd <- wq %>% 
 # filter(cdec_code == "GZL" & analyte_name == "Dissolved Oxygen")

#do_odd_summary <- do_odd %>% 
 # group_by(unit_name) %>% 
  #summarise(min = min(time)
   #         ,max = max(time)) 

#quick plot of GZL DO by unit
#(p_gzl_do <- ggplot(do_odd, aes(x = time,y=value,color=unit_name)) + 
  #  geom_line()
#)
#always has mg/L and % saturation included in recent years too
#just keep mg/L and drop % saturation for all stations

#clean up and format data frame
wq_final <- wq %>% 
  #drop the less used types of measurements for chlorophyll and DO
  filter(unit_name!="RFU" & unit_name!="% saturation") %>% 
  mutate(
    #find and replace micro with "u" and drop degrees from Celsius
    #this isn't working anymore
    #unit_name2 = case_when(grepl("\xb5S/cm", unit_name) ~ "uS/cm"
     #                           grepl("\xb0C", unit_name) ~ "C"
      #                          ,grepl("\xb5g/L", unit_name) ~ "ug/L" 
       #                         ,TRUE ~ unit_name)
    unit_name2 = case_when(analyte_name=="Water Temperature" ~ "C"
                            ,analyte_name=="Chlorophyll" ~"ug/L"
                            ,analyte_name=="Specific Conductance" ~ "uS/cm"
                            ,TRUE ~ unit_name
    )
    #specify time zone 
    ,date_time_pst = force_tz(time,tzone="Etc/GMT+8")
    #create year column from date-time column
    ,year = year(date_time_pst)
           )  %>% 
select(cdec_code    
       ,analyte_name 
       ,unit_name = unit_name2 
       ,date_time_pst 
       ,year
       ,value       
       ,qaqc_flag_id) %>%
  #drop a few stray 2023 values
  filter(year < 2023) %>% 
  glimpse()

#check times
# tz(wq$time)
# range(wq$time)
# tz(wq_final$date_time_pst)
# range(wq_final$date_time_pst)

#create dataframe with NAs for value
#wq_final_na <- wq_final %>% 
 # filter(is.na(value))
#537482 rows with NA for value
#remove these records

#create dataframe with NAs for value for original data set
#could be more NAs because this is before I did some filtering out of rows
#wq_final_na_orig <- wq %>% 
 # filter(is.na(value))
#540214 rows with NA for value
#so more as expected

#what are codes for those NAs?
#wq_final_na_orig_sum <- wq_final_na_orig %>% 
 # group_by(qaqc_flag_id) %>% 
  #count()
#mostly U (makes sense) but also some G (which is odd)

#summarize NAs by station, analyte, and flag
#wq_final_na_stn_sum <- wq_final_na_orig %>% 
 # group_by(cdec_code,analyte_name,qaqc_flag_id) %>% 
  #count()
#write_csv(wq_final_na_stn_sum,"./EDI/data_input/wq/smscg_wq_na.csv")

#look at cases of exact duplicates
#wq_final_dup_sub <- wq_final %>%
 # group_by_all() %>%
  #filter(n()>1) %>%
  #ungroup() %>% 
  #arrange(cdec_code, analyte_name,date_time_pst,value)

#count duplicates
#wq_final_dup <- wq_final_dup_sub %>%
 # add_count(cdec_code,analyte_name,unit_name,date_time_pst, year,value,qaqc_flag_id) %>%
  #filter(n>1) %>%
  #distinct() %>% 
  #arrange(cdec_code,analyte_name,date_time_pst) %>% 
  #mutate(n = as.numeric(n)) %>% 
  #glimpse()
#33,523 exact duplicates

#are there always two duplicates?
#wq_final_dup_count <- wq_final_dup %>% 
 # filter(n!=2)
#yes, always two

#summarize duplicates by station, analyte, flag
#wq_final_dup_stn_sum <- wq_final_dup %>% 
 # group_by(cdec_code,analyte_name,qaqc_flag_id) %>% 
  #count()
#write_csv(wq_final_dup_stn_sum,"./EDI/data_input/wq/smscg_wq_dup.csv")

#count duplicates in original file
#wq_final_dup_orig <- wq %>%
 # add_count(cdec_code,analyte_name,unit_name,time,value,qaqc_flag_id) %>%
  #filter(n>1) %>%
  #distinct() %>% 
  #arrange(cdec_code,analyte_name,time) %>% 
  #mutate(n = as.numeric(n)) %>% 
  #glimpse()
#33,523 which is same as more formatted version of file, 
#so these duplicates aren't due to something I messed up

#drop exact duplicates and work on dealing with rows that are identical except for QAQC code
wq_final_nodup <- wq_final %>% 
  #remove exact duplicates
  distinct()  %>% 
  #drop rows with values of NA
  filter(!is.na(value) ) %>%
  mutate(
    G = case_when(qaqc_flag_id == "G" ~ 1, TRUE ~ 0)
    ,Q = case_when(qaqc_flag_id == "Q" ~ 1, TRUE ~ 0)
    ,U = case_when(qaqc_flag_id == "U" ~ 1, TRUE ~ 0)
   ) %>% 
  select(-qaqc_flag_id) %>% 
  glimpse()


#combine analyte and unit
wq_final_cleaner <-wq_final_nodup %>% 
  #concatonate analytes and units
  unite(col = analyte_unit, c(analyte_name,unit_name), remove=T) %>% 
  #drop units from pH which is unitless
  mutate(analyte = case_when(analyte_unit =="pH_pH Units"~ "pH"
                             ,TRUE ~ analyte_unit)) %>% 
  #reorder columns and drop old analyte column
  select(cdec_code,year,date_time_pst,analyte,value,G,Q,U) %>% 
  #reorder rows
  arrange(cdec_code,date_time_pst,analyte) %>% 
  glimpse()

#summarize data by qaqc code
wq_final_summary <- wq_final_cleaner %>% 
  group_by(cdec_code,year, date_time_pst,analyte,value) %>% 
  summarize(G2 = sum(G)
            ,Q2 = sum(Q)
            ,U2 = sum(U)
            ,.groups = "drop")

#look at cases where there are either multiple values per code (shouldn't be any)
#wq_final_summary_mult <- wq_final_summary %>% 
 # filter(G2>1 | U2 > 1 | Q2> 1)
#none as expected

#look at cases where there are codes per value
#wq_final_summary_mult2 <- wq_final_summary %>% 
  #filter(
   # (G2 ==1 & U2 == 1)|
    #  (Q2 ==1 & U2 == 1)|
     # (G2 ==1 & Q2 == 1)
    #)
#G & U: 6866 cases
#G & Q: 0
#Q and U: 0

#write file with all cases of duplicates with G and U flags
#need to make date-time a character type first so it exports correctly
#wq_final_summary_mult2a <-wq_final_summary_mult2 %>% 
 # mutate(date_time_pst = as.character(date_time_pst)) %>% 
  #glimpse()
#write_csv(wq_final_summary_mult2a,"./EDI/data_input/wq/smscg_wq_dup_flag_detailed.csv")


#summarize flag duplicates by station, analyte
#wq_final_dup_flag_stn_sum <- wq_final_summary_mult2 %>% 
 # group_by(cdec_code,analyte) %>% 
  #count()
#write_csv(wq_final_dup_flag_stn_sum,"./EDI/data_input/wq/smscg_wq_dup_flag.csv")

#now deal with the cases with G and U codes for the same record
#should assign any record with a G as G (even if also U)
wq_final_summary_nodups <- wq_final_summary %>% 
  #add column with new code
  mutate(code = case_when(G2 == 1 ~ "G"
                          ,U2 == 1 ~ "U"
                          ,Q2 ==1 ~ "Q"))

#again look at cases where there are codes for both U and G
#code should be G, not U
#wq_final_summary_mult3 <- wq_final_summary_nodups %>% 
 # filter(G2 ==1 & U2 == 1 & code!="G")
#no cases of code = U as expected

wq_final_summary_nodups_atall <- wq_final_summary_nodups %>% 
  #drop the unneeded columns
  select(-c(G2,U2,Q2)) %>% 
  glimpse()

#look for duplicates 
#dups <- wq_final_summary_nodups_atall %>%
 # group_by(cdec_code, year, date_time_pst,analyte) %>%
  #summarise(n = n(), .groups = "drop") %>%
  #filter(n > 1L) %>% 
  #arrange(cdec_code,date_time_pst,analyte)
#329,852 duplicates

#summarize these weird dups by station and analyte
#dups_te <- dups %>% 
 # group_by(cdec_code,analyte) %>% 
  #count()
#write_csv(dups_te,"./EDI/data_input/wq/smscg_wq_dup_temp_ec.csv")

#unique(dups$analyte)
#"Specific Conductance_uS/cm" "Water Temperature_C"   
#unique(dups$cdec_code)
#"CSE" "MAL"
#range(dups$date_time_pst)

#look at example of duplicates
#use semi_join
#dups_ec_temp <- semi_join(wq_final_summary_nodups_atall,dups)

#dup_ex <- wq_final_summary_nodups_atall %>% 
 # filter(cdec_code=="MAL" & date_time_pst=="2020-08-21 10:15:00")

#wq_wide_test <-wq_final_cleaner %>% 
  #convert long to wide
 # pivot_wider(id_cols = c(cdec_code,year,date_time_pst,analyte)
  #            ,names_from = analyte
   #           ,values_from = c(value, qaqc_flag_id)
    #          ,values_fill = NA
  #) %>% 
  #glimpse()
#says there are duplicates

#look at example of duplicates
#dup_ex <- wq_final_cleaner %>% 
 # filter(cdec_code=="CSE" & date_time_pst=="2018-05-17 08:15:00" & analyte == "Water Temperature_C")
  
#look at all combos of parameter and units again
#par_unit2 <- wq_final %>% 
 #   distinct(analyte_name,unit_name) %>% 
  #  arrange(analyte_name)
#looks fine now

#check that the date/time again after setting time zone
#range(wq_final$date_time_pst)
#"2018-01-01 00:00:00 -08" "2022-12-31 23:45:00 -08"
#looks good

#look at summary of data points by station, parameter
#wq_summary_sp <- wq_final %>% 
 # group_by(cdec_code,analyte_name) %>% 
  #count() %>% 
  #ungroup() %>% 
  #glimpse()

#make df with all combos of station and parameter
#NOTE: had to make sure to ungroup above to get this to work
#stn_par_combo <- wq_summary_sp %>% 
 # select(cdec_code,analyte_name) %>% 
  #expand(cdec_code,analyte_name)
#15 stations x 6 parameters = 90 so looks right

#determine which parameters are missing from stations 
#stn_par_missing <- anti_join(stn_par_combo,wq_summary_sp)
#came up with 10 missing station-parameter combos as expected
#based on CDED metadata, none of these pH and chlorophyll parameters should be missing

#quick check to see if these are missing from original df
#hon_ph <- wq %>% 
 # filter(cdec_code=="HON" & analyte_name=="pH")
#it is true that this combo is missing from the data set

#look at summary of data points by station, parameter, and year
#wq_summary_spy <- wq_final %>% 
 # group_by(cdec_code,analyte_name,year) %>% 
  #count() %>% 
  #ungroup() %>% 
  #glimpse()
#seems like some parameters are missing

#quick plot of data points
#NOTE: this isn't a very good way to plot the data because of overlap of points/lines
#(plot_wq <- ggplot(wq_summary_spy,aes(x = year, y = n, group = analyte_name, color=analyte_name)) +
 # geom_line()+
  #geom_point()+
  #facet_wrap(~cdec_code)
#)

#remove bad data for CSE and MAL------------------
#EC and temp
#will be replaced with data from new files

#filter out old MAL data
wq_mal_filt <- wq_final_summary_nodups_atall %>% 
  #drop the old MAL data
  filter(!(cdec_code=="MAL" & (analyte=="Water Temperature_C" |analyte=="Specific Conductance_uS/cm")))

#look at date range for each station
#date_range <- wq_mal_filt %>% 
 # group_by(cdec_code) %>% 
  #summarise(date_min = min(date_time_pst)
   #         ,date_max = max(date_time_pst)
    #        ,.groups='drop')
#CSE covers full date range from beginning 2018 to end 2022

#filter out old CSE data
wq_cse_filt <- wq_mal_filt %>% 
  #drop the old CSE data
  filter(!(cdec_code=="CSE" & (analyte=="Water Temperature_C" |analyte=="Specific Conductance_uS/cm")
          #& (date_time_pst>="2018-05-15 10:45:00" & date_time_pst<="2018-06-12 23:45:00"))) %>% 
          #needed to shift time range an hour later to get correct date-time range; not sure why though
          & (date_time_pst>="2018-05-15 11:45:00" & date_time_pst<="2018-06-13 00:45:00"))) %>% 
  arrange(date_time_pst) %>% 
  glimpse()

#look at date range for each station
#date_range <- wq_mal_filt %>% 
# group_by(cdec_code) %>% 
#summarise(date_min = min(date_time_pst)
#         ,date_max = max(date_time_pst)
#        ,.groups='drop')
#CSE covers full date range from beginning 2018 to end 2022

#format new data for MAL and CSE-------------

data_new <- bind_rows(cse_ec,cse_temp,mal_sc,mal_temp) %>% 
  clean_names() %>% 
  #add some columns
  mutate(cdec_code = case_when(station_id == 22~ "CSE",station_id==60~"MAL")
         ,analyte = case_when(grepl("Conductance | SpC", constituent) ~ "Specific Conductance_uS/cm"
                              ,grepl("Temperature", constituent) ~ "Water Temperature_C")
         #format date-time
         ,date_time = mdy_hm(datetime)
         ,date_time_pst = force_tz(date_time,tzone="Etc/GMT+8")
         ,year = year(date_time)
         ) %>% 
  #rename and reorder some columns
  select(
    cdec_code
    ,year
    ,date_time_pst
    ,analyte
    ,value
    ,code = qaqc_flag
  ) %>% 
  #looks like data with all possible QAQC codes are still included so filter these
  filter(code =="G" | code=="U") %>% 
  arrange(date_time_pst,cdec_code) %>% 
  glimpse()

# tz(data_new$date_time_pst)

#check resulting data frame
#new_data_summary <- data_new %>% 
 # group_by(cdec_code, year,analyte,code) %>% 
  #count()

#add new data back to the main data set------------
#also converting long to wide

#should just need to bind the rows of old and new data
data_updated <- bind_rows(wq_cse_filt,data_new) %>% 
  glimpse()

#make quick panel of plots for data to see if it generally looks OK
#note this takes a very long time to run
#(plot_wq_clean <-ggplot(data_updated,aes(x = date_time_pst, y = value, group=cdec_code,color=cdec_code)) +
 #   geom_line()+
  #  geom_point()+
   # facet_wrap(~analyte,scales="free_y")
#)
#still outliers for at least some stations for most parameters

#converted long to wide
#had to tweak the code to get the column order I wanted
#https://github.com/tidyverse/tidyr/issues/1064
spec <- build_wider_spec(
  data_updated,
  names_from = analyte,
  values_from = c(value,code),
  names_glue = "{analyte}_{.value}"
)

spec <- arrange(spec, analyte, .value)

wq_wide <- pivot_wider_spec(data_updated, spec) %>%
  arrange(date_time_pst) %>%
  glimpse()

# wq_wide <-data_updated %>%
# #convert long to wide
# pivot_wider(id_cols = c(cdec_code,year,date_time_pst)
#          ,names_from = analyte
#        ,values_from = c(value,code)
#      ,values_fill = NA
#     ,names_sort = T
#   ) %>%
# arrange(date_time_pst) %>%
# glimpse()

#print names of columns
#names(wq_wide)

#look at date range for each station
#date_range_w <- wq_wide %>% 
 # group_by(cdec_code) %>% 
  #summarise(date_min = min(date_time_pst)
   #         ,date_max = max(date_time_pst)
    #        ,.groups='drop')

#add RVB data to main data set-----------------

rvb_format <- rvb %>% 
  #create date-time column
  mutate(date_time = ymd_hms(paste(date, time))
         ,date_time_pst = force_tz(date_time,tzone="Etc/GMT+8")
         ,year = year(date_time_pst)
         ) %>% 
  #filter to just the needed time period
  filter(date_time_pst>="2018-01-01 00:00:00")  %>% 
  #filter to the date range I want
  select(station 
         ,year
         ,date_time_pst
         ,'fluorescence_ug/L_value' = fluorescence          
         ,'dissolved_oxygen_mg/L_value' = dissolvedoxygen
         ,'specific_conductance_uS/cm_value' = spc
         ,turbidity_FNU_value = turbidity          
         ,temperature_C_value = watertemperature  
         ,pH_value = ph
  ) %>% 
  glimpse()

# range(rvb_format$date_time)
# range(rvb_format$date_time_pst)


#write final file for publishing on EDI--------------------------
#convert date-time to character
wq_wide_ft <-wq_wide%>% 
  #format column names
  select(station = cdec_code
         ,year
         ,date_time_pst
         ,'fluorescence_ug/L_code' = 'Chlorophyll_ug/L_code'           
         ,'fluorescence_ug/L_value' = 'Chlorophyll_ug/L_value'           
         ,'dissolved_oxygen_mg/L_code' = 'Dissolved Oxygen_mg/L_code'
         ,'dissolved_oxygen_mg/L_value' = 'Dissolved Oxygen_mg/L_value'
         ,'specific_conductance_uS/cm_code' = 'Specific Conductance_uS/cm_code'
         ,'specific_conductance_uS/cm_value' = 'Specific Conductance_uS/cm_value'
         ,turbidity_FNU_code = Turbidity_FNU_code
         ,turbidity_FNU_value = Turbidity_FNU_value              
         ,turbidity_NTU_code = Turbidity_NTU_code
         ,turbidity_NTU_value = Turbidity_NTU_value
         ,temperature_C_code = 'Water Temperature_C_code'         
         ,temperature_C_value = 'Water Temperature_C_value'  
         ,pH_code                         
         ,pH_value
         ) %>% 
  glimpse()

#combine RVB with rest of data
wq_wide_all <- bind_rows(wq_wide_ft,rvb_format) %>% 
  #write_csv() will convert date-time back to UTC, which is 8 h off, so need to make date-time character for export
  #but all the 00:00:00 times get dropped from dates when converting to character
  #mutate(date_time_pst = as.character(date_time_pst))  %>% 
  #need to use format() to prevent 00:00:00 time loss; format will convert date-time to character
  mutate(date_time_pst = format(date_time_pst, "%Y-%m-%d %H:%M:%S")) %>% 
  arrange(date_time_pst,station) %>% 
  glimpse()

#range(wq_wide_all$date_time_pst) # "2018-01-01 00:00:00" "2022-12-31 23:45:00"

#create tiny subset of the final data set to make sure everything is exporting correctly, especially date-time
#use write_excel_csv() because write_csv() will mess up date-time by converting to UTC 
wq_wide_all_head <-head(wq_wide_all)
wq_wide_all_tail <-tail(wq_wide_all)
wq_wide_all_ht <- bind_rows(wq_wide_all_head,wq_wide_all_tail)
#write_excel_csv(wq_wide_all_ht,file = paste0(sharepoint_path_output,"./smscg_data_water_quality_toy.csv"))

#export whole final data set
#write_excel_csv(wq_wide_all,file = paste0(sharepoint_path_output,"./smscg_data_water_quality_revised.csv"))

#just to be safe, now read data back in and see if it still looks OK
#test <- read_csv(paste0(sharepoint_path_output,"./smscg_data_water_quality_tz.csv")) %>% 
#  glimpse()
#range(test$date_time_pst) #looks fine



