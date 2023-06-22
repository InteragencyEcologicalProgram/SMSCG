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

glimpse(wq)
#everything looks good

#integrated discrete WQ data set
#iwq <-read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.7&entityid=6c5f35b1d316e39c8de0bfadfb3c9692")

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
#"\xb0C"        "\xb5S/cm"     "mg/L"         "\xb5g/L"      "NTU"          "pH Units"     "% saturation" "RFU"          "FNU"     
#a few of these need work; degree and micro symbols didn't translate

#look at all combos of parameter and units
par_unit <- wq %>% 
  distinct(analyte_name,unit_name) %>% 
  arrange(analyte_name)
#chlorophyll: two types of units (RFU, ug/L)
#turbidity: two types of units (NTU, FNU); I think they're considered pretty interchangeable
#DO: two types of units (mg/L, % saturation)
#for these, do we have two sets of values for each date, a change through time in units, or a mix of the two?

#look at all combos of station, parameter, and units
par_unit_stn <- wq %>% 
  distinct(cdec_code, analyte_name,unit_name) %>% 
  arrange(analyte_name,cdec_code)

par_unit_stn_odd <- par_unit_stn %>% 
  group_by(cdec_code,analyte_name) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n>1)
#5 stations have more than one unit type for at least one analyte

stn_odd <- par_unit_stn_odd %>%
  #select(cdec_code) %>% 
  distinct(cdec_code) %>% 
  pull(cdec_code)

#look at how many data points per station,analyte,unit
par_unit_stn_sum <- wq %>% 
  filter(cdec_code %in% stn_odd) %>% 
  group_by(cdec_code,analyte_name,unit_name) %>% 
  count() %>% 
  arrange(cdec_code,analyte_name)
#Except for GZL, can ignore the RFU and % saturation units because only one each 
#need to look closer at FNU vs NTU for turbidity; probably just changed at some point
#look at date range for each unit type by station

#turbidity units
turb_odd <- wq %>% 
  filter(
    #cdec_code %in% stn_odd & 
    analyte_name=="Turbidity") %>% 
  group_by(cdec_code,unit_name) %>% 
  summarise(min = min(time)
            ,max = max(time)) %>% 
  arrange(cdec_code,unit_name)
#so looks like probably NTU was old unit
#then some stations switched to FNU with a period of overlap for unit comparison
#some stations have stayed on NTU so only one unit type
#let's just keep both types of data in the data set

#now look closer at GZL chlorophyll
chlor_odd <- wq %>% 
  filter(cdec_code == "GZL" & analyte_name == "Chlorophyll")

chlor_odd_summary <- chlor_odd %>% 
  group_by(unit_name) %>% 
  summarise(min = min(time)
            ,max = max(time)) 

#quick plot of GZL Chlorophyll by unit
(p_gzl_chlor <- ggplot(chlor_odd, aes(x = time,y=value,color=unit_name)) + 
  geom_line()
)
#so always ug/L but in recent years also RFU
#overall, just keep ug/L for all stations and drop RFU

#now look at DO for GZL too
#now look closer at GZL chlorophyll
do_odd <- wq %>% 
  filter(cdec_code == "GZL" & analyte_name == "Dissolved Oxygen")

do_odd_summary <- do_odd %>% 
  group_by(unit_name) %>% 
  summarise(min = min(time)
            ,max = max(time)) 

#quick plot of GZL Chlorophyll by unit
(p_gzl_do <- ggplot(do_odd, aes(x = time,y=value,color=unit_name)) + 
    geom_line()
)
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
    ,date_time_pst = force_tz(time,tzone="Etc/GMT+8")
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

#convert long to wide
wq_final_cleaner <-wq_final %>% 
  #concatonate analytes and units
  unite(col = analyte_unit, c(analyte_name,unit_name), remove=T) %>% 
  #drop units from pH which is unitless
  mutate(analyte = case_when(analyte_unit =="pH_pH Units"~ "pH"
                             ,TRUE ~ analyte_unit)) %>% 
  #reorder columns and drop old analyte column
  select(cdec_code,year,date_time_pst,analyte,value,qaqc_flag_id) %>% 
  glimpse()

wq_wide <-wq_final_cleaner %>% 
  #convert wide to long
  pivot_wider(id_cols = c(cdec_code,year,date_time_pst),names_from = analyte,values_from = c(value,qaqc_flag_id))

dups <- wq_wide %>%
  dplyr::group_by(cdec_code, year, date_time_pst) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

unique(wq_final_wide$analyte)
  
#confirm time zone
tz(wq_final$date_time_pst)
#works fine

#look at all combos of parameter and units again
par_unit2 <- wq_final %>% 
    distinct(analyte_name,unit_name) %>% 
    arrange(analyte_name)
#looks fine now

#check that the date/time again after setting time zone
range(wq_final$date_time_pst)
#"2018-01-01 00:00:00 -08" "2022-12-31 23:45:00 -08"
#looks good

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
