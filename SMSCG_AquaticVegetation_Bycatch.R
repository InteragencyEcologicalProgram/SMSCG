#Suisun Marsh Salinity Control Gate Action
#UC-Davis Suisun Marsh Fish Survey
#Otter trawl
#Submerged aquatic vegetation data


#important metadata
#vegetation data collection started 2014

#To do list
#ask for details on how percent area is calculated for Habitat data set of hook and line survey
#add the "non-detect" otter trawl samples
#incorporate survey effort (trawl duration)
#try using UCD trawl WQ data (SpCond, secchi, depth)
#refer to following database tabs: Sample, Depth, TrawlEffort


#required packages
library(tidyverse)
library(janitor) #function for data frame clean up
library(EDIutils) #download EDI data
library(sf) #working with spatial data

#additional data sets needed for analysis----------
#survey effort: need to know SAV volume per unit survey effort (though maybe this is standardized)
#water quality measurements: salinity is key but turbidity would be nice too

# Read in the Data----------------------------------------------
#EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1947.1

#use EDIutils package to read in all file names and download the ones you want to use
#https://docs.ropensci.org/EDIutils/index.html

#list all data files from EMP benthic inverts EDI package
ucd_marsh_fish_pkg <- read_data_entity_names(packageId = "edi.1947.1")

#catch data which includes vegetation bycatch data
catch <- read_csv(read_data_entity(packageId = "edi.1947.1", entityId= ucd_marsh_fish_pkg$entityId[2])) %>% 
  clean_names() %>% 
  glimpse()

#info about samples (eg, date, station, etc)
sample <- read_csv(read_data_entity(packageId = "edi.1947.1", entityId= ucd_marsh_fish_pkg$entityId[14])) %>% 
  clean_names() %>% 
  glimpse()

#info about stations
station <- read_csv(read_data_entity(packageId = "edi.1947.1", entityId= ucd_marsh_fish_pkg$entityId[17])) %>% 
  clean_names() %>% 
  glimpse()

#info about sampling method
method <- read_csv(read_data_entity(packageId = "edi.1947.1", entityId= ucd_marsh_fish_pkg$entityId[10])) %>% 
  clean_names() %>% 
  glimpse()

#info about species codes
spp <- read_csv(read_data_entity(packageId = "edi.1947.1", entityId= ucd_marsh_fish_pkg$entityId[11])) %>% 
  clean_names() %>% 
  glimpse()

#trawl effort
teff <- read_csv(read_data_entity(packageId = "edi.1947.1", entityId= ucd_marsh_fish_pkg$entityId[18])) %>% 
  clean_names() %>% 
  glimpse()

#seine effort
seff <- read_csv(read_data_entity(packageId = "edi.1947.1", entityId= ucd_marsh_fish_pkg$entityId[15])) %>% 
  clean_names() %>% 
  glimpse()

# Simplify and combine data sets--------------------

#reduce data set on catch to just columns relevant to veg
catch_ft <- catch %>% 
  select(
    sample_row_id
    ,organism_code
    #,catch_comments #a quick review of comments showed these are just about the fish, not the veg which makes sense
    ,volume
    ) %>% 
  glimpse()
#unique(catch_ft$catch_comments)

#format sample data set
sample_ft <- sample %>% 
  mutate(sample_date = mdy_hm(sample_date)) %>% 
  glimpse()

#format station data set
#note that not all stations include GPS coordinates
#presumably the long term monitoring stations do have these coordinates
station_ft <- station %>% 
    select(
    station_code
    ,active
    ,location
    ,x_wgs84
    ,y_wgs84
  ) %>% 
  glimpse()

#format trawl effort

#first are there multiple trawls per sample?
# trawl_sample <- teff %>% 
#   distinct(trawl_row_id, sample_row_id)
#no, never more than one trawl per sample

#tow duration is in minutes
teff_ft <- teff %>% 
  select(
    sample_row_id
    ,tow_duration
  ) %>% 
  glimpse()

#make list of FAV taxa
fav_codes = c("AZ","EICR","LUPE")

#make a list of Other taxa

#format spp data
spp_ft <- spp %>% 
  select(
    organism_code
    ,native
    ,common_name
    ,phylum
    ,genus
    ,species
  ) %>% 
  filter(phylum == "Tracheophytes" | organism_code == "SAVSPP") %>% 
  filter(organism_code != "PICK" & organism_code != "SEPU" ) %>% 
  #add veg type
  mutate(type = case_when(organism_code %in% fav_codes ~ "fav",
                          TRUE ~ "sav")) %>% 
  arrange(type,organism_code) %>% 
  select(organism_code
         ,native
         ,genus
         ,species
         ,type) %>% 
  glimpse()
#SAVSPP includes veg and algae
#pickleweed (PICK) and sea purlane (SEPU) are not aquatic so drop them

#filter catch data to just include veg 
catch_veg <- catch_ft %>% 
  filter(organism_code %in% veg_codes)

#combine catch data and sample data
sample_catch <- sample_ft %>% 
  left_join(catch_veg) %>% 
  #filter out data prior to 2014 because veg data wasn't recorded before then
  #the first record of veg in data is April 17, so let's use that as our start
  #even though I don't know the exact date in 2014 that the data collection started
  filter(sample_date > "2014-04-16") %>% 
  #add station metadata to df
  left_join(station_ft) %>% 
  #specify the crs as wgs84
  #couldn't implement this because of stations missing GPS info
  # st_as_sf(coords = c(x='x_wgs84',y='y_wgs84'), #identify the lat/long columns
  #          crs = 4326 #EPSG code for WGS84
  #          ,remove=F #retains original lat/long columns
  # ) %>% 
  select(
    sample_row_id
    ,date = sample_date
    ,station = station_code
    ,method = method_code
    ,organism_code
    ,volume
    ,x_wgs84
    ,y_wgs84
    ,location
    ,water_temperature:tide_code
    ,elec_cond
    ,qa_done
  ) %>% 
  arrange(method, date, station) %>% 
  glimpse()
#can't convert GPS to geometry because of stations with missing coordinates

#are there cases where catch rows didn't match with sample data?
# sample_catch_mismatch <- sample_catch %>% 
#   filter(is.na(sample_row_id))
#no instances of this

#how many cases of missing GPS coordinates
# station_ft_no_gps <- sample_catch %>% 
#   distinct(station, method, location, x_wgs84, y_wgs84) %>% 
#   filter(is.na(x_wgs84) | is.na(y_wgs84))
#64 stations with no GPS data
#distributed across sample method types but mostly seines and hook and line
#could guess at locations based on location descriptions

#Do hook and line surveys ever record veg catch? 
#I think for these surveys, veg data wouldn't be in catch. they would be in the habitat table
# sample_catch_hkln <- sample_catch %>% 
#   filter(method_code == "HKLN" 
#          & volume != 0
#          )
#as expected, no veg catch for this survey method
#pull this survey method from rest of data
#if using veg data from this survey method, need to reference habitat data set
#need to see how often aquatic veg is recorded in this survey

#count number of samples per station and show whether they have GPS coordinates
#hopefully, the ones without coordinates weren't a significant part of the samples
sample_catch_stn_ct <- sample_catch %>% 
  #reduce rows to just the unique samples
  distinct(date,station, method, x_wgs84, y_wgs84,location) %>% 
  #count samples by station
  group_by(station,method, x_wgs84,y_wgs84,location) %>% 
  summarise(n = n(),.groups = "drop") %>% 
  arrange(method, -n)
#those with coordinates are not necessarily the ones with the most samples

#sum samples with vs without GPS coordinates
sample_catch_stn_ct_sum <- sample_catch_stn_ct %>% 
  #add column that indicates whether coordinates or not
  mutate(gps = case_when(is.na(x_wgs84) | is.na(y_wgs84) ~ 0
                         ,TRUE ~ 1)) %>% 
  group_by(gps) %>% 
  summarise(sample_count = sum(n))
#2/3 of samples have GPS coordinates

#Look at otter trawls
sample_catch_otr <- sample_catch %>% 
  #for now drop, samples without GPS coordinates
  filter(method == "OTR" & !(is.na(x_wgs84) | is.na(y_wgs84))) %>% 
  #specify the crs as wgs84
  st_as_sf(coords = c(x='x_wgs84',y='y_wgs84'), #identify the lat/long columns
           crs = 4326 #EPSG code for WGS84
           ,remove=T #drop original lat/long columns
  ) %>%
  #add species info
  left_join(spp_ft) %>% 
  #add year column 
  mutate(year = year(date),.after = date) %>% 
  glimpse()

#how many trawl stations are left?
#these are all otter trawl samples since April 2014 with GPS coordinates
otr_stn_ct <- unique(sample_catch_otr$station)
#31 stations 

#what is date range?
#range(sample_catch_otr$date)
#"2014-04-17 UTC" "2025-02-14 UTC"

#columns to group by


#sum sav and fav data by sample and calculate ml per min
sum_veg_otr <- sample_catch_otr %>% 
  group_by(sample_row_id,date,year,station,location, geometry,type) %>% 
  summarise(tot_volume = sum(volume),.groups = 'drop') %>% 
  #add zeros where no veg
  mutate(tot_volume = case_when(is.na(tot_volume)~0,
                                 TRUE ~ tot_volume)) %>% 
  #add effort data
  left_join(teff_ft) %>% 
  #volume (ml) per trawl effort (min)
  mutate(ml_per_min = tot_volume/tow_duration) %>% 
  glimpse()


#median sav and fav ml per min by station and year
median_veg_otr <- sum_veg_otr %>% 
  group_by(year,station,location, geometry, type) %>% 
  summarize(median_ml_per_min = median(ml_per_min)) %>% 
  glimpse()

#to do
#look at similar maps I made for drought barrier and plankton sampling
#make SAV map for each year


  #calculate mL of veg per minute of trawl effort
  # mutate(
  #   volume_ml = case_when(is.na(volume)~0, TRUE ~ volume)
  #   ,veg_ml_min = volume_ml/tow_duration)
  
#start with map for each year showing SAV abundance per sample effort at each station using point size
#that would be 2015-2024
#stacked bar plot showing each plant taxon by station with years as facet
#could lump plant taxa by life form (SAV, FAV) and/or station by region
#could also look at seasonal abundances and/or just look at summer/fall months 
#plot total veg abundance relative to SMSCGs






