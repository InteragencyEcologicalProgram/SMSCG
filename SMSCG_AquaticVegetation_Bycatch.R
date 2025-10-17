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
library(deltamapr) #delta base maps
library(ggrepel) #nonoverlapping point labels
library(plotrix) #standard error

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

#create data frame that categorizes station by region
rgwq <- data.frame(
  station = c(
    "MZ1", "MZ2", "MZN3","MZ6"
    ,"NS1","NS2","NS3", "DV1","DV2", "DV3"
    ,"SUVOL","CO1", "CO2","SU1","SU2", "GR3","SB1","SB2","SB0_5","BY1","BY2", "BY3", "PT1","PT2","PT3"
    , "SU4", "SU3","GY1", "GY2","GY3"),
  region = c(rep("SE",4), rep("NE",6), rep("NW",15),rep("SW",5))
)

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

#create a list of the organism codes for veg
#will be used to filter catch data
veg_codes <- spp_ft %>% 
pull(organism_code)

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

#look at beach seine data
sample_catch_bsein <- sample_catch %>%
  filter(method == "BSEIN"
         & volume != 0
         )

bsein_stations <- sample_catch_bsein %>% 
  distinct(station,location) %>% 
  arrange(station)
#14 stations with veg catch in at least some samples
#veg starts showing up (or getting quantified) in 2021
#no GPS coordinates
#less sure about standardizing by effort for these

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
  #add month and year columns 
  mutate(month = month(date)
    ,year = year(date),.after = date) %>% 
  glimpse()

#how many trawl stations are left?
#these are all otter trawl samples since April 2014 with GPS coordinates
#otr_stn_ct <- unique(sample_catch_otr$station)
#31 stations 

#what is date range?
#range(sample_catch_otr$date)
#"2014-04-17 UTC" "2025-02-14 UTC"

#summer fall months to target
sfmonths <- c(7,8,9,10)

#sum sav volume data by sample and calculate ml per min
#no fav samples in dataset
sum_veg_otr1 <- sample_catch_otr %>% 
  #focus just on summer-fall period (July-Oct)
  filter(month %in% sfmonths) %>% 
  #add zeros where no veg
  mutate(volume = case_when(is.na(volume)~0,
                                TRUE ~ volume)) %>% 
  #sum all species volumes into total sav volume per sample
  group_by(sample_row_id,date,year,month,station,location, geometry) %>% 
  summarise(samp_volume = sum(volume),.groups = 'drop') %>% 
  #add effort data
  #NOTE: only one NA for tow duration and it is for a sample with no veg volume so doesn't matter
  left_join(teff_ft) %>% 
  #add region categories
  left_join(rgwq) %>% 
  glimpse()

#how many samples per stations with vs without veg?
station_sample_summary <- sum_veg_otr1 %>% 
  st_drop_geometry() %>% 
  mutate(pa = case_when(samp_volume > 0 ~ 1,
                        TRUE ~ 0)) %>% 
  group_by(region,station,pa) %>% 
  summarise(n = n()) %>% 
  arrange(region,station,pa) %>% 
  pivot_wider(names_from = 'pa', values_from = n) %>%
  select(region
         ,station
         ,absent = "0"
         ,present = "1") %>% 
  mutate(present = case_when(is.na(present) ~ 0,
                        TRUE ~ present)
         ,prop = present/(absent+present)
         ) %>% 
  glimpse()


sum_veg_otr <- sum_veg_otr1 %>% 
  #let's just sum all veg volume across all summer fall months
  group_by(year,station,region,location, geometry) %>% 
  summarise(tot_volume = sum(samp_volume)
            ,tot_duration = sum(tow_duration)
            ,.groups = 'drop') %>% 
  mutate(  
    #volume (ml) per trawl effort (min)
    ml_per_min = tot_volume/tot_duration
    #split out longitude and latitude from geometry
         ,longitude = st_coordinates(.)[,1],
         latitude= st_coordinates(.)[,2]) %>% 
  arrange(year,region,station) %>% 
  glimpse()



#confirm it only includes summer fall months
#unique(sum_veg_otr$month)
# 7  8  9 10

#no 2025 map because data only for Jan-Feb on EDI as of 8/26/25

#split data set into one year increments for mapping
sav24 <- sum_veg_otr %>% filter(year == 2024)
sav23 <- sum_veg_otr %>% filter(year == 2023)
sav22 <- sum_veg_otr %>% filter(year == 2022)
sav21 <- sum_veg_otr %>% filter(year == 2021)
sav20 <- sum_veg_otr %>% filter(year == 2020)
sav19 <- sum_veg_otr %>% filter(year == 2019)
sav18 <- sum_veg_otr %>% filter(year == 2018)
sav17 <- sum_veg_otr %>% filter(year == 2017)
sav16 <- sum_veg_otr %>% filter(year == 2016)
sav15 <- sum_veg_otr %>% filter(year == 2015)
sav14 <- sum_veg_otr %>% filter(year == 2014)

#make SAV map for each year

#match CRS of base map and stations
st_crs(WW_Delta) #NAD83 which is EPSG = 4269
st_crs(sum_veg_otr$geometry) #4326

ww_delta_4326 <- st_transform(WW_Delta, crs = 4326)

#make 2024 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav24, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2024 SAV (July-Oct)")

#make 2023 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav23, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2023 SAV (July-Oct)")
#NOTE: apparently one value missing, look into this

#make 2022 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav22, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2022 SAV (July-Oct)")

#make 2021 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav21, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2021 SAV (July-Oct)")

#make 2020 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav20, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2020 SAV (July-Oct)")

#make 2019 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav19, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2019 SAV (July-Oct)")

#make 2018 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav18, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2018 SAV (July-Oct)")

#make 2017 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav17, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2017 SAV (July-Oct)")

#make 2016 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav16, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2016 SAV (July-Oct)")

#make 2015 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav15, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2015 SAV (July-Oct)")

#make 2014 map
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sav14, color= "red",  aes(size= ml_per_min))+ 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2014 SAV (July-Oct)")


#make one map that shows SAV volume/min of trawling across all years

sum_veg_otr_allyears <- sum_veg_otr %>% 
  group_by(station,region,location,geometry,latitude,longitude) %>% 
  summarise(all_volume = sum(tot_volume)
            ,all_duration = sum(tot_duration,na.rm = T),.groups = 'drop') %>% 
  mutate(all_ml_per_min = all_volume/all_duration) %>% 
  arrange(-all_duration)
#note: station SU2 has at least one NA for duration but it doesn't matter because no veg there ever

#drop stations with low effort
sum_veg_otr_allyears_higheff <- sum_veg_otr_allyears %>% 
  #drop some stations that have less than 50 minutes of samples across the whole time period
  #drops six stations, nearly all in NW region
  filter(all_duration > 40) %>% 
  arrange(-all_ml_per_min)

#create list of stations with low effort
stn_low_effort <- sum_veg_otr_allyears %>% 
  filter(all_duration <= 40) %>% 
  pull(station)
#"MZN3"  "SUVOL" "SB0_5" "BY2"   "GR3"   "PT3"  

#map showing all years
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "skyblue3", color= "black") +
  #plot stations with point size set to veg volume per effort
  geom_sf(data= sum_veg_otr_allyears_higheff, color= "red",  aes(size= all_ml_per_min))+ 
  #add station names as labels and make sure they don't overlap each other or the points
  geom_label_repel(data = sum_veg_otr_allyears_higheff, aes(x=longitude,y=latitude, label=station) #label the points
                   #,nudge_x = -0.008, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                   , size = 3 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  #crop area to just marsh
  coord_sf( 
    xlim =c(-122.15, -121.85)
    ,ylim = c(38.025,38.25)
  )+
  ggtitle( label = "2014-2024 (July - Oct)")

#time series of SAV by region with error bars
sum_veg_otr_rgyr <- sum_veg_otr %>% 
  #drop geometry colum
  st_drop_geometry() %>% 
  #drop low effort stations
  filter(!(station %in% stn_low_effort)) %>% 
  group_by(region,year) %>%  
  summarize(mean_ml_per_min = mean(ml_per_min,na.rm=T)
            ,se_ml_per_min = std.error(ml_per_min,na.rm = T),.groups='drop') 
#NOTE: need to decide what to do about missing value for SU2
#for now just drop this NA from dataset for plotting

#plot data
(plot_ts_rgyr <- ggplot(data = sum_veg_otr_rgyr, aes(x = year, y = mean_ml_per_min,color=region)) +
    geom_point()+
    geom_line())+
  geom_errorbar(aes(ymin = mean_ml_per_min-se_ml_per_min, ymax = mean_ml_per_min+se_ml_per_min), width = 0.2)

#high veg volume stations 
high_veg <- c("MZ1", "MZ2", "MZ6")

#subset data to just the SE stations
sum_veg_otr_allyears_se <-sum_veg_otr1 %>% 
  filter(station %in% high_veg) %>% 
  mutate(ml_per_min = samp_volume/tow_duration,.after=tow_duration)
#130 data points (3 stations x 4 months x ~10 years)

#time series of SAV for SE region with error bars
sum_veg_otr_yr_se <- sum_veg_otr_allyears_se %>% 
  #drop geometry colum
  st_drop_geometry() %>% 
  group_by(year) %>%  
  summarize(mean_ml_per_min = mean(ml_per_min,na.rm=T)
            ,se_ml_per_min = std.error(ml_per_min,na.rm = T),.groups='drop') 


#strings for connecting letters in plot below
years <- c(2014:2024)
ml_per_min <- c(rep(50,6),150,50,50,450,300)
connect_letters <- c(rep("A",6),"A,B","A","A","B","A,B")

#plot data
(plot_ts_yr_se <- ggplot(data = sum_veg_otr_yr_se, aes(x = year, y = mean_ml_per_min)) +
    geom_point()+
    geom_line())+
  geom_errorbar(aes(ymin = mean_ml_per_min-se_ml_per_min, ymax = mean_ml_per_min+se_ml_per_min), width = 0.2)+
  annotate("text", x=years, y=ml_per_min, label= connect_letters)

#do overall test and make sure assumptions of normality, etc apply

#multiple comparisons showing which years differ
pairwise_results <- pairwise.t.test(sum_veg_otr_allyears_se$ml_per_min, sum_veg_otr_allyears_se$year, p.adjust.method = "fdr")










