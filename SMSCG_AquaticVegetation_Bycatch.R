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

#read in water quality data
#wq_data<-read_csv(file = paste0(sharepoint_path,"./WaterQuality/AquaticVegetation_WQ_SummarizedData.csv"))

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
#NOTE: need to convert GPS coordinates to correct format

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
  glimpse()
#SAVSPP includes veg and algae
#I don't think pickleweed (PICK) or sea purlane (SEPU) are aquatic so we will see how much they come up in the data sets

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
    date = sample_date
    ,station = station_code
    ,method = method_code
    ,organism = organism_code
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
sample_catch_hkln <- sample_catch %>% 
  filter(method_code == "HKLN" 
         #& volume != 0
         )
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

#create subset of catch data without hook and line surveys because veg data recorded differently for that survey
sample_catch_nets <- sample_catch %>% 
  filter(method_code != "HKLN")

#look at unique survey methods remaining
#unique(sample_catch_nets$method_code)
#"BSEIN" "OTR"  
#I guess the midwater trawl and larval sled data were dropped somewhere along the way
#perhaps they were only done in years before 2014




#might just need to create a file that has the stations categorized into region if we want to look by region

#probably need to incorporate survey effort into this because I doubt it's the same through time
#take total veg volume and divide by survey effort
#not perfect because some regions are more prone to veg invasion than others



#examine SAV data columns-------------

#generate list of the column names
names(sav_cleaner)

#how many stations in data set?
unique(sav_cleaner$station_code)
#n=16

#range of dates?
range(sav_cleaner$date)
#2014-04-17" "2020-12-23"

#which SAV species?
unique(sav_cleaner$organism_code)
#Nine categories
#"ELCA" = Elodea canadensis = American waterweed   
#"EGDE" = Egeria densa = Brazilian waterweed   
#"CEDE" = Ceratophyllum demersum = coontail   
#"MYSP" = Myriophyllum spicatum = Eurasian watermilfoil   
#"ALG" = filamentous green algae (not SAV)
#"POCR" = Potamogeton crispa = curlyleaf pondweed   
#"STSPP" = Stuckenia spp = Sago pondweed  (salt tolerant native)
#"SAVSPP" = unidentified submerged aquatic plant species
#"CACA" = Cabomba caroliniana = Carolina Fanwort    

#distribution of SAV volume
range(sav_cleaner$volume) #1 - 102206
hist(sav_cleaner$volume)
#vast majority are small but at least one or two that are very large

#create data frame that categorizes station by region
#and identifies the closest water quality station
#E = eastern marsh, C = central marsh, W = western marsh
rgwq <- data.frame(
  station_code = c("MZ1", "MZ2", "MZ6", "NS3", "NS2", "NS1", "DV2", "DV1","MZN3","CO1", "SU2", "SB1", "BY3","SD2", "SU4", "SU3"),
  region = c(rep("E",9), rep("C",5), rep("W",2)),
  wq = c("MSL", "NSL", rep("BLL",7),rep("SFBFMWQ",5),rep("GOD",2))
)

#fish stations that seem to be missing
#East marsh: DV3
#Central marsh: CO2, SB2, SU1, BY1, PT1, PT2
#West marsh: GY2, GY1, GY3
#all of these are active stations according to data base
#two stations that aren't on my map of the study: 
#"MZN3": Montezuma Slough - at side channel, 38.1503740699, -121.916719863 (East marsh)
#"SD2": Sheldrake Slough - at Suisun Slough, no coordinates in database (West Marsh)

#add region categories to main data set
savr<- inner_join(sav_cleaner,rgwq) %>% 
  mutate_at(vars(station_code, organism_code,region,wq),factor)
#glimpse(savr)

#look at samples by station and year
#vast majority of samples are in eastern Montezuma Slough (MZ1, MZ2, MZ6)
#note there are multiple entries per month in some cases because of multiple species
savt1<-savr %>% 
  tabyl(station_code)

savt2<-savr %>% 
  tabyl(station_code, year)

#create new data frame that sums all SAV by taxon 
sav_sum_tax <- savr %>% 
  group_by(organism_code) %>% 
  summarize(
    sav_tot = sum(volume))
glimpse(sav_sum_tax)

#create new data frame that sums all SAV by station 
sav_sum_tot <- savr %>% 
  group_by(station_code,region, wq) %>% 
  summarize(
    sav_tot = sum(volume))
glimpse(sav_sum_tot)

#create new data frame that sums all SAV by station and date
sav_sum_dt <- savr %>% 
  group_by(station_code,region, wq, date) %>% 
  summarize(
    sav_tot = sum(volume)) %>% 
  #create a month column 
  mutate(month = month(date)) %>% 
  #create a year column 
  mutate(year = year(date)) 

#create new data frame that sums all SAV by station and date
#And removes algae (not SAV) and sago (salt tolerant native)
sav_sum_dt_sub <- savr %>% 
  filter(organism_code != "STSPP" & organism_code != "ALG") %>% 
  group_by(station_code,region, wq, date) %>% 
  summarize(
    sav_tot = sum(volume)) %>% 
  #create a month column 
  mutate(month = month(date)) %>% 
  #create a year column 
  mutate(year = year(date))

#create new data frame with just egeria
sav_sum_dt_egde <- savr %>% 
  filter(organism_code == "EGDE") %>% 
  #create a month column 
  mutate(month = month(date)) %>% 
  #create a year column 
  mutate(year = year(date))

#create new data frame that sums volume by species for each station
sav_sum_stn_spp <- savr %>% 
  group_by(station_code,region, wq, organism_code) %>% 
  summarize(
    sav_spp_tot = sum(volume))

#create subset with just MZ1 which has by far the most veg
#mostly on a single date
mz1 <- savr %>% 
  filter(station_code == "MZ1")

#plots-----------

#total SAV volume by taxon
(plot_tot_vol_tax <-ggplot(sav_sum_tax, aes(x=organism_code, y=sav_tot))+ 
   geom_bar(stat="identity")
)
#mostly Egeria by volume, followed by algae, coontail, and sago

#total SAV volume by station
(plot_tot_vol_stn <-ggplot(sav_sum_tot, aes(x=station_code, y=sav_tot))+ 
    geom_bar(stat="identity")
)

#total SAV volume time series by station
(plot_tot_vol_stn_dt <-ggplot(sav_sum_dt, aes(x=date, y=sav_tot))+ 
    geom_line() + 
    geom_point() + 
    #ylim(0,13000)+ #exclude a high end outlier in MZ1
    facet_wrap(~station_code)
)

#stacked bar plots showing composition by station
(plot_tot_vol_stn_spp <-ggplot(sav_sum_stn_spp
                               , aes(x=station_code, y= sav_spp_tot,  fill = organism_code))+
    geom_bar(position = "stack", stat = "identity") + 
    ylim(0,25000)+ #zooms in on lower volume stations (MZ1 bar not accurate this way)
    ylab("Volume") + xlab("Station")
)

#stacked bar plots showing composition by month for MZ1
(plot_tot_vol_mz1_spp <-ggplot(mz1
                               , aes(x=date, y= volume,  fill = organism_code))+
    geom_bar(position = "stack", stat = "identity") + 
    #ylim(0,25000)+ #zooms in on lower volume stations (MZ1 bar not accurate this way)
    ylab("Volume") + xlab("Date")
)

#combine SAV and WQ data sets------------

glimpse(sav_sum_dt)
glimpse(wq_data)
wq_data$wq <- factor(wq_data$wq)
#NOTE: some missing WQ data for BLL, so can't include some SAV samples in plots and analyses below
#use different WQ station for those SAV samples

#join them by month, year, and wq station name
vgwq <- left_join(sav_sum_dt,wq_data) %>% 
  #filter out the one extreme outlier for total SAV 
  filter(sav_tot <9000)
#NOTE: BLL is the WQ station used for a number of veg stations
#but BLL data stops in July 2019
#for veg data past that month, would need to designate a different WQ station (NSL)

#join them by month, year, and wq station name
#but with algae and sago excluded
vgwq_sub <- left_join(sav_sum_dt_sub,wq_data) %>% 
  filter(sav_tot <9000)

#join dataset with just egeria with wq
#join them by month, year, and wq station name
vgwq_egde <- left_join(sav_sum_dt_egde,wq_data) %>% 
  filter(volume <9000)

#look at correlation between total vegetation biomass and specific conductance------
#one high end outlier removed

(plot_vg_sc_corr <- ggplot(vgwq, aes(x=sp_cond_avg, y=sav_tot))+
   geom_point()+
   geom_smooth(method='lm')
)

cor.test(x = vgwq$sp_cond_avg, y=vgwq$sav_tot)
#t = -0.30938, df = 78, p-value = 0.7579
#cor = -0.03500853 
#not a significant correlation

#same analysis but with algae and sago excluded
(plot_vg_sc_corr2 <- ggplot(vgwq_sub, aes(x=sp_cond_avg, y=sav_tot))+
    geom_point()+
    geom_smooth(method='lm')
)

cor.test(x = vgwq_sub$sp_cond_avg, y=vgwq_sub$sav_tot)
#t = -0.15111, df = 64, p-value = 0.8804
#corr = -0.01888581

#same analysis but just egeria
(plot_vg_sc_corr3 <- ggplot(vgwq_egde, aes(x=sp_cond_avg, y=volume))+
    geom_point()+
    geom_smooth(method='lm')
)

cor.test(x = vgwq_egde$sp_cond_avg, y=vgwq_egde$volume)
#t = -0.18162, df = 50, p-value = 0.8566
#corr = -0.02567717



#look at correlation between total vegetation biomass and temperature------
(plot_vg_tp_corr <- ggplot(vgwq, aes(x=temp_avg, y=sav_tot))+
   geom_point()+
   geom_smooth(method='lm')
)

cor.test(x = vgwq$temp_avg, y=vgwq$sav_tot)
#t = 0.050997, df = 79, p-value = 0.9595
#cor = 0.005737467
#not a significant correlation


#next steps-----------


#look at correlations for particular species instead of totals (when data sufficient)

#could focus on just a few of the stations in East Marsh where most samples are collected

#could look at particular water year types (very dry and very wet)

#look at correlation between SAV volume and flows into the marsh via SMSCG
#veg caught in the trawl might not have been growing in location where it was collected
#SAV drifts around and could have come from upstream in the Delta




