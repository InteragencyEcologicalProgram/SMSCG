#Suisun Marsh Salinity Control Gate Action
#UC-Davis Suisun Marsh Fish Survey
#Otter trawl
#Submerged aquatic vegetation data

#To do list
#what are the volume units for SAV?
#add the "non-detect" otter trawl samples
#incorporate survey effort (trawl duration)
#try using UCD trawl WQ data (SpCond, secchi, depth)
#refer to following database tabs: Sample, Depth, TrawlEffort


#required packages
library(tidyverse)
library(janitor) #function for data frame clean up
library(lubridate) #format dates

#additional data sets needed for analysis----------
#survey effort: need to know SAV volume per unit survey effort (though maybe this is standardized)
#water quality measurements: salinity is key but turbidity would be nice too

# Read in the Data----------------------------------------------
# Datasets are on SharePoint site for the SMSCG action

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data"
  )
)  

#read in submerged aquatic vegetation data
sav_data<-read_csv(file = paste0(sharepoint_path,"./UC-Davis Suisun Marsh Fish Survey/SAV_Data_2014-2020.csv"))

#read in water quality data
wq_data<-read_csv(file = paste0(sharepoint_path,"./WaterQuality/AquaticVegetation_WQ_SummarizedData.csv"))

#format SAV data-----------

#remove unneeded columns
#MethodCode = OTR for all samples because all from same type of gear
#StandardLength and Count are all zeros because Volume is unit of measurement for SAV

sav_cleaner <- sav_data %>% 
  #use janitor package to simplify column names
  clean_names() %>% 
  #format date
  mutate(date = dmy(sample_date)) %>% 
  #remove unneeded columns and reorder remaining columns
  select(station_code
         ,date
         ,year
         ,month
         ,sample_time
         ,organism_code
         ,volume) 
  

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




