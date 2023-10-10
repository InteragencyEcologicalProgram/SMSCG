#Suisun Marsh Salinity Control Gate Action
#calculate distances along waterways among monitoring stations, including salinity control gates

#Nick Rasmussen 10/09/2023

#Notes-----------
#this script doesn't include WQ because the EDI station file was missing a bunch of WQ stations
#see SMSCG_DataFormatting_Stations_WQ.R

#also decided not to use EDI station file for plankton
#read in newer file located in this repo

#To do list-----------
#would be good to spot check the distances 

#required packages
library(sf)
library(tidyverse)
library(spacetools)
library(deltamapr)

#read in data from EDI; will only use this file for the clam stations
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.876.5
stn_smscg <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.5&entityid=877edca4c29ec491722e9d20a049a31c")

#read in plankton station data from repo
#just use this for zoop for now; eventually replace with zoop specific station metadata file
station_zoop <- read_csv("./StationDistances/station_plankton.csv") %>% 
  glimpse()

#read in phytoplankton station data from repo
station_phyto <- read_csv("./EDI/data_output/smscg_stations_phyto.csv")

#EDI: add geometry column to station dataframes------

#create geometry column in station data frame
stn_smscg_4326 <- stn_smscg %>%
  #first add station code for SMSCG (currently NA)
  mutate(StationCode = case_when(StationName=="Salinity Control Gates"~"SMSCG",TRUE~StationCode)) %>% 
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='Longitude',y='Latitude'),
           crs = 4326 #EPSG code for WGS84
           ,remove=F #retains original columns
  )   

#create data frame with just the SMSCG location, which we will add to other data sets
gates <- stn_smscg_4326 %>% 
  filter(StationCode=="SMSCG") %>% 
  select(
    station = StationCode
    ,longitude = Longitude
    ,latitude = Latitude
    ,geometry
  ) %>% 
  glimpse()

#create object from bounding box for the stations
#add a buffer around points to improve map aesthetic
#will use this to crop the map to focal area
bbox_stn <- st_bbox(st_buffer(stn_smscg_4326,2000))

#do the same for plankton station data file
stn_zoop_4326 <- station_plank %>%
  #drop the two EZ stations which have NA for coordinates
  filter(!is.na(longitude)) %>% 
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude',y='latitude'),
           crs = 4326 #EPSG code for WGS84
           ,remove=F #retains original columns
  )   

#phyto: create geometry column and add location of gates
stn_phyto_4326 <- bind_rows(station_phyto,gates) %>% 
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude',y='latitude'),
           crs = 4326 #EPSG code for WGS84
           ,remove=F #retains original columns
  )   

#create subsets of stations by type--------

#discrete wq
#Discrete EMP stations include water quality but we don't use these 
#data for this study

#continuous wq
#all stations with WQ=Y are sonde stations
#don't need to include SMSCG because MSL wq station is at that location
#need to filter out SMSCG because they say WQ=Y but again it's redundant
#only other data type collected at these stations is clams (sometimes)
#all sonde stations are included in data set twice
#once with sonde station name and once with clam station name

#just create list of sonde stations to use for filtering stations
#NOTE: there are a bunch of stations missing from the EDI file
#sondes <- c("BDL","CSE","GZL","HUN","MSL","NSL")

#stn_wq <- stn_smscg_4326 %>% 
#  filter(StationCode %in% sondes)

#zoop: overlaps with phyto but not wq or clams
#phyto: a subset of zooplankton stations
#NOTE: need to work on this because neither station names or station codes are unique
#need to combine project and station code probably
stn_zoop <- stn_zoop_4326 %>% 
  #first combine program and station code so all station codes are unique
  #default separator is "_" which is fine
  unite("project_station",program,station,remove=F) 

#clams
#most clam stations are only clams
#though sometimes clams are sampled at continuous wq sites
#drop rows with sonde station names because they are redundant with overlapping clam station rows
stn_clam <- stn_smscg_4326 %>% 
  filter(Clams=="Y" & !(StationCode %in% sondes))

#Plot the Delta shapefile built into the spacetools package----
#it includes the whole Bay-Delta, not just the legal delta

#(map_delta <- ggplot()+
   #plot waterways base layer
#   geom_sf(data= spacetools::Delta, fill= "skyblue3", color= "black") +
#   theme_bw()
#)

#look at Delta base map CRS
#st_crs(Delta)
#CRS = NAD83 (EPSG: 4269)

#convert Delta base map to WGS84, which is CRS of stations  
Delta_4326 <- st_transform(Delta, crs = 4326)
#st_crs(Delta_4326) #looks like it worked

#map stations on Delta base map----------

#make map
(map_delta_stns <- ggplot()+
    #plot waterways base layer
    geom_sf(data= Delta_4326, fill= "skyblue3", color= "black") +
    #plot stations
    geom_sf(data=stn_smscg_4326, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_stn$xmin, bbox_stn$xmax),
      ylim = c(bbox_stn$ymin, bbox_stn$ymax)
    ) +
    theme_bw()+
    ggtitle("SMSCG")
)

#Calculate distances along waterways------
#documentation doesn't indicate the units of the distances
#but they are probably meters

#Rasterize and transition Delta shapefile
#I think this is supposed to reduce processing time when doing 
#multiple sets of calculations like I am doing
#start with default grid size of 75 m
#this isn't working; refer to documentation; it might be because I have it in wgs84
#Delta_Map <- Maptransitioner(Water_map = Delta_4326, Grid_size=75, Process_map = T, Plot = T)


#sonde stations 
#distance_wq<-Waterdist(Water_map = Delta_4326
#                          , Points = stn_wq
#                          , Latitude_column = Latitude
#                          ,Longitude_column = Longitude
#                          , PointID_column = StationCode)
#NOTE: BDL coordinates were also used for GZL, which needs to be corrected
#the updated metadata document in progress fixes this

#clam stations and SMSCG
distance_clam<-Waterdist(Water_map = Delta_4326
                         , Points = stn_clam
                         , Latitude_column = Latitude
                         ,Longitude_column = Longitude
                         , PointID_column = StationCode)

#should figure out a way to spot check these distances to see if they are accurate


#zoop stations and SMSCG
distance_zoop<-Waterdist(Water_map = Delta_4326
                       , Points = stn_zoop
                       , Latitude_column = latitude
                       ,Longitude_column = longitude
                       , PointID_column = project_station)

#phyto stations and SMSCG
distance_phyto<-Waterdist(Water_map = Delta_4326
                         , Points = stn_phyto_4326
                         , Latitude_column = latitude
                         , Longitude_column = longitude
                         , PointID_column = station)




#save distance matrices as csv files------
#need to convert matrix to dataframe and make rownames a column before export
#maybe it makes more sense to convert wide to long 
#and delete duplicate comparisons and zero distances (ie, comparisons between station and itself) 

#wq
#library(reshape)
#this transposes the upper triangle of your table into three columns 
#distance_wq_format <- melt(distance_wq)[melt(upper.tri(distance_wq))$value,]
#names(distance_wq_format) <- c("station1","station2","distance_m")

#wq
#NOTE: this is incomplete because there are stations missing
#distance_wq_df <- as.data.frame(distance_wq) %>% 
#  rownames_to_column("station")

#write_csv(distance_wq_df,"./StationDistances/station_distances_m_wq.csv")

#phyto
distance_phyto_df <- as.data.frame(distance_phyto) %>% 
  rownames_to_column("station")

#write_csv(distance_phyto_df,"./StationDistances/station_distances_m_phyto.csv")

#plankton
distance_zoop_df <- as.data.frame(distance_zoop) %>% 
  rownames_to_column("station")

#write_csv(distance_zoop_df,"./StationDistances/station_distances_m_zoop.csv")


#clams
distance_clam_df <- as.data.frame(distance_clam) %>% 
  rownames_to_column("station")

#write_csv(distance_clam_df,"./StationDistances/station_distances_m_clam.csv")





