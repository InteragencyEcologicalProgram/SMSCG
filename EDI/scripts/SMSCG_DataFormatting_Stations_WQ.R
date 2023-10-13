#Suisun Marsh Salinity Control Gate Action
#creating metadata file for water quality stations

#Nick Rasmussen 6/19/2023

#required packages
library(sf)
library(tidyverse)
library(deltamapr)
library(janitor)
library(lubridate)
library(spacetools)

#read in the data---------

#read in the metadata file I started "by hand"
#probably just need to add station distances
wq_meta <- read_csv("./EDI/data_input/wq/smscg_wq_metadata.csv") %>% 
  glimpse()

#plot the station coordinates to check for errors---------

#look at WW_Delta base map CRS
st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
#EPSG: 4269

#change CRS of of basemap to match station data sets EPSG = 4326
ww_delta_4326 <- st_transform(WW_Delta, crs = 4326)

#format the station data set
station_wq <- wq_meta %>%
  #specify CRS WGS84 (EPSG = 4326)
  st_as_sf(coords = c(x='longitude',y='latitude'), remove=F,crs = 4326) %>% 
  glimpse()

#create object from bounding box for the stations
#add a buffer around points to improve map aesthetic
bbox_p <- st_bbox(st_buffer(station_wq,2000))

#make map
(map_region_all<-ggplot()+
    #Delta waterways
    geom_sf(data = ww_delta_4326, fill= "lightblue", color= "black")+
    #plot station locations 
    geom_sf(data= station_wq, fill="black",shape = 21, color= "black",  size= 3.5)+
    #zoom in on region where stations are located using bounding box
    coord_sf( 
      xlim =c(bbox_p$xmin,bbox_p$xmax)
      ,ylim = c(bbox_p$ymin,bbox_p$ymax)
    )+
    labs(x="Longitude",y="Latitude")+
    theme_bw()
)
#looks good

#calculate distances among stations along the waterway-----------
#spacetools documentation doesn't indicate the units of the distances
#but they are probably meters

distance_wq<-Waterdist(Water_map = ww_delta_4326
                       , Points = station_wq
                       , Latitude_column = latitude
                       ,Longitude_column = longitude
                       , PointID_column = station)

#transposes the upper triangle of table into three columns 
#library(reshape)  
distance_wq_format <- reshape::melt(distance_wq)[reshape::melt(upper.tri(distance_wq))$value,]
names(distance_wq_format) <- c("station1","station2","distance_m")

distance_wq_df <- as.data.frame(distance_wq) %>% 
  rownames_to_column("station")

#format the distance matrix--------

#just keep the needed columns
wq_dist_trunc <- distance_wq_df %>% 
  select(station
         ,smscg_distance = MSL
  ) %>% 
  #round distances to nearst m
  mutate(smscg_distance_m = round(smscg_distance,0)) %>% 
  select(station = station
         ,smscg_distance_m
  ) %>% 
  glimpse()

#add distances to main metadata df---------

wq_format <- wq_meta %>% 
  left_join(wq_dist_trunc) %>% 
  relocate(smscg_distance_m,.after = location) %>% 
  arrange(region,longitude) %>% 
  select(-c(program,station_zoop:station_clam)) %>% 
  glimpse()

#write the output file------
#write_csv(wq_format,"./EDI/data_output/smscg_stations_wq_updated.csv")





