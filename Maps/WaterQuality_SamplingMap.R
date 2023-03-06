#Suisun Marsh Salinity Control Gate Action
#Water Quality monitoring map

#load packages
library(tidyverse) #suite of data science tools
library(sf) #working with spatial features
library(deltamapr) #delta base maps
library(ggsn) #north arrow and scale bar
library(ggrepel) #nonoverlapping point labels

#read in data
#get station names and coordinates from EDI
#stations_all <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.3&entityid=877edca4c29ec491722e9d20a049a31c")

#use my updated version instead
stations_all <-read_csv("./Data/wq_stations_2023-03-03.csv")

#look at WW_Delta base map CRS
st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
#EPSG: 4269

#format the data set

stations <- stations_all %>%
  #specify CRS WGS84 (EPSG = 4326)
  st_as_sf(coords = c(x='longitude',y='latitude'), remove=F,crs = 4326) %>% 
  glimpse()

#create separate dataframe for SMSCG to plot on map
smscg <- stations_all %>% 
  filter(station=="MSL") %>% 
  #change station name for gates
  mutate(station = if_else(station=="MSL","SMSCG",station),.after = program,.keep = "unused") %>% 
  #specify CRS WGS84 (EPSG = 4326)
  st_as_sf(coords = c(x='longitude',y='latitude'), remove=F,crs = 4326) 


#create object from bounding box for the stations
#add a buffer around points to improve map aesthetic
bbox_p <- st_bbox(st_buffer(stations,2000))

#Prepare shapefile for adding regions to map--------------

#look at coordinate reference system (CRS) of regions and basemap
#EDSM 2019 Phase 3 Subregions
st_crs(R_EDSM_Subregions_19P3) #NAD83 / UTM zone 10N which is EPSG = 26910
st_crs(WW_Delta) #NAD83 which is EPSG = 4269

#change CRS of both to match station data sets EPSG = 4326
ww_delta_4326 <- st_transform(WW_Delta, crs = 4326)
subregions_4326 <- st_transform(R_EDSM_Subregions_19P3, crs = 4326)

#make map
(map_region_all<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data = ww_delta_4326, fill= "lightblue", color= "black")+
    #EDSM 2017-18 Phase 1 Strata
    geom_sf(data = subregions_4326, aes(fill=SubRegion), alpha=0.8)+
    #add title
    ggtitle("R_EDSM_Subregions_19P3")+
    theme_bw()
)

#only keep the needed subregions

#create vector of subregions to keep
subregions_focal <- c("Suisun Marsh","Grizzly Bay","Mid Suisun Bay","Honker Bay","Confluence","Lower Sacramento River","Sacramento River near Rio Vista")

#categorize subregions into regions useful for SMSCG
regions_new <- as.data.frame(
  cbind(
    "Region_smscg" = c("Suisun Marsh",rep("Suisun Bay",3),rep("River",3))
    ,"SubRegion" = c("Suisun Marsh","Grizzly Bay","Mid Suisun Bay","Honker Bay","Confluence","Lower Sacramento River","Sacramento River near Rio Vista")
  )
)

region_focal <- subregions_4326 %>% 
  #just keep the needed subregions
  filter(SubRegion %in% subregions_focal) %>% 
  #add SMSCG regions which groups the subregions appropriately
  left_join(regions_new) %>% 
  group_by(Region_smscg) %>% 
  summarise(SQM = sum(SQM), do_union = TRUE)

#remake map with SMSCG regions  
(map_region_focal<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= ww_delta_4326, fill= "lightblue", color= "black")+
    #reduced region
    geom_sf(data =region_focal, aes(fill=Region_smscg), alpha=0.8)+
    #add title
    ggtitle("Focal Region")+
    theme_bw()
)
#overall this matches up pretty well with where our focal stations are


#plot for study plan-------------------
#includes shaded regions
ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "lightblue", color= "black") +
  #add shaded regions
  geom_sf(data =region_focal
          , fill = c("gold","orchid1", "chartreuse")
          , alpha=0.2)+
  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= stations, fill="black",shape = 21, color= "black",  size= 3.5)+
  #add point for SMSCG 
  geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  #add label for SMSCG
  geom_label_repel(data = smscg, aes(x=longitude,y=latitude, label=station) #label the points
                   ,nudge_x = -0.04, nudge_y = -0.008 #can specify the magnitude of nudges if necessary
                   , size = 5 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  geom_label_repel(data = stations, aes(x=longitude,y=latitude, label=station) #label the points
                   #,nudge_x = 0.02, nudge_y = 0.02 #can specify the magnitude of nudges if necessary
                   , size = 3 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  #zoom in on region where stations are located using bounding box
  coord_sf( 
    xlim =c(bbox_p$xmin,bbox_p$xmax)
    ,ylim = c(bbox_p$ymin,bbox_p$ymax)
  )+
  #north(data = stations, symbol = 12) + #Add north arrow
  #theme(legend.position =c(-121.80, y = 38.20))+ #this isn't working
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
  #annotate("text", label = "2023 SMSCG Plankton stations", x = -121.80, y = 38.20, size = 4)+
  annotate("text", x = c(-121.78,-121.75,-121.98), y=c(38.19,38.13,38.025), label = c("Suisun Marsh","River","Suisun Bay"), size=8)+
  theme_bw()+
  labs(x="Longitude",y="Latitude")
#ggsave(file = "./Maps/SMSCG_WQ_Map_Plan_2023.png",type ="cairo-png", scale=2.5, dpi=300)





