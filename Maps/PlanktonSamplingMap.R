#Suisun Marsh Salinity Control Gate Action
#Biological Monitoring
#2022 phytoplankton and zooplankton station maps

#load packages
library(tidyverse) #suite of data science tools
library(sf) #working with spatial features
library(deltamapr) #delta base maps
library(ggspatial) #north arrow and scale bar
library(ggrepel) #nonoverlapping point labels

#Note: should add a start and end year to the stations file which will make it easier to work
#with it in the future

#read in data
#get station names and coordinates from EDI
stations_zoop <- read_csv("EDI/data_output/smscg_stations_zoop.csv") %>%
  mutate(station = paste(program, Station, sep = "_"))
stations_phyto <- read_csv("EDI/data_output/smscg_stations_phyto.csv")

stations_all = mutate(stations_zoop,Type = case_when(station %in% stations_phyto$station ~ "Phyto + Zoop",
                                                     program == "EMP" ~ "Phyto + Zoop",
                                                       TRUE ~ "Zoop"))

#look at WW_Delta base map CRS
st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
#EPSG: 4269

stations_plan <- stations_all %>%
  filter(!is.na(longitude), active ==1) %>%
  st_as_sf(coords = c(x='longitude',y='latitude'), remove=F,crs = 4326) 

#create separate dataframe for SMSCG to plot on map-----------
smscg = data.frame(Latitude = 38.093321, Longitude = -121.886291, Station = "SMSCG") %>%
  st_as_sf(coords = c(x='Longitude',y='Latitude'), remove=F,crs = 4326) 
  


#make version of station file with just those with phyto sampling
stations_phyto <- stations_plan %>% 

  filter(Type=="Phyto + Zoop")

#make separate data sets for STN and FMWT to make separate maps
stations_phyto_stn <- stations_phyto %>% 
  filter(program == "STN")

stations_phyto_fmwt <- stations_phyto %>% 
  filter(program == "FMWT")

#Prepare shapefile for adding regions to map--------------

#look at coordinate reference system (CRS) of regions and basemap
#EDSM 2019 Phase 3 Subregions
st_crs(R_EDSM_Subregions_19P3) #NAD83 / UTM zone 10N which is EPSG = 26910
st_crs(WW_Delta) #NAD83 which is EPSG = 4269

#change CRS of both to match station data sets EPSG = 4326
ww_delta_4326 <- st_transform(WW_Delta, crs = 4326)
subregions_4326 <- st_transform(R_EDSM_Subregions_19P3, crs = 4326)


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

save(region_focal, file = "Data/regions_focal.RData")

#DFW sites phyto and zoop map------------------
ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= stations_plan, aes(fill= Type, shape= Type), color= "black",  size= 3.5)+
  #add point for SMSCG 
  geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  scale_shape_manual(
    values=c(21:22)
  )+
  scale_fill_manual(
    values=c(
      #"#7FFF00",
      "#CD6600","#7A378B"))+
  #add station names as labels and make sure they don't overlap each other or the points
  geom_label_repel(data = stations_plan, aes(x=longitude,y=latitude, label=Station) #label the points
                #,nudge_x = -0.008, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                , size = 3 #adjust size and position relative to points
                ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  #add label for SMSCG
  geom_label_repel(data = smscg, aes(x=Longitude,y=Latitude, label=Station) #label the points
                   , size = 3 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
    coord_sf( 
      xlim =c(-122.2, -121.65)
      ,ylim = c(38.0,38.3)
    )+
  annotation_north_arrow() + #Add north arrow
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
    theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle( label = "2024 SMSCG Plankton Stations")
#ggsave(file = "./Maps/SMSCG_Plankton_Map_Field_2023.png",type ="cairo-png", scale=2.5, dpi=300)

#DFW sites phyto only map------------------
ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= stations_phyto, aes(fill = program), shape = 22, color= "black",  size= 3.5)+
  #add point for SMSCG 
  geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  #add station names as labels and make sure they don't overlap each other or the points
  geom_label_repel(data = stations_phyto, aes(x=longitude,y=latitude, label=Station) #label the points
                   #,nudge_x = -0.008, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                   , size = 3 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  #add label for SMSCG
  geom_label_repel(data = smscg, aes(x=Longitude,y=Latitude, label=Station) #label the points
                   , size = 3 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  coord_sf( 
    xlim =c(-122.2, -121.65)
    ,ylim = c(38.0,38.3)
  )+
  annotation_north_arrow()+
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle(label = "2024 SMSCG Phytoplankton Stations")
#ggsave(file = "./Maps/SMSCG_Phytoplankton_Map_Field_2023.png",type ="cairo-png", scale=2.5, dpi=300)

#DFW STN sites phyto only map------------------
ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= stations_phyto_stn, fill = "#7A378B", shape = 22, color= "black",  size= 3.5)+
  #add point for SMSCG 
  #geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  #add station names as labels and make sure they don't overlap each other or the points
  geom_label_repel(data = stations_phyto_stn, aes(x=longitude,y=latitude, label=Station) #label the points
                   #,nudge_x = -0.008, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                   , size = 3 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  #add label for SMSCG
  #geom_label_repel(data = smscg, aes(x=Longitude,y=Latitude, label=Station) #label the points
   #                , size = 3 #adjust size and position relative to points
    #               ,inherit.aes = F #tells it to look at points not base layer
  #) + 
  #zoom in on region where stations are located using bounding box
  coord_sf( 
    xlim =c(-122.2, -121.65)
    ,ylim = c(38.0,38.3)
  )+
  annotation_north_arrow(location = "tl") + #Add north arrow
  #theme(legend.position =c(-121.80, y = 38.20))+ #this isn't working
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle(label = "2024 Summer Townet SMSCG Phytoplankton Stations")
ggsave(file = "./Maps/SMSCG_Phytoplankton_Map_Field_STN_2024.png",type ="cairo-png", 
       scale=1.25, height=3.5, units="in",dpi=300)

#DFW FMWT sites phyto only map------------------
ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= stations_phyto_fmwt, fill = "#7A378B", shape = 22, color= "black",  size= 3.5)+
  #add point for SMSCG 
  #geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  #add station names as labels and make sure they don't overlap each other or the points
  geom_label_repel(data = stations_phyto_fmwt, aes(x=longitude,y=latitude, label=Station) #label the points
                   #,nudge_x = -0.008, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                   , size = 3 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  coord_sf( xlim =c(-122.2, -121.65),ylim = c(38.0,38.3))+
  annotation_north_arrow(location = "tl") + #Add north arrow
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle(label = "2024 FMWT SMSCG Phytoplankton Stations")
ggsave(file = "./Maps/SMSCG_Phytoplankton_Map_Field_FMWT_2024.png",type ="cairo-png", scale=1.25, height=3.5, units="in",dpi=300)

#Study plan map------------------- now with zoops only
#includes shaded regions
ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "lightblue", color= "black") +
  #add shaded regions
  geom_sf(data =region_focal
          , fill = c("gold","orchid1", "chartreuse")
          , alpha=0.2)+
  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  geom_sf(data= filter(stations_plan, program != "ICF"), aes(shape=program), size= 2.5, fill = "blue")+
  scale_shape_manual(
    labels=c('EMP','FMWT', 'STN'),
    values=c(21:24)
  )+

  #add point for SMSCG 
  geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  #add label for SMSCG
  geom_label_repel(data = smscg, aes(x=Longitude,y=Latitude, label=Station) #label the points
                   ,nudge_x = 0.04, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                   , size = 5 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  #zoom in on region where stations are located using bounding box
  coord_sf( 
    xlim =c(-122.1, -121.65)
    ,ylim = c(38.0,38.25)
  )+
  annotation_north_arrow(location = "tl")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
  #annotate("text", label = "2023 SMSCG Plankton Stations", x = -121.80, y = 38.20, size = 4)+
  annotate("text", x = c(-121.9,-121.75,-121.98), y=c(38.24,38.13,38.02), label = c("Suisun Marsh","River","Suisun Bay"), size=6)+
  theme_bw()+
  labs(x="Longitude",y="Latitude")
ggsave(file = "./Maps/SMSCG_zoops_Map_Plan_2025.png",device = "png", width = 8, height =5)



#map for landowner workshop presentaiton

ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "lightblue", color= "black") +

  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  geom_sf(data= stations_plan, aes(fill=program, color=program), size= 3)+
  scale_color_manual(
    values=c("#CD6600","#7A378B", "blue")
  )+
  geom_label_repel(data = smscg, aes(x=Longitude,y=Latitude, label=Station) #label the points
                   ,nudge_x = 0.04, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                   , size = 5 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  #zoom in on region where stations are located using bounding box
  coord_sf( 
    xlim =c(-122.1, -121.65)
    ,ylim = c(38.0,38.25)
  )+
  #north(data = stations, symbol = 12) + #Add north arrow
  #theme(legend.position =c(-121.80, y = 38.20))+ #this isn't working
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
  theme_bw()+
  labs(x="Longitude",y="Latitude")
#ggsave(file = "./Maps/SMSCG_Plankton_Map_Plan_2023.png",type ="cairo-png", scale=1, dpi=300)


