#Suisun Marsh Salinity Control Gate Action
#Biological Monitoring
#2022 phytoplankton and zooplankton station map
#only show statons sampled by DFW

#To do list
#add rest of EMP stations to map for study plan
#NZ032, D7, D8, D10, D4, D22

#load packages
library(tidyverse) #suite of data science tools
library(sf) #working with spatial features
library(deltamapr) #delta base maps
library(ggsn) #north arrow and scale bar
library(ggrepel) #nonoverlapping point labels

#Note: should add a start and end year to the stations file which will make it easier to work
#with it in the future

#read in data
#get station names and coordinates from EDI
stations_all <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.3&entityid=877edca4c29ec491722e9d20a049a31c")

#look at WW_Delta base map CRS
st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
#EPSG: 4269

#format the data set for DFW map-------------

stations_dfw <- stations_all %>%
  #filter stations to just those used in 2020 for plankton sampling
  #add Grizzly Bay station (602) and SMSCG
  filter(Zoops == "Y" | Phyto == "Y" | StationCode == "602" ) %>% 
  #filter some other stations that aren't currently sampled by DFW
  filter(StationCode != "NZ032" & StationCode != "NZ028" & StationCode!="HONK" & StationCode != "NZ054" & StationCode != "D22" & StationCode!="520" &
         StationCode != "D4" & StationCode!="NZ068" & StationCode!="707" & StationCode!="GRIZZ" & StationCode!="NZ060" & StationCode!="NZ064" & Longitude > -122.0833 &
           StationCode!="501" & StationCode!="504"
         ) %>% 
  #correct location of GZM
  #mutate(
  #  Longitude = ifelse(grepl("GZM",StationCode),-122.05770, Longitude)
  #  ,Latitude = ifelse(grepl("GZM",StationCode),38.13234,Latitude)
  #  ) %>% 
  #drop clam column
  select(-Clams) %>% 
  #specify CRS WGS84 (EPSG = 4326)
  st_as_sf(coords = c(x='Longitude',y='Latitude'), remove=F,crs = 4326) %>% 
  #rename column
  rename(Station = StationCode) %>% 
  glimpse()

#format the data set for SMSCG study plan map-------------

stations_plan <- stations_all %>%
  #filter stations to just those used in 2020 for plankton sampling
  #add Grizzly Bay station (602) and SMSCG
  filter(Zoops == "Y" | Phyto == "Y" | StationCode == "602" ) %>% 
  #filter some other stations that aren't currently sampled by DFW
  #but keep the EMP stations
  filter(StationCode!="HONK" & StationCode!="520" &
           StationCode!="NZ068" & StationCode!="707" & StationCode!="GRIZZ" & Longitude > -122.0833 &
           StationCode!="501" & StationCode!="504"
  ) %>% 
  #correct location of GZM
  #mutate(
  #  Longitude = ifelse(grepl("GZM",StationCode),-122.05770, Longitude)
  #  ,Latitude = ifelse(grepl("GZM",StationCode),38.13234,Latitude)
  #  ) %>% 
  #drop clam column
  select(-Clams) %>% 
  #specify CRS WGS84 (EPSG = 4326)
  st_as_sf(coords = c(x='Longitude',y='Latitude'), remove=F,crs = 4326) %>% 
  #rename column
  rename(Station = StationCode) %>% 
  glimpse()

#create separate dataframe for SMSCG to plot on map-----------
smscg <- stations_all %>% 
  filter(StationCode=="SM14") %>% 
  #change Station name for gates
  mutate(Station = if_else(StationCode=="SM14","SMSCG",StationCode),.after = Project,.keep = "unused") %>% 
  #specify CRS WGS84 (EPSG = 4326)
  st_as_sf(coords = c(x='Longitude',y='Latitude'), remove=F,crs = 4326) 
  

#DFW map: add column to use for color coding stations
#zoop only, phyto only (none currently), both
stations <- stations_dfw %>% 
  #add column that combines survey and station name
  unite("Station_label","Project","Station", sep= " ",remove=F) %>%
  mutate(
    #change the N to Y for phyto in two 602 stations
    Phyto = ifelse(grepl("602",Station),"Y",Phyto)
    #couldn't quickly figure out how to change 602 and 519 in same line
    #change the N to Y for phyto in two 519 stations
    ,Phyto = ifelse(grepl("519",Station),"Y",Phyto)
    ,Type = as.factor(
    if_else(Zoops=="Y" & Phyto == "Y", "B" 
         #,if_else(Zoops=="N" & Phyto == "Y", "P"
                  ,if_else(Zoops=="Y" & Phyto == "N","Z","N")
         #)
    ) )  
    #modify name for MONT
    ,Station_label = if_else(Station_label == "STN MONT","MONT",Station_label)
    ) %>% 
  glimpse()

#Study plan map: add column to use for color coding stations
#zoop only, phyto only (none currently), both
stations_plan_cat <- stations_plan %>% 
  #add column that combines survey and station name
  #unite("Station_label","Project","Station", sep= " ",remove=F) %>%
  mutate(
    #change the N to Y for phyto in two 602 stations
    Phyto = ifelse(grepl("602",Station),"Y",Phyto)
    #couldn't quickly figure out how to change 602 and 519 in same line
    #change the N to Y for phyto in two 519 stations
    ,Phyto = ifelse(grepl("519",Station),"Y",Phyto)
    #change the N to Y for phyto in NZ028
    ,Phyto = ifelse(grepl("NZ028",Station),"Y",Phyto)
    #change the N to Y for phyto in NZ054
    ,Phyto = ifelse(grepl("NZ054",Station),"Y",Phyto)
    ,Type = as.factor(
      if_else(Zoops=="Y" & Phyto == "Y", "B" 
              #,if_else(Zoops=="N" & Phyto == "Y", "P"
              ,if_else(Zoops=="Y" & Phyto == "N","Z","N")
              #)
      ) )  
    #modify name for MONT
    #,Station_label = if_else(Station_label == "STN MONT","MONT",Station_label)
  ) %>% 
  glimpse()

#create object from bounding box for the stations
#add a buffer around points to improve map aesthetic
bbox_p <- st_bbox(st_buffer(stations,2000))

#DFW: reorder station type factor levels for plotting
stations$Type <- factor(stations$Type
                        , levels=c(
                          #'P',
                          'Z','B'))

#Plan: reorder station type factor levels for plotting
stations_plan_cat$Type <- factor(stations_plan_cat$Type
                        , levels=c(
                          'Z','B'))

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


#DFW map------------------
ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= stations, aes(fill= Type, shape= Type), color= "black",  size= 3.5)+
  #add point for SMSCG 
  geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+
  scale_shape_manual(
    labels=c(
      #'Phyto',
      'Zoop','Zoop + Phyto'),
    values=c(21:22)
  )+
  scale_fill_manual(
    labels=c(
      #'Phyto',
      'Zoop','Zoop + Phyto'),
    values=c(
      #"#7FFF00",
      "#CD6600","#7A378B"))+
  #add station names as labels and make sure they don't overlap each other or the points
  geom_label_repel(data = stations, aes(x=Longitude,y=Latitude, label=Station_label) #label the points
                #,nudge_x = -0.008, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                , size = 3 #adjust size and position relative to points
                ,inherit.aes = F #tells it to look at points not base layer
  ) + 
  #add label for SMSCG
  geom_label_repel(data = smscg, aes(x=Longitude,y=Latitude, label=Station) #label the points
                   , size = 3 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) + 
    #zoom in on region where stations are located using bounding box
    coord_sf( 
      xlim =c(bbox_p$xmin,bbox_p$xmax)
      ,ylim = c(bbox_p$ymin,bbox_p$ymax)
    )+
  north(data = stations, symbol = 12) + #Add north arrow
  #theme(legend.position =c(-121.80, y = 38.20))+ #this isn't working
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
    theme_bw()+
  labs(x="Longitude",y="Latitude")+
  annotate("text", label = "2023 SMSCG Plankton Stations", x = -121.80, y = 38.20, size = 4)
#ggsave(file = "./Maps/SMSCG_Plankton_Map_Field_2023.png",type ="cairo-png", scale=2.5, dpi=300)


#Study plan map-------------------
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
  geom_sf(data= stations_plan_cat, aes(fill=Type,color=Type, shape=Project), size= 2.5)+
  scale_shape_manual(
    labels=c('EMP','FMWT','STN'),
    values=c(21:23)
  )+
   scale_color_manual(
   labels=c('Zoop','Zoop + Phyto'),
    values=c("#CD6600","#7A378B")
   )+
  scale_fill_manual(
    labels=c('Zoop','Zoop + Phyto'),
    values=c("#CD6600","#7A378B")
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
    xlim =c(bbox_p$xmin,bbox_p$xmax)
    ,ylim = c(bbox_p$ymin,bbox_p$ymax)
  )+
  #north(data = stations, symbol = 12) + #Add north arrow
  #theme(legend.position =c(-121.80, y = 38.20))+ #this isn't working
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
  #annotate("text", label = "2023 SMSCG Plankton Stations", x = -121.80, y = 38.20, size = 4)+
  annotate("text", x = c(-121.78,-121.75,-121.98), y=c(38.19,38.13,38.02), label = c("Suisun Marsh","River","Suisun Bay"), size=8)+
  theme_bw()+
  labs(x="Longitude",y="Latitude")
#ggsave(file = "./Maps/SMSCG_Plankton_Map_Plan_2023.png",type ="cairo-png", scale=1, dpi=300)





