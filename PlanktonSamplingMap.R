#Suisun Marsh Salinity Control Gate Action
#Biological Monitoring
#2022 phytoplankton and zooplankton station map
#only show statons sampled by DFW

#load packages
library(tidyverse) #suite of data science tools
library(sf) #working with spatial features
library(deltamapr) #delta base maps
library(ggsn) #north arrow and scale bar
library(ggrepel) #nonoverlapping point labels

#Note: should add a start and end year to the stations file which will make it easier to work
#with it in the future
#update the GZM GPS coordinates: 38.13234, -122.05770 

#read in data
#get station names and coordinates from EDI
stations_all <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.3&entityid=877edca4c29ec491722e9d20a049a31c")

#look at WW_Delta base map CRS
st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
#EPSG: 4269

#format the data set

#need to add a column that indicates
#add column that indicates whether sampling is specific to SMSCG; then use it as shape

stations_p <- stations_all %>%
  #filter stations to just those used in 2020 for plankton sampling
  #add Grizzly Bay stations (GZM and 602)
  filter(Zoops == "Y" | Phyto == "Y" | StationCode == "GZM" | StationCode == "602") %>% 
  #filter some other stations that aren't currently sampled by DFW
  filter(StationCode != "NZ032" & StationCode != "NZ028" & StationCode!="HONK" & StationCode != "NZ054" & StationCode != "D22" & StationCode!="520" &
         StationCode != "D4" & StationCode!="NZ068" & StationCode!="707" & StationCode!="GRIZZ" & StationCode!="NZ060" & StationCode!="NZ064" & Longitude > -122.0833 &
           StationCode!="501" & StationCode!="504"
         ) %>% 
  #drop clam column
  select(-Clams) %>% 
  #convert coordinates data frame to sf object
  #WGS84 (EPSG = 4326)
  #then transform coordinates to NAD83 which is CRS of base layer
  st_as_sf(coords = c(x='Longitude',y='Latitude'), remove=F,crs = 4326) %>%  
  #transform to NAD83
  st_transform(crs = 4269) %>% 
  glimpse()

#add column to use for color coding stations
#zoop only, phyto only, both
stations <- stations_p %>% 
  mutate(
    #change the N to Y for phyto in two 602 stations
    Phyto = ifelse(grepl("602",StationCode),"Y",Phyto)
    #change the N to Y for phyto in GZM station
    #couldn't quickly figure out how to change 602 and GZM in same line
    ,Phyto = ifelse(grepl("GZM",StationCode),"Y",Phyto)
    ,Type = as.factor(
    if_else(Zoops=="Y" & Phyto == "Y", "B" 
         ,if_else(Zoops=="N" & Phyto == "Y", "P"
                  ,if_else(Zoops=="Y" & Phyto == "N","Z","N")))
  )) %>% 
  glimpse()


#create object from bounding box for the stations
#add a buffer around points to improve map aesthetic
bbox_p <- st_bbox(st_buffer(stations,2000))

#reorder station type factor levels for plotting
stations$Type <- factor(stations$Type, levels=c('P','Z','B'))

#plot bay-delta base layer with phytoplankton and zooplankton stations
#move legend into plot space
#work on station labels
ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
  #plot the 2014-2016 sampling locations based on Excel data
  geom_sf(data= stations, aes(fill= Type, shape= Type), color= "black",  size= 3.5)+
  scale_shape_manual(
    labels=c('Phyto','Zoop','Both'),
    values=c(21:23)
  )+
  scale_fill_manual(
    labels=c('Phyto','Zoop','Both'),
    values=c("#7FFF00","#CD6600","#7A378B"))+
  #add station names as labels
  #geom_sf_label(data = stations, aes(label = StationCode), #label the points
 #              nudge_x = -0.008, nudge_y = 0.008, size = 3, #adjust size and position relative to points
 #              inherit.aes = F, #tells it to look at points not base layer
 #              ) + 
  geom_text_repel(data = stations, aes(x=Latitude,y=Longitude, label=StationCode), #label the points
                nudge_x = -0.008, nudge_y = 0.008, size = 3, #adjust size and position relative to points
                inherit.aes = F, #tells it to look at points not base layer
  ) + 
    #zoom in on region where stations are located using bounding box
    coord_sf( 
      xlim =c(bbox_p$xmin,bbox_p$xmax)
      ,ylim = c(bbox_p$ymin,bbox_p$ymax)
    )+
  north(data = stations, symbol = 3) + #Add north arrow
  theme(legend.position =c(-121.80, y = 38.20))+ #this isn't working
    theme_bw()+
  labs(x="Latitude",y="Longitude")+
  annotate("text", label = "2022 SMSCG Plankton Stations", x = -121.80, y = 38.20, size = 4)
#ggsave(file = "SMSCG_Plankton_Map.png",type ="cairo-png",width=8.5, units="in",scale=1.5, dpi=300)

#make a map with just phyto samples 



