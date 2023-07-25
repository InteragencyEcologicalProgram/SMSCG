#Suisun Marsh Salinity Control Gate Action
#Biological Monitoring
#2023 fish station map
#only show statons sampled by DFW

#load packages
library(tidyverse) #suite of data science tools
library(sf) #working with spatial features
library(deltamapr) #delta base maps
library(ggsn) #north arrow and scale bar
library(ggrepel) #nonoverlapping point labels
library(deltafish) #all the fish data


#get fish stations
create_fish_db()
surv = open_survey() %>%
  filter(Date > as.Date("2015-01-01")) %>%
  collect()

#subset just surveys we want and select stations
susuinfish = filter(surv, Source %in% c("Bay Study", "STN", "FMWT", "Suisun")) %>%
  select(Station, Latitude, Longitude, Source) %>%
  distinct() %>%
  filter(!is.na(Longitude))

#sf object of fish stations
fishsf = st_as_sf(susuinfish, coords = c("Longitude", "Latitude"),remove = F, crs = 4326)

#get GPS coordinates for SMSCG
stations_all <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.3&entityid=877edca4c29ec491722e9d20a049a31c")
smscg <- stations_all %>% 
  filter(StationCode=="SM14") %>% 
  #change Station name for gates
  mutate(Station = if_else(StationCode=="SM14","SMSCG",StationCode),.after = Project,.keep = "unused") %>% 
  #specify CRS WGS84 (EPSG = 4326)
  st_as_sf(coords = c(x='Longitude',y='Latitude'), remove=F,crs = 4326) 


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

#clip to just stations in focal region
fishsf2 = st_join(fishsf, region_focal) %>%
  filter(!is.na(Region_smscg))


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
  geom_sf(data= fishsf2, aes(fill= Source, shape= Source), color= "black",  size= 2.5)+
  scale_shape_manual(values = 21:25)+
  #add point for SMSCG 
  geom_sf(data= smscg, fill = "black", shape = 23, color= "black",  size= 4.5)+

                 #add station names as labels and make sure they don't overlap each other or the points
  # geom_label_repel(data = fishsf2, aes(x=Longitude,y=Latitude, label=Station) #label the points
  #                  #,nudge_x = 0.02, nudge_y = 0.02 #can specify the magnitude of nudges if necessary
  #                  , size = 3 #adjust size and position relative to points
  #                  ,inherit.aes = F #tells it to look at points not base layer
  # ) + 
  #add label for SMSCG
  geom_label_repel(data = smscg, aes(x=Longitude,y=Latitude, label=Station) #label the points
                   ,nudge_x = 0.04, nudge_y = 0.008 #can specify the magnitude of nudges if necessary
                   , size = 5 #adjust size and position relative to points
                   ,inherit.aes = F #tells it to look at points not base layer
  ) +
  #zoom in on region where stations are located using bounding box
  coord_sf( 
  xlim =c(-122.1, -121.65),
  ylim = c(38, 38.25)
   )+
  #north(data = stations, symbol = 12) + #Add north arrow
  #theme(legend.position =c(-121.80, y = 38.20))+ #this isn't working
  theme(plot.margin=grid::unit(c(0,0,0,0), "in"))+
  #annotate("text", label = "2023 SMSCG Plankton Stations", x = -121.80, y = 38.20, size = 4)+
  annotate("text", x = c(-121.85,-121.75,-121.98), y=c(38.19,38.13,38.02), label = c("Suisun \nMarsh","River","Suisun Bay"), size=7)+
  theme_bw()+
  labs(x="Longitude",y="Latitude")
ggsave(file = "SMSCG_fish_Map_Plan_2023.png",type ="cairo-png", scale=1, dpi=300)





