#Suisun Marsh Salinity Control Gate
#Zooplankton
#Power analysis to determine amount of required sampling

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(lubridate) #working with dates
library(zooper) #Bay-Delta zooplankton data
library(sf) #spatial analysis
library(deltamapr) #maps of the Bay-Delta
library(pwr) #power analysis

#read in data--------------

#results from the modeling that compared SMSCG action and non-action years
modout <- read_csv("./PowerAnalysis/modeledzoops_fixed.csv") %>% 
  clean_names() %>% 
  select(prey:bpue3) %>% 
  glimpse()

#zoop BPUE from SMSCG GitHub repo
zoop_bpue_git <- read_csv("./Data/zoop_Copepod and Cladoceran Biomass Values.csv") %>% 
  clean_names()

#zoop BPUE from EDI
zoop_bpue_edi <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.539.3&entityid=e4dc2a5bedb35f15d13b4ca04a46024d") %>% 
  clean_names()

#sacramento valley water year types from drought synthesis
#data originates from http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
water_year <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/DroughtSynthesis/main/data/yearassignments.csv") %>% 
  clean_names() %>% 
  arrange(year)

#format water year type data set--------------
wy <- water_year %>% 
  select(year,yr_type)


#format the zoop modeling data------------
#results are divided by month and taxa
#presumably the values are estimated biomass of carbon
#combine taxa within month but for now keep months separate

#create vector of distinct taxa
mod_taxa <- modout %>% 
  distinct(prey) %>% 
  pull(prey)
#"acartela"   "allcopnaup" "daphnia"    "eurytem"    "limno"      "mysid"      "othcalad"   "othcaljuv"  "othclad"   
#"othcyc"     "other"      "pdiapfor"

#summarize action and no action results by month
modout_sum_mo <- modout %>%
  group_by(month,scenario) %>% 
  summarise(bpue = sum(bpue3)) %>% 
  #convert long to wide
  pivot_wider(names_from = scenario, values_from = bpue) %>% 
  rename(na_sum = NoAct,ya_sum = SMSCG4ppt) %>% 
  #calculate percent change
  mutate(
    num = ya_sum-na_sum
    ,denom = (na_sum+ya_sum)/2
    ,perc_change = (num/denom)*100
  )

#look at range of percent change
range(modout_sum_mo$perc_change)
#0.00000 86.85506


#get data from zooper package-----------------------

#get the data from DOP, STN, EMP, and FMWT
zoop_grab <- Zoopsynther(Data_type = "Community"
                         ,Sources=c("EMP","DOP","STN","FMWT")
                         #all the SMSCG monitoring is focused on mesozooplankton so we'll stick to that
                         ,Size_class = "Meso"
                         #I don't think Eastern Montezuma Slough was sampled prior to 2018
                         ,Date_range = c("2018-01-01",NA)
                         #probably has been much change in taxonomic resolution since 2018 but specify this anyway
                         ,Time_consistency = T
                         ) %>% 
  clean_names() 
#"These species have no relatives in their size class common to all datasets and have been removed from one or more size classes:
#Camptocercus Undifferentiated (Meso), Ostracoda Undifferentiated (Meso), Ostracoda Adult (Meso), Cumacea Undifferentiated (Meso)"

#take a look at the columns
glimpse(zoop_grab)

#range of dates
range(zoop_grab$date)
#"2018-01-05 PST" "2021-12-16 PST"

#Initial formatting of zoop data---------------------
#Should we filter out undersampled taxa? For this purpose, I'm not sure it matters that much

zoop_select <- zoop_grab %>% 
  #reduce to just the needed columns
  select(source
         ,station
         ,latitude
         ,longitude
         ,sample_id
         ,date
         ,year
         ,tow_type
         ,tide
         ,taxname
         ,lifestage
         ,undersampled
         ,cpue
         ) %>% 
  glimpse()

#are GPS coordinates missing for any samples?
zoop_coord_miss <- zoop_select %>% 
  distinct(sample_id,latitude,longitude)  %>%
  filter(is.na(latitude) | is.na(longitude) )
#only 6 of 6311 samples, so just filter those out

#add geometry column for spatial filtering
zoop_geom <- zoop_select %>% 
  #remove samples with missing lat/long
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude',y='latitude')
           ,crs = 4326 #EPSG code for WGS84
           ,remove = F
  )   %>% 
  glimpse()

#Prepare shapefile for filtering data spatially--------------

#look at coordinate reference system (CRS) of regions and basemap
#EDSM 2019 Phase 3 Subregions
st_crs(R_EDSM_Subregions_19P3) #NAD83 / UTM zone 10N which is EPSG = 26910
st_crs(WW_Delta) #NAD83 which is EPSG = 4269

#change CRS of both to match zoop data sets EPSG = 4326
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
subregions_focal <- c("Suisun Marsh","Grizzly Bay","Honker Bay","Confluence","Lower Sacramento River","Sacramento River near Rio Vista")

#categorize subregions into regions useful for SMSCG
regions_new <- as.data.frame(
  cbind(
    "Region_smscg" = c("Suisun Marsh",rep("Suisun Bay",2),rep("River",3))
    ,"SubRegion" = c("Suisun Marsh","Grizzly Bay","Honker Bay","Confluence","Lower Sacramento River","Sacramento River near Rio Vista")
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
#We mostly don't need to Sacramento River near Rio Vista but it does contain STN 706 and FMWT 706
#could filter out any points east of the 706 stations which would pretty much do what we want
#consider splitting east and west marsh polygons at some point

#Filter samples spatially-----------------

#grab coordinates of eastmost station (STN 706)
zoop_706 <- zoop_geom %>% 
  filter(station=="706" & source== "STN") %>% 
  distinct(source,station,latitude,longitude) %>% 
  pull(longitude)

#filter zoop data by region
zoop_spatial_filter <- zoop_geom %>% 
  #assign samples to regions
  st_join(region_focal,join = st_within) %>% 
  #drop any samples outside of focal region
  st_filter(region_focal) %>% 
  #then drop samples east of the 706 stations
  filter(longitude < zoop_706)

#create data frame with category for DOP vs other sources
#this will be used for mapping DOP separately from rest of sources 
source_category <- as.data.frame(
  cbind(
  "source_type" = c("random",rep("fixed",3))
  ,"source" = c("DOP","EMP", "STN", "FMWT")
  )
)
#Note that EMP does include their floating stations so not purely fixed in this case

#data set with just unique samples for mapping
zoop_samp_uniq <- zoop_spatial_filter %>% 
  distinct(source,sample_id,geometry) %>% 
  #add source type
  left_join(source_category)

#create object from bounding box for the focal region
#add a buffer around points to improve map aesthetic
bbox_region <- st_bbox(st_buffer(region_focal,2000))

#map the samples 
ggplot()+
  #plot waterways base layer
  geom_sf(data= ww_delta_4326, fill= "lightblue", color= "black")+
  #reduced region
  geom_sf(data =region_focal, aes(fill=Region_smscg), alpha=0.3)+
  #plot station locations using different shapes and colors for different types of stations
  geom_sf(data= zoop_samp_uniq, aes(fill= source, shape= source, color=source),  size= 3.5)+
  #zoom in on region where stations are located using bounding box
  coord_sf( 
    xlim =c(bbox_region$xmin,bbox_region$xmax)
    ,ylim = c(bbox_region$ymin,bbox_region$ymax)
  )+
  facet_grid(rows = vars(source_type))+
  theme_bw()+
  labs(x="Longitude",y="Latitude")

#Filter zooplankton samples by month ----------------
#info needed:
#taxa considered important for fish
#BPUE for zoop taxa
#taxa present in the dataset

zoop_season <- zoop_spatial_filter %>% 
  #add column for sample month
  mutate(month = month(date)) %>% 
  filter(month >5 & month<11)

#check remaining months
unique(zoop_season$month)
#looks good

#look at all taxa remaining in data set
zoop_taxa <- zoop_season %>% 
  distinct(taxname,lifestage)
#45 combos
#should have kept some of the higher taxonomy info for filtering

#probably quickest thing to do is export this list, manually match it to the BPUE data by taxon code, and then read that back in
#write_csv(zoop_taxa,"./PowerAnalysis/taxon_list.csv")

#Calculate zooplankton BPUE from CPUE-------------

#I guess we could add back all the higher level taxonomy for the zoop samples
#then try using case when to match mass to taxon names; complicated because of life stage too


# Formatting and aggregating zoop data by region---------------
#add water year but 2018-2021 are all different water year types
#and only 2018 is a type in which we would perform the action (ie, Below Normal)
























