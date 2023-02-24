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
modout <- read_csv("./PowerAnalysis/zooplanktonmodel.csv") %>% 
  clean_names() %>% 
  rename("yes_action" = smsc_gaction) %>% 
  glimpse()

#format the zoop modeling data------------
#results are divided by month and taxa
#presumably the values are estimated biomass of carbon
#combine taxa within month but for now keep months separate

#create vector of distinct taxa
mod_taxa <- modout %>% 
  distinct(taxon) %>% 
  pull(taxon)
#"acartela"   "allcopnaup" "daphnia"    "eurytem"    "limno"      "mysid"      "othcalad"   "othcaljuv"  "othclad"   
#"othcyc"     "other"      "pdiapfor"

#summarize action and no action results by month
modout_sum_mo <- modout %>%
  group_by(month) %>% 
  summarise(na_sum = sum(no_action)
            ,ya_sum = sum(yes_action)) %>% 
  #percent difference
  #mutate(
  #  num = abs(na_sum-ya_sum)
  #  ,denom = (na_sum+ya_sum)/2
  #  ,pdiff = (num/denom)*100
  #) %>% 
  #percent change
  mutate(
    num = ya_sum-na_sum
    ,denom = (na_sum+ya_sum)/2
    ,perc_change = (num/denom)*100
  )

na = 1674927.3
ya =1766962.1

pdiff = ((abs(na-ya)/((na+ya)/2))*100)

pchange = ((ya-na)/((na+ya)/2))*100

#write_csv(modout_sum_mo,"./PowerAnalysis/zoop_perc_change_check.csv")


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
  st_as_sf(coords = c(x='longitude',y='latitude'),
           crs = 4326 #EPSG code for WGS84
  )   

#Prepare shapefile for filtering data spatially--------------

#look at coordinate reference system (CRS) of regions and basemap
#EDSM 2019 Phase 3 Subregions
st_crs(R_EDSM_Subregions_19P3) #NAD83 / UTM zone 10N which is EPSG = 26910
st_crs(WW_Delta) #NAD83 which is EPSG = 4269

#change CRS of WW_Delta to EPSG = 26910
#Note: the difference is so subtle is probably doesn't even matter
WW_Delta_26910 <- st_transform(WW_Delta, crs = 26910)

#make map
(map_region_all<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data = WW_Delta_26910, fill= "skyblue3", color= "black")+
    #EDSM 2017-18 Phase 1 Strata
    geom_sf(data = R_EDSM_Subregions_19P3, aes(fill=SubRegion), alpha=0.8)+
    #add title
    ggtitle("R_EDSM_Subregions_19P3")+
    theme_bw()
)

#only keep the needed subregions

#create vector of subregions to keep
subregions_focal <- c("Suisun Marsh","Grizzly Bay","Honker Bay","Confluence","Lower Sacramento River","Sacramento River near Rio Vista")

region_focal <- R_EDSM_Subregions_19P3 %>% 
  filter(SubRegion %in% subregions_focal)

#remake map with reduced subregions  
(map_region_focal<-ggplot()+
    #CDFW Delta waterways
    geom_sf(data= WW_Delta_26910, fill= "skyblue3", color= "black")+
    #reduced region
    geom_sf(data =region_focal, aes(fill=SubRegion), alpha=0.8)+
    #add title
    ggtitle("Focal Region")+
    theme_bw()
)
#overall this matches up pretty well with where our focal stations are
#We mostly don't need to Sacramento River near Rio Vista but it does contain STN 706 and FMWT 706

#Filter and group samples by subregion-----------------

zoop_spatial_filter <- zoop_geom %>% 
  st_filter(region_focal)
#need CRS to match; easy enough to fix

#map the samples
























