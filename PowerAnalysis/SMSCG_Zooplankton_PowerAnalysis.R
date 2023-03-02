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

#zoop mass from SMSCG GitHub repo
#zoop_bpue_git <- read_csv("./Data/zoop_Copepod and Cladoceran Biomass Values.csv") %>% 
 # clean_names()

#zoop mass from EDI
#zoop_bpue_edi <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.539.3&entityid=e4dc2a5bedb35f15d13b4ca04a46024d") %>% 
 # clean_names()

#zoop mass from Rosie
#this was the easiest way to match the taxa to the mass
zoop_bpue_rosie <- read_csv("./PowerAnalysis/Mesomicrotaxa.csv") %>% 
  clean_names()

#sacramento valley water year types from drought synthesis
#data originates from http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
water_year <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/DroughtSynthesis/main/data/yearassignments.csv") %>% 
  clean_names() %>% 
  arrange(year)

#format water year------------------

wy <- water_year %>% 
  select(year,yr_type) %>% 
  glimpse()

#format the zoop modeling data------------
#results are divided by month and taxa
#biomass is in mg/m^3 and probably total biomass instead of mass of carbon
#eg, 3.333 means 3,333 ug of biomass per cubic meter

#create vector of distinct taxa
mod_taxa <- modout %>% 
  distinct(prey) %>% 
  pull(prey)
#"limno"      "othcaljuv"  "pdiapjuv"   "othcalad"   "acartela"   "othclad"    "allcopnaup" "daphnia"    "othcyc"    
# "other"      "eurytem"    "pdiapfor" 

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

#calculate mean for action and no action years across months
#decided to drop June; in model they are the same and in field data I dropped this month
modout_mean <- modout %>% 
  #sum BPUE across taxa
  group_by(month,scenario) %>% 
  summarise(bpue = sum(bpue3),.groups = "drop") %>%  
  #filter out June
  filter(month>6) %>% 
  #mean of monthly BPUE
  group_by(scenario) %>% 
  summarize(bpue_mean_mg = mean(bpue),.groups = "drop") %>% 
  mutate(bpue_mean_ug = bpue_mean_mg*1000)
#these values are an order of magnitude higher than the mean estimates I got from field data
#field data for Marsh for 2017-2021: 4,233 ug/m3
#presumably the models used total mass instead of carbon mass

#look at range of percent change
range(modout_sum_mo$perc_change)
#0.00000 86.85506

#summarize action and no action results across months
modout_sum_mo <- modout %>%
  group_by(scenario) %>% 
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

#get data from zooper package-----------------------

#get the data from DOP, STN, EMP, and FMWT
zoop_grab <- Zoopsynther(Data_type = "Community"
                         ,Sources=c("EMP","DOP","STN","FMWT")
                         #all the SMSCG monitoring is focused on mesozooplankton so we'll stick to that
                         ,Size_class = "Meso"
                         #DOP started sampling program in 2017 so start there
                         #DFW didn't start sampling Eastern Montezuma Slough until 2018
                         ,Date_range = c("2017-06-01",NA)
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
         ,phylum:species
         ,taxname
         ,lifestage
         ,taxlifestage
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
  distinct(taxname,lifestage,taxlifestage)
#45 combos

#Match taxa with their estimated mass--------------

#try matching taxa with Rosie's mass
zoop_mass <- zoop_taxa %>% 
  left_join(zoop_bpue_rosie, by="taxlifestage") %>% 
  arrange(carbon_weight_ug,taxlifestage) %>% 
  #all but three matched
  #Daphniidae_UnID Adult (4), Acanthocyclops_UnID Adult (3.36), Acartiella sinensis Juvenile (1.16)
  mutate(c_mass_ug = case_when(taxlifestage=="Daphniidae_UnID Adult" ~ 4 
                                 ,taxlifestage=="Acanthocyclops_UnID Adult" ~ 3.36
                               ,taxlifestage=="Acartiella sinensis Juvenile" ~ 1.16
                               ,TRUE ~ carbon_weight_ug
                               )) %>% 
  #drop unneeded column
  select(-carbon_weight_ug) %>% 
  glimpse()


#Calculate zooplankton BPUE from CPUE-------------
#did not filter out undersampled taxa

zoop_bpue_all <- left_join(zoop_season,zoop_mass) %>% 
  left_join(wy) %>% 
  #drop taxa we don't need 
  filter(phylum!="Rotifera" & phylum!="Euarthropoda" & taxname!="Decapoda_UnID")  %>% 
  #add BPUE
  mutate(bpue = cpue*c_mass_ug) %>% 
  #sum BPUE by sample
  group_by(source,station,sample_id,month,year,yr_type,Region_smscg) %>% 
  summarize(sample_bpue = sum(bpue), .groups = 'drop') %>% 
  #rename columns
  rename(region = Region_smscg, bpue = sample_bpue) %>% 
  #drop geometry
  st_set_geometry(NULL) %>% 
  #make some columns factors
  mutate(
    region = as.factor(region)
    ,month = as.factor(month)
    ,year = as.factor(year)
  ) %>% 
  glimpse()



#look at summary stats for subsets of the Marsh data---------------
#the two wet years 2017 and 2019 have a lot of variation, especially in June
#just drop June
#probably should do SD calculations with two approaches
#min SD: drop wet years 
#max SD: all years

#all years: how many samples per year and month have been collected
n_marsh_all_ym <- zoop_bpue_all %>% 
  filter(region=="Suisun Marsh")  %>% 
  group_by(year,month) %>% 
  summarize(n = n(),.groups = "drop" ) %>% 
  arrange(year,month)

#plot n by month and year
(p_marsh_ym_n <- ggplot(n_marsh_all_ym,aes(month,n))+
    geom_bar(stat = "identity") +
    facet_wrap(.~year)+
    labs(x="Year", y="zooplankton samples")+
    ggtitle("Suisun Marsh")
)
#sampling is relatively low during 2017 and 2018
#mostly because DOP only samples in Oct during those years

#all years: how many samples per year and survey have been collected
n_marsh_all_ys <- zoop_bpue_all %>% 
  filter(region=="Suisun Marsh" )  %>% 
  group_by(year,source) %>% 
  summarize(n = n(),.groups = "drop" ) %>% 
  arrange(year,source)

#all years: how many samples per year (exclude June)
n_marsh_all_y <- zoop_bpue_all %>% 
  filter(region=="Suisun Marsh"& month!="6")  %>% 
  group_by(year) %>% 
  summarize(n = n(),.groups = "drop" ) %>% 
  arrange(year)
#178-182 samples per year for 2019-2021 (including June)
#140-147 exluding June

#plot n by year
(p_marsh_ym_n <- ggplot(n_marsh_all,aes(year,n))+
    geom_bar(stat = "identity") +
    labs(x="Year", y="zooplankton samples")+
    ggtitle("Suisun Marsh")
)

#SD for the marsh across all months (July-Oct) and for 2019-2021
#2019 adds a lot to SD because it was a wet year
#represents high SD option
sd_marsh_tot <- zoop_bpue_all %>% 
  filter(region=="Suisun Marsh" & month!="6" & year!="2017" & year!="2018")  %>% 
  summarize(
    mean = mean(bpue)
    ,sd = sd(bpue)
    ,n = n()
  )
#SD = 5333

#SD for the marsh for just 2020 and 2021
#sampling low in 2017 and 2018
#2017 and 2018 had low sampling
#2017 and 2019 were wet years with lots of variation
#2018 was an action year
#represents low SD option
sd_marsh_tot_min <- zoop_bpue_all %>% 
  filter(region=="Suisun Marsh" & (year=="2020" | year=="2021") & month!="6") %>%  
  summarize(
    mean = mean(bpue)
    ,sd = sd(bpue)
    ,n=n()
  )
#SD = 3773

#SD for the marsh across all months by year
sd_marsh_year <- zoop_bpue_all %>% 
  filter(region=="Suisun Marsh" & month!="6") %>% 
  group_by(year,yr_type) %>% 
  summarize(
    mean = mean(bpue)
    ,sd = sd(bpue)
    , .groups = 'drop'
  )

#plot mean and SD for all months and years
(p_marsh_yr <- ggplot(sd_marsh_year,aes(year,mean))+
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width = 0.2) +
  labs(x="Year", y="zooplankton BPUE (µgC/m3)")+
  ggtitle("Suisun Marsh")
)

#SD for the marsh by months and year
sd_marsh_ym <- zoop_bpue_all %>% 
  filter(region=="Suisun Marsh") %>% 
  group_by(year,month) %>% 
  summarize(
    mean = mean(bpue)
    ,sd = sd(bpue)
    , .groups = 'drop'
  )

#plot mean and SD
(p_marsh_ym <- ggplot(sd_marsh_ym,aes(month,mean))+
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width = 0.2) +
    facet_wrap(.~year)+
    labs(x="Year", y="zooplankton BPUE (µgC/m3)")+
    ggtitle("Suisun Marsh")
)

#histograms of SD and mean
hist(sd_marsh_ym$mean)
hist(sd_marsh_ym$sd)


#Power analysis calculations ------------------------
#estimate how many samples needed
#estimate power with current sampling

#modelling results means 
#NOTE: values are an order of magnitude higher than what I get from 
#the field data
#maybe because these are likely total mass instead of carbon mass
mod_mean <- modout_mean %>% pull(bpue_mean_ug)

#no action mean
mod_mean_no <- mod_mean[1]

#action mean
mod_mean_yes<-mod_mean[2]

#difference in means
mod_mean_diff <- mod_mean_yes - mod_mean_no

#percent difference in means
mod_mean_diff <-mod_mean_diff / ((mod_mean_yes + mod_mean_no)/2)
#zoop with action is %59 higher than without action

#Field data means 
#use mean for all three years as no action mean (2019-2021)
#then use the 59% difference to estimate the action year mean
field_mean_all_no <- 4407
field_mean_all_yes <-8080
#mod_mean_diff2 <- (field_mean_all_yes-field_mean_all_no) / ((field_mean_all_yes+field_mean_all_no)/2)

#Field data means 
#use mean for selected years as no action mean (2020-2021)
#then use the 59% difference to estimate the action year mean
field_mean_sel_no <- 4023
field_mean_sel_yes <-7390

#field data means
#use just the 2019 wet year as high end estimate
field_mean_wet_no <- 5174
field_mean_wet_yes <- 11150

#SD for all years (2017-2021)
sd_all <- sd_marsh_tot %>% pull(sd)

#SD excluding June and excludes wet years (2017, 2019)
sd_sel <- sd_marsh_tot_min %>% pull(sd)

#SD for 2019
sd_wet <-sd_marsh_year %>% 
  filter(year=="2019") %>% 
  pull(sd)

#NOTE: there is an r package called 'effectsize' for calculating different 
#types of effect sizes 

#calculate Cohen's d based on Rosie's modeling results
#assumes similar SD and same sample size
#SD is almost certainly not similar between action and no action years
#sd_pooled <- sqrt((sd(A)^2 + sd(B)^2)/2)
#Cohens_d <- abs((mean(A) - mean(B)))/sd_pooled

#Glass' delta is used if SD are different between groups
#use SD of control if applicable
#if no control group, calculate Glass' delta twice
#use the two different SD in the the two different calculations

#effect size using means and SD for 2020-2021
#this is the low SD option
glass_d_low_sd <- abs(field_mean_sel_yes - field_mean_sel_no)/sd_sel 
#0.89 which is high effect size

#effect size using means and SD for 2019-2021
#this is hte high SD option
glass_d_high_sd <- abs(field_mean_all_yes - field_mean_all_no)/sd_all
#0.69

#effect size using means and SD for 2019
#this is the highest SD option
glass_d_highest_sd <- abs(field_mean_wet_yes - field_mean_wet_no)/sd_wet
#0.80; actually effect size is between other two so don't need to try this
#in power analysis

#power analysis with effect size using low SD option
pwr.t.test(power = 0.8 #general recommendation is 0.8-0.9
           ,d= glass_d_low_sd #effect size
           ,sig.level=.05 #standard alpha
           ,type="two.sample"
           ,alternative="two.sided"
           )
#21 samples per group

#power analysis with effect size using high SD option
pwr.t.test(power = 0.8 #general recommendation is 0.8-0.9
           ,d= glass_d_high_sd #effect size
           ,sig.level=.05 #standard alpha
           ,type="two.sample"
           ,alternative="two.sided"
)
#34 samples per group

#power of detecting effect with our current sampling effort
#low SD
pwr.t.test(n = 140 #mean annual sample size for July - Oct during 2019-2021
           ,d= glass_d_low_sd #effect size
           ,sig.level=.05 #standard alpha
           ,type="two.sample"
           ,alternative="two.sided"
)
#power = 1

#power of detecting effect with our current sampling effort
#high SD
pwr.t.test(n = 140 #mean annual sample size for July - Oct during 2019-2021
          ,d= glass_d_high_sd #effect size
           ,sig.level=.05 #standard alpha
           ,type="two.sample"
           ,alternative="two.sided"
)
#power = 0.9999974

#how small of an effect size can we detect with current sampling
pwr.t.test(n = 140 #mean annual sample size for July - Oct during 2019-2021
           ,power = 0.8 #standard value
           ,sig.level=.05 #standard alpha
           ,type="two.sample"
           ,alternative="two.sided"
)
#d = 0.336015 which is fairly small





















