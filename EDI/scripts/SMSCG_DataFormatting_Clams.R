## This is a script to bring together field data and clam measuring data from Suisun Marsh surveys, along with live sort data from EMP, known filtration rates from literature and station data, to create density, biomass, and grazing data for each station visit (and also site-averaged data).  

## input is in long form, but only with occurrence data numbers 
## output will be in wide form, with all zeros in all rows.  

## dataflow is: 

## 1) input tables: station list, field data, clam measuring data, live sort data, filtration rates 
## 2) Create length:biomass regresssion and extract slopes and intercepts for each species*month*site combination, and extract coefficients
## 3) Add regression coefficients (step 2) to clam measuring data to get biomass per size class
## 4) Add temperature, filtration, and density to get filtration and grazing per size class
## 5) Output = a) summarized table of biomasses, grazing, and filtration rate, w/ species (Corbicula, Potamocorbula, and totals) turned into wide format ready for R or GIS analysis, and b) rearranged and renamed for EDI
## 6) Average the values for GIS mapping.

library(tidyverse)
library(readxl) # lets us read excels directly, instead of needing to generate CSVs.
library(dplyr)## lets us do the group_by
library(broom) ## the tidy(model) if we need it 
library(data.table) ## allows fwrite function
getwd()


##########################
# Step 1
##########################
## input tables

## station list
stations = read_excel('Data/SMSCG_station_list.xlsx', 
                        sheet = "Station list", 
                        col_types = c("text", "text","text","text", "numeric", "numeric", "text", "text")) 
# Input columns = done_all_years	site 	location	comments	north_decimal_degrees	west_decimal_degrees	habitat	habitat_type
str(stations)

## field data
field = read_excel('Data/SMSCG Field data 18-21.xlsx', 
                      sheet = "compiled field data") 
field$depth_m <- (field$depth_ft*.3048)
# Input columns = done_all_years	site 	location	depth_ft_raw date	time	tide	sed_description	orgmatter	clay	silt	mica	fine_sand	coarse_sand	gravel	fines	wtemp	SC	pH	chla	turb	DO	biota	comments	sediment	habitat	habitat_type	year	month	depth_ft		
#note that depth_ft corrects for missing depth_ft by pre-calculating average of all other depths taken at that site.
str(field)

## filtration rates - note that for Potamocorbula, for temps <15C, filtration rate = 270 L/g AFDM/day
filt = read_excel('Data/Clam lookup tables - siphons and filtration rates.xlsx', 
                   sheet = "filtration rate") 
# Input columns = species	wtemp	filtration_rate
str(filt)

## siphon diameters
siph = read_excel('Data/Clam lookup tables - siphons and filtration rates.xlsx', 
                  sheet = "siphon diameter") 
# Input columns = species	length_mm	Do_mm
str(siph)

# SMSCG clam measurements
sizes = read_excel('Data/SMSCG clam size measurements.xlsx', 
                  sheet = "clam measurements")
# Input columns = year	month	entry_order	site	location	grab	date	species	individuals	size_class	comments
sizes$length_mm <-(sizes$size_class)+0.5 #add 0.5 to make it the middle of the size bin for easier reference and calculation later
str(sizes)
#view(sizes)

## live sort data (all sites, all times) 
#This is all live sort data ever, from EMP, GRTS, etc.
#Before importing, duplicate columns from compilation "Total animal weight per individual" and "average weight per individual" were removed

biomass_in = read_excel('Data/SMSCG subset of live sort data, 2018-2021.xlsx', 
                        sheet = "formatted for R", 
                        col_types = c("text", "text","date", "text", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric","text", "text",  "text", "text", "text",  "text", "numeric", "text")) 
# Input columns = tray,	station,	date,	species,	no_orgs,	size_class,	pan_mass,	dry_mass_incl_pan,	afdm_incl_pan,	total_afdm,	afdm_per_org,	comments,	Total animal weight,	Average weight per individual,	QC_flag,	QC_exclude,	Check_datasheets,	Year,	Month
str(biomass_in) #summarizes data in console

##########################
# Step 2
##########################

## Create length:biomass regresssion and extract slopes and intercepts for each species*month*site combination, and extract coefficients


##Wrangling biomass into better shape:

biomass1 = biomass_in %>% 
  select(2:6,10:12,15:16,18:19) #only keeping wanted columns 
str(biomass1)
biomass<-subset(biomass1, is.na(biomass1$"QC_exclude"))   ##Remove "QC_exclude" lines that =y; we only keep those that were blanks in original data and loaded as NA here. 
str(biomass)
 
## create new variables for loglength:logbiomass regression

biomass$log_afdm<-log10(biomass$afdm_per_org) # some values will be NAs or infinites; because they're negative; omit them in the further analysis
biomass$length_mm <-(biomass$size_class)+0.5 #add 0.5 to make it the middle of the size bin
biomass$log_length<-log10(biomass$length_mm)
str(biomass)
# view(biomass)
# levels(biomass$species)

modelmass = biomass %>%  #new DF 
filter(size_class > 0)%>% ## Remove all size zero clams since they tend to throw off the data (this also removes everything in the "No clams" level of species)
  filter (afdm_per_org > 0) # removes the NA lines of log_afdm and also the -Inf lines, where afdm_per_org =0
str(modelmass)

## Creating new dataset that summarises the regression coefficients for each station*species*month combination

modelmass2 <- modelmass%>%  #includes year as variable
  group_by(station, month, year, species) %>% 
  filter(n()>4)%>%  #this removes groups with less than 5 rows, since those are not creating good regressions anyway
  summarise(number = n(),
            intercept= summary(lm(log_afdm ~ log_length, na.action=na.omit))$coefficients["(Intercept)", "Estimate"], # what coefficient to pull out
            slope= summary(lm(log_afdm ~ log_length, na.action=na.omit))$coefficients["log_length", "Estimate"])%>%
  ungroup()

str(modelmass2)
view(modelmass2)

# modelmass3 <- modelmass%>%
#   group_by(station, month, species) %>% ## grouping all years together, as a comparison
#   filter(n()>9)%>%  #this removes groups with less than 5 rows, since those are not creating good regressions anyway
#   summarise(number = n(),
#             intercept= summary(lm(log_afdm ~ log_length))$coefficients["(Intercept)", "Estimate"], # what coefficient to pull out
#             slope= summary(lm(log_afdm ~ log_length))$coefficients["log_length", "Estimate"])
# str(modelmass3)
# view(modelmass3)


fwrite(modelmass2,"Products/SMSCG clam length-biomass regressions, 2018-2021.csv", row.names=FALSE)    

##########################
# Step 3
########################## 

##  Add regression coefficients (step 2) to clam measuring data to get biomass per size class

## identify D7 as reference site for Potamocorbula regression, and D4 as site for Corbicula regression

sizes$refsite <- ifelse(sizes$species=="Potamocorbula amurensis", "D7", 
                          ifelse(sizes$species== "Corbicula fluminea", "D4", NA))
str(sizes)  
#view(sizes)

sizes2 <- left_join(sizes, modelmass2, by=c("refsite" ="station", "species", "month", "year"))%>%
  select(1:12,14:15) #only keeping wanted columns - slopes and intercepts of length:biomass
str(sizes2) 

sizes2$afdm_perindiv<-10^(log10(sizes2$length_mm)*sizes2$slope +(sizes2$intercept)) ## this is the biomass of each individual clam in that size class
str(sizes2)


##########################
# Step 4
########################## 
## Add temperature, filtration, and siphon distances 

## Add water temp from field data
sizes3  <- left_join(sizes2, field, by=c("site", "month", "year"))%>%
  select(1:15,31) #only adding water temp right now from field data; can add others later for fuller analysis
## round water temp for matching filtration constants
sizes3$roundtemp = case_when(sizes3$species == "Potamocorbula amurensis"& between(sizes3$wtemp,14.5, 15.001) ~ 14,
                        sizes3$species == "Potamocorbula amurensis"& between(sizes3$wtemp,15, 15.5) ~ 15,
                        TRUE ~ round(sizes3$wtemp))
str(sizes3)

## Add filtration constants by temp and species
sizes4 <- left_join(sizes3, filt, by=c("species", "roundtemp"="wtemp"))
str(sizes4)

## Add do (distance between siphon pairs based on clam size) - note need to still find weighted averages of Do per sampling grab
sizes5 <- left_join(sizes4, siph, by=c("species", "length_mm"))
str(sizes5)

##########################
# Step 5
########################## 
##Part a) 

## Calculate total clams, density, weighted average siphon diameter, filtration rates, and grazing rates
## and also sum them per grab

## Grazing rate = filtration rate (L/g afdm/day) * biomass (g afdm/m2) * proportion of time spent feeding * (1-nmax)
## nmax = refiltration proportion = 3.0/(s/do)
## s = clam spacing factor =100/(square root of density (individuals/m2))
## do = siphon diameter, based on median clam size (approximated here by the weighted average of siphon diameters of all clam sizes in a grab; Jan Thompson uses median but that's a lot more work to figure out and approximately the same.

clams_by_grab<- sizes5%>%
  group_by(site, month, year, species, grab)%>%
  summarise(tot_clams_grab = sum(individuals),
            do_avg_grab = sum((individuals*Do_mm)/tot_clams_grab), ## weighted averages of siphon diameters for each grab
            density_m2_grab=(tot_clams_grab/0.052), ## density by m2
            afdm_g_m2_grab=((sum(afdm_perindiv * individuals))/0.052), ## afdm by m2
            filt_m3_m2_grab=(((sum(afdm_perindiv * individuals * filtration_rate))/0.052)*.001), # maximum filtration by m2, multiply by 0.001 to convert from L to m3
            graze_m3_m2_grab= filt_m3_m2_grab*1*(1-(3/((100/sqrt(density_m2_grab))/do_avg_grab))) 
  )%>%
  ungroup()
str(clams_by_grab) 
head(clams_by_grab)
view(clams_by_grab)

# And now average multiple grabs per event

clams_by_event<- clams_by_grab%>%
  group_by(site, month,year, species)%>%
  summarise(density_m2=mean(density_m2_grab), ## density by m2
            afdm_g_m2=mean(afdm_g_m2_grab), ## afdm by m2
            filt_m3_m2=mean(filt_m3_m2_grab), #maximum filtration by m2
            graze_m3_m2 = mean(graze_m3_m2_grab)
  )%>%
  ungroup()
str(clams_by_event)
head(clams_by_event)
# view(clams_by_event)

clam_wide = clams_by_event%>% #specify the data set you want to pivot
  pivot_wider(names_from = species, #give the name of the new column you want to use for the names of the new columns
              values_from = density_m2:graze_m3_m2, #names of the column for the values
              values_fill = 0)%>% # replace NA with 0
  select(1:5,7:8,10:11,13:14) #remove No Clams categories; now captured by 0 data in both Potamocorbula and Corbicula (and in RV totals, next step)
str(clam_wide)


## create totals of response variables (Corbicula +Potamocorbula ) and grazing turnovers
clam_wide$Total_AFDM_g_per_m2 <- 
  clam_wide$`afdm_g_m2_Corbicula fluminea` + clam_wide$`afdm_g_m2_Potamocorbula amurensis`
clam_wide$Total_density_per_m2 <- 
  clam_wide$`density_m2_Corbicula fluminea`+ clam_wide$`density_m2_Potamocorbula amurensis`
clam_wide$Total_filtration_rate_m3_per_m2_per_day <- 
  clam_wide$`filt_m3_m2_Corbicula fluminea` +clam_wide$`filt_m3_m2_Potamocorbula amurensis`
clam_wide$Total_grazing_rate_m3_per_m2_per_day <- 
  clam_wide$`graze_m3_m2_Corbicula fluminea`+clam_wide$`graze_m3_m2_Potamocorbula amurensis`

  
view(clam_wide)
str(clam_wide)

## Getting data ready for analysis, step 1: adding data from station and field 

clam_wide2 <-left_join( field, clam_wide, by=c("site", "month", "year"))%>%
  select(2,5,9:22,25,28:43)
str(clam_wide2)
clam_wide3<-left_join(clam_wide2, stations, by="site")%>%
  select(1:35,37:40)

str(clam_wide3)

#Getting data ready for analysis, setp 3: create turnover variables
clam_wide3$Corbicula_grazing_turnover_per_day <-
  clam_wide3$`graze_m3_m2_Corbicula fluminea`/clam_wide3$depth_m
clam_wide3$Potamocorbula_grazing_turnover_per_day <-
  clam_wide3$`graze_m3_m2_Potamocorbula amurensis`/clam_wide3$depth_m
clam_wide3$Total_grazing_turnover_per_day <-
  clam_wide3$Total_grazing_rate_m3_per_m2_per_day/clam_wide3$depth_m

str(clam_wide3)

## Final EDI renaming

clam_wide3 = 
  rename(clam_wide3,
         Year = year,
         Month = month,
         Station = site,
         Station_alias = location,
         Date = date,
         North_decimal_degrees = north_decimal_degrees ,
         West_decimal_degrees = west_decimal_degrees    ,
         Corbicula_AFDM_g_per_m2 = 'afdm_g_m2_Corbicula fluminea',
         Corbicula_density_per_m2 = 'density_m2_Corbicula fluminea',
         Corbicula_filtration_rate_m3_per_m2_per_day = 'filt_m3_m2_Corbicula fluminea',
         Corbicula_grazing_rate_m3_per_m2_per_day = 'graze_m3_m2_Corbicula fluminea',
         Potamocorbula_AFDM_g_per_m2 = 'afdm_g_m2_Potamocorbula amurensis',
         Potamocorbula_density_per_m2 = 'density_m2_Potamocorbula amurensis',
         Potamocorbula_filtration_rate_m3_per_m2_per_day = 'filt_m3_m2_Potamocorbula amurensis',
         Potamocorbula_grazing_rate_m3_per_m2_per_day = 'graze_m3_m2_Potamocorbula amurensis',
         Total_AFDM_g_per_m2 = ,
         Total_density_per_m2 = ,
         Total_filtration_rate_m3_per_m2_per_day = ,
         Total_grazing_rate_m3_per_m2_per_day = ,
         Corbicula_grazing_turnover_per_day = ,
         Potamocorbula_grazing_turnover_per_day = ,
         Total_grazing_turnover_per_day = ,
         Depth_ft = depth_ft,
         Depth_m = depth_m,
         Percent_Organic_Matter = orgmatter,
         Percent_Clay = clay,
         Percent_Silt = silt,
         Percent_Mica = mica,
         Percent_Fine_Sand = fine_sand,
         Percent_Coarse_Sand = coarse_sand,
         Percent_Gravel = gravel,
         Percent_Fines = fines,
         Water_temperature_C = wtemp,
         Specific_conductivity_uS_per_cm = SC,
         pH = pH,
         Chlorophyll_a_ug_per_L = chla,
         Turbidity_NTU = turb,
         Dissolved_oxygen_mg_per_L = DO,
         Sediment = sediment,
         Habitat = habitat,
         Habitat_type = habitat_type,
         Done_all_years = done_all_years
  )
## And final EDI rearranging and changing column types for EDI
clam_wide4 <- select(clam_wide3,
                    Year,
                    Month,
                    Station,
                    Station_alias,
                    Date,
                    North_decimal_degrees,
                    West_decimal_degrees,
                    Corbicula_AFDM_g_per_m2,
                    Corbicula_density_per_m2,
                    Corbicula_filtration_rate_m3_per_m2_per_day,
                    Corbicula_grazing_rate_m3_per_m2_per_day,
                    Potamocorbula_AFDM_g_per_m2,
                    Potamocorbula_density_per_m2,
                    Potamocorbula_filtration_rate_m3_per_m2_per_day,
                    Potamocorbula_grazing_rate_m3_per_m2_per_day,
                    Total_AFDM_g_per_m2,
                    Total_density_per_m2,
                    Total_filtration_rate_m3_per_m2_per_day,
                    Total_grazing_rate_m3_per_m2_per_day,
                    Corbicula_grazing_turnover_per_day,
                    Potamocorbula_grazing_turnover_per_day,
                    Total_grazing_turnover_per_day,
                    Depth_ft,
                    Depth_m,
                    Percent_Organic_Matter,
                    Percent_Clay,
                    Percent_Silt,
                    Percent_Mica,
                    Percent_Fine_Sand,
                    Percent_Coarse_Sand,
                    Percent_Gravel,
                    Percent_Fines,
                    Water_temperature_C,
                    Specific_conductivity_uS_per_cm,
                    pH,
                    Chlorophyll_a_ug_per_L,
                    Turbidity_NTU,
                    Dissolved_oxygen_mg_per_L,
                    Sediment,
                    Habitat,
                    Habitat_type,
                    Done_all_years
)


str(clam_wide4)

fwrite(clam_wide4,"Products/SMSCG_clam_EDI_2018_2021.csv", row.names=FALSE)

###########################################################################

## 6) Average the 2018-2022 values (clam and enviromnent) for GIS mapping

clam_wide4 = read.csv("Products/SMSCG_clam_EDI_2018_2021.csv")
str(clam_wide4)

clams_avg<- clam_wide4%>%
  filter (Done_all_years =="yes") %>%
  group_by(Station, Station_alias, North_decimal_degrees, West_decimal_degrees, Habitat, Habitat_type) %>%
  summarise(across(where(is.numeric), list(mean=mean), na.rm=TRUE))%>%
  select(1:6,8:38) %>%
ungroup()

str(clams_avg)
head(clams_avg)

fwrite(clams_avg,"Products/SMSCG_clam_EDI_2018_2021_AVG.csv", row.names=FALSE)
