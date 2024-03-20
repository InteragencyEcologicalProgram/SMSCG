## Script dedicated to updating the SMSCG zoop graphs and analysis for the Summer- Fall Action Report each year

library(tidyverse)
library(readxl)
#library(lubridate)
library(ggthemes)
#library(ggplot2)
library(devtools)
library(zooper)
library(sf) #for the regional data
library(lme4) #for linear models
library(lmerTest) #for getting p values with the linear models
library(emmeans)
library(effects)
library(car)


#devtools::install_github("InteragencyEcologicalProgram/zooper") #install zooper to grab the integrated data with DOP

#Zooper Data-----

zooper = Zoopsynther(Data_type = "Community", 
                     Sources = c("EMP", "FMWT", "STN", "DOP"), 
                     Size_class = "Meso", 
                     Date_range = c("2017-06-01", NA)) %>% 
  mutate("Month" = month(Date)) %>% #make month column so we can filter only the ones we want
  filter(Month >5, Month <11) %>% 
  filter(Order == "Calanoida") %>% #only including calanoids since thats what's in our hypothesis
  mutate(Date2 = ymd(Date))%>% # change date format to same as SMSCG
  mutate(Date = Date2) %>% 
  select(Source, Station, Latitude, Longitude, Year, Date, TowType, SampleID, Taxlifestage, CPUE) 

  

#DOP does multiple tows at a station (surface and bottom), plus added a special study for oblique in 2017, so want to average the cpue of these at the same date/ station.... but first need to add the regions in so we can use the lat/ longs


####SMSCG Region designations----

#add in the stations locations/ regions for the zooper dataset before taking averages

load("Data/SMSCGRegions.RData")

na_coords = zooper %>% 
  subset(is.na(zooper$Latitude)) #need to check which coordinates have NAs, looks like its from a 2018 LSC station, which don't need anyway

zoopstations = zooper %>%
  filter(!is.na(zooper$Latitude)) %>% #remove stations with no locs
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry() %>% 
  filter(Region != "NA") %>% #remove any of the stations not included in the SMSCG footprint
  select(Source:CPUE, Region) 

#now take the station averages with the regions assigned

#Avg Zooper CPUE----

avgzoopCPUE = zoopstations %>% 
  group_by(Source, Region, Station, Date, Taxlifestage) %>% 
  summarize(meanCPUE = mean(CPUE)) %>% 
  mutate(SampleID = paste (Source, Region, Station, Date, Taxlifestage, sep = ' '))#create a sample ID for each combo so that when I combine I can eliminate duplicates

#Zooper BPUE------

#read in the biomass file 

biomass = read.csv("Data/Mesomicrotaxa Biomass.csv")

zooper_bpue = left_join(avgzoopCPUE, biomass) %>% 
  mutate(BPUE = meanCPUE*Carbon_weight_ug) %>% 
  filter(!is.na(BPUE)) %>% 
  #mutate(taxa = str_remove(Taxlifestage, "_UnID")) %>% 
  select(c(-meanCPUE, -Carbon_weight_ug, -EMP_Code,)) 
 

#Add unpubed SMSCG data-----

#read in SMSCG data which has EMP that hasn't been published yet

SMSCG_CPUE = read.csv("Data/SMSCG_CBNet_2018to2023CPUE_07Feb2024.csv") %>% 
  select(Project, Year, Date, Station, ACARTELA:CUMAC ) %>% 
  rename(Source = Project) %>% #renaming Project so that the survey matches the source column in zooper
  mutate(Date2 = mdy(Date)) %>% #change the date column to be a date format since its a character now
  select(-starts_with("ALL")) %>%  #remove the total columns
  select(-Date) %>% 
  rename(Date = Date2) #changing it so we only have the right date column


###Station locations-----

#read in station locations file for lat and long

SMSCG_CPUE_Loc = read.csv("Data/SMSCG_Station_Coords.csv") %>% 
  select(Project, Station, longitude, latitude) %>% 
  rename(Source = Project, Longitude = longitude, Latitude = latitude) #renaming so can combine down the line

#merge the two files together so have coordinates in the SMSCG file

SMSCG_CPUE_new = merge(SMSCG_CPUE, SMSCG_CPUE_Loc, by = c("Source", "Station")) 

#convert the SMSCG to long format so can convert to biomass and combine both file later

SMSCG_long = pivot_longer(SMSCG_CPUE_new, cols = (ACARTELA:CUMAC), 
                          names_to = "EMP_Code", 
                          values_to = "CPUE")

###SMSCG Regions----

unpubstations = SMSCG_long %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry() %>% 
  filter(Region != "NA") %>%  #remove any of the stations not included in the SMSCG footprint
  select(Source:Station, Date, EMP_Code, CPUE, Region)


###SMSCG biomass--------------

#convert SMSCG file to biomass via the codes in the biomass csv
#full join so have the tax life stage column so I can combine the two files

SMSCG_bpue = full_join(unpubstations, biomass, by= "EMP_Code") %>% 
  mutate(BPUE = CPUE*Carbon_weight_ug) %>% 
  filter(!is.na(BPUE)) %>% 
  group_by(Source, Region, Station, Date, Taxlifestage) %>% 
  mutate(SampleID = paste (Source, Region, Station, Date, Taxlifestage, sep = ' ')) %>% #create sample ID so can check for duplicates between zooper and this dataset
  select(-Carbon_weight_ug, -CPUE, -EMP_Code)

#check the number of rows for emp 2022 only to see how many records should be added to the zooper set

emp2022 = SMSCG_bpue %>% 
  mutate(Year = year(Date)) %>% 
  filter(Source == "EMP" & Year ==2022)


#Zooper and SMSCG files-----

#combine datasets but first need to filter out any duplicates

nodups = filter(SMSCG_bpue, !(SampleID %in% zooper_bpue$SampleID))

all_data = rbind(nodups, zooper_bpue) %>% #use rbind because just adding rows and has the same columns
  select(-SampleID) %>% #filter out for just calanoids
  #filter(Taxlifestage %in% c('Acartiella sinensis Adult', 'Acartiella sinensis Juvenile', 'Tortanus_UnID Adult', 'Tortanus_UnID 
                             #Juvenile', 'Pseudodiaptomus forbesi Juvenile' , 'Pseudodiaptomus forbesi Adult' ,
                             #'Pseudodiaptomus_UnID Larva' , 'Pseudodiaptomus_UnID Adult' , 'Pseudodiaptomus_UnID Juvenile', 
                             #'Eurytemora affinis Larva' , 'Eurytemora affinis Adult' , 'Eurytemora affinis Juvenile' , 
                             #'Eurytemora_UnID Larva', 'Sinocalanus doerrii Juvenile' , 'Sinocalanus doerrii Adult' ,'Sinocalanus
                             #doerrii Larva' , 'Sinocalanus_UnID Juvenile' , 'Acartia_UnID Adult' , 'Acartia_UnID Juvenile' ,
                             #'Diaptomidae_UnID Adult' , 'Diaptomidae_UnID Juvenile' , 'Pseudodiaptomus marinus Adult' , 
                             #'Calanoida_UnID Adult' , 'Calanoida_UnID Juvenile'))
  mutate(Year = as.factor(year(Date))) %>% #add month and year plus adding a factor
  mutate(Month = as.factor(month(Date)))
  
dups = rbind(zooper_bpue, SMSCG_bpue) %>% #checking to make the numbers match up with combining and number of dups
  group_by(SampleID) %>% 
  summarize(N= n()) %>% 
  filter(N ==2)

#numbers work out with combing two datasets and removing the duplicates

#now need to move back to wide format so can group taxa, then will need to move back to long for the rest (e.g. plots)

all_data_wide = all_data %>% 
  pivot_wider(id_cols = c(Source: Region, Year, Month), 
              names_from = Taxlifestage, 
              values_from = BPUE, 
              values_fill = 0) 

#combine taxa groups for plots
#the all data file has all zoop groups because of the SMSCG file

zoopgroups = mutate(all_data_wide, 
                        Acartiella = `Acartiella sinensis Adult` + `Acartiella sinensis Juvenile`,
                        Tortanus = `Tortanus_UnID Adult` + `Tortanus_UnID Juvenile`,
                        Pseudodiaptomus = `Pseudodiaptomus forbesi Juvenile` + `Pseudodiaptomus forbesi Adult` + 
                              `Pseudodiaptomus_UnID Larva` + `Pseudodiaptomus_UnID Adult` + `Pseudodiaptomus_UnID Juvenile`,    
                        Eurytemora = `Eurytemora affinis Larva` + `Eurytemora affinis Adult` + `Eurytemora affinis Juvenile` + 
                                    `Eurytemora_UnID Larva`,         
                        `Other Calanoids` = `Sinocalanus doerrii Juvenile` + `Sinocalanus doerrii Adult` + 
                      `Sinocalanus doerrii Larva` + `Sinocalanus_UnID Juvenile` + `Acartia_UnID Adult` + `Acartia_UnID Juvenile` 
                            +   `Diaptomidae_UnID Adult` + `Diaptomidae_UnID Juvenile` + `Pseudodiaptomus marinus Adult` + 
                                `Calanoida_UnID Adult` + `Calanoida_UnID Juvenile`) %>% 
  select(Source:Month, Acartiella, Tortanus, Pseudodiaptomus, Eurytemora, 'Other Calanoids') %>%  #remove all the other taxa files
  mutate(SampleID = paste (Source, Region, Year, Month, Station, sep = ' ')) %>%   #create a sample ID b/c things get funky with the total graph later
  pivot_longer(., cols = (c(Acartiella: Eurytemora, 'Other Calanoids')), names_to = "Taxa_Group", values_to = "BPUE")  #move back to long format for combining
  


####Plots------


#need graphs with years, not doing month for this report since the time hasn't been consistent and not doing BACI...
#still need month though for the random effect for the stats later

#File with summary stats
zoopsummary1 = group_by(zoopgroups, Region, Year, Month, Taxa_Group) %>% 
  mutate(Region = factor(Region, levels = c("Suisun Bay", "Suisun Marsh", "River"))) %>% #change levels so geographically organized from W to E
  summarize(mean_BPUE = mean(BPUE),
            n = length(BPUE),
            se_BPUE = (sd(BPUE))/n) 

zoopsummary = zoopsummary1%>%
  group_by(Region, Year, Taxa_Group) %>%
  summarize(mean_BPUE2 = mean(mean_BPUE),
            n = length(mean_BPUE),
            se_BPUE = (sd(mean_BPUE))/n) 

zoopsampsize = group_by(zoopgroups, Region, Year) %>%
  summarize(N = length(unique(SampleID)))

#Making plot
bpue1 = ggplot(zoopsummary, aes(x = Year, y = mean_BPUE2))
bpue2 = bpue1+ geom_bar(stat = "identity", aes(fill = Taxa_Group)) + 
  facet_wrap(facets= vars(Region)) + 
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("Mean BPUE (µgC/m3)") +
  geom_label(data = zoopsampsize, aes(x = Year, y = 100, label = paste(N)), size = 2.5, nudge_y = -100,
             inherit.aes = FALSE) +
  theme_few() + theme(text = element_text(family = "sans", size = 10),
                      legend.text = element_text(face = "italic"), 
                      axis.text.x = element_text(angle=45, hjust=1),
                      legend.position = "bottom") #angles x axis so the years fit

bpue2

ggsave(plot = bpue2, filename = "SMSCG_2017to2023_Zoop.tiff", device = "tiff", width = 6, height =5, units = "in", dpi = 300)
####Analysis------------------------------------

#want to do an analysis with years and regions with the total bpue----- but this is still just for calanoids only

zooptots = group_by(zoopgroups, Region, Year, Month, SampleID) %>% 
  summarize(BPUE = sum(BPUE), 
            logBPUE = (log(BPUE+1)))

zooptotsave1 = group_by(zooptots, Region, Year, Month) %>% 
  summarize(BPUEmean = mean(BPUE), 
            logBPUE = (log(BPUEmean))) %>%
  mutate(Region = factor(Region, levels = c("Suisun Bay", "Suisun Marsh", "River")))  #change levels so geographically organized from W to E

zooptotsave = zooptotsave1 %>%
  group_by(Region, Year) %>%
  summarize(BPUEmean = mean(BPUEmean))

tots1 = ggplot(zooptotsave, aes(x = Year, y = BPUEmean))
tots2 = tots1 + geom_bar(stat = "identity") +
  facet_wrap(facets= vars(Region)) + #(coord_cartesian (ylim = c(1, 15000))) +
  ylab("BPUE (µgC/m3)") +
  theme_few() + theme(text = element_text(family = "sans", size = 10),
                      legend.text = element_text(face = "italic"), 
                      axis.text.x = element_text(angle=45, hjust=1)) #angles x axis so the years fit

tots2


tots1 = ggplot(zooptotsave1, aes(x = Year, y = BPUEmean))
tots2 = tots1 + geom_bar(stat = "identity") +
  facet_wrap(facets= vars(Region)) + #(coord_cartesian (ylim = c(1, 15000))) +
  ylab("BPUE (µgC/m3)") +
  theme_few() + theme(text = element_text(family = "sans", size = 10),
                      legend.text = element_text(face = "italic"), 
                      axis.text.x = element_text(angle=45, hjust=1)) #angles x axis so the years fit

tots2


###############
#wft

#$$$$$$$$$$$$$$$$$$$$$

hist(zooptots$BPUE)
hist(zooptots$logBPUE) #skewed so need to use log bpue

#doing a linear mixed model with month as a random effect so it removes the seasonality aspect
#need to use the package lm4

zlmer = lmer(logBPUE~ Region*Year + (1|Month), data = zooptots)
summary(zlmer)
plot(zlmer)

summary(zlmer)$coefficients

#plot the output of the linear model. Need the effects package for that. This is just an effects plot

plot(allEffects(zlmer), x.var = "Year")

#this one is a prettier effects plot than using the 
effs = allEffects(zlmer)[[1]]

effs_df = data.frame(effs$x, SE = effs$se, Fit = effs$fit)

ggplot(effs_df, aes(x = Region, y = Fit)) + geom_point()+
  facet_wrap(~Year)+
  geom_errorbar(aes(ymin = Fit-SE, ymax = Fit+SE))+ theme_bw()

#could also do an anova but the aov code in R is just for a type I anova which you can only do with an equal sample size which we definitely don't have
#need to do a type III which we can do in the package car

Anova(zlmer, type = "III") #region and year signif but not the interaction

#stick with linear model since we can use a random effect for month

#the lmer above shows River and SM signifi diff, and 2017/2023 are signif diff, but thats it.
#need to do pairwise comparisons to break out exactly what it is
#can do that by region and year combos

#pairwise comparisons to look at which region and year combos are signif

foo = emmeans(zlmer, pairwise ~ "Region", by = "Year") #looking at how the regions differ to each other in the same year
emmeans(zlmer, pairwise ~ "Year", by = "Region") #how the years differ in the same region
emmeans(zlmer, pairwise ~ "Year") #just how the years differ





