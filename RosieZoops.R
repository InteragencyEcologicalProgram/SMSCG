#Play aroudn with zooplankton data

library(tidyverse)
library(lubridate)
library(zooper)
library(deltamapr)
library(sf)

library(lme4)
library(lmerTest)

# Hypotheses: 
#   1.	Decreasing X2 will maximize the area of Delta Smelt habitat with appropriate temperatures, turbidity, and salinity, which will result in higher Delta Smelt growth and survival.
# 2.	Decreasing X2 will increase biomass of calanoid copepods in the low salinity zone through increased transport of freshwater species from upstream, which will result in higher Delta Smelt growth and survival.
# 3.	Operating the SMSCGs during the summer and fall will maximize the duration and area of Delta Smelt habitat with appropriate temperatures, turbidity, and salinity that can be accomplished with 100 TAF of water, which will result in higher Delta Smelt Growth and survival.
# 4.	Operating the SMSCGs during the summer and fall will increase biomass of calanoid copepods in Suisun Marsh through increased transport of freshwater species from upstream, which will result in higher Delta Smelt Growth and survival.
# 5.	Operating the SMSGs will increase the area of appropriate Delta Smelt Habitat in Grizzly Bay.
# To address each of these hypotheses, we will rely on three primary comparisons:
#   1.	Inter-annual comparisons – We will compare constituents during 2023 to conditions in previous wet years with X2 actions (2017, 2019), previous years with SMSCG actions (2018) and dry years with no action (2020-2022).
# 2.	Regional comparisons – We expect X2 actions to improve conditions in Suisun Bay. We expect SMSCG actions to improve conditions in Suisun Marsh. Neither action will change conditions in the River, and the River will always be hotter and clearer (less ideal). 


#let's pull all the zooplankton data from Jun-October, 2017-present

SMSCGzoops = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FMWT", "STN", "DOP"),
                         Years = 2017:2022, Size_class = c("Meso", "Micro")) %>%
  filter(!Undersampled)

limno = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FMWT", "STN", "DOP"),
                                 Years = 2017:2022, Size_class = "Meso") %>%
  filter(Taxname %in% c("Limnoithona tetraspina", "Limnoithona_UnID", "Limnoithona sinensis"))

SMSCGzoopsx = bind_rows(SMSCGzoops, limno)

zoop_biomass <- read_csv("./Data/zoop_Copepod and Cladoceran Biomass Values.csv")

#filter the CPUE file so it only has the stations we're interested in 
#wait... where should i put the confluence?
Regions = filter(R_EDSM_Subregions_1718P1, SubRegion %in% c("Suisun Marsh", "Mid Suisun Bay", 
                                                            "West Suisun Bay" ,
                                                            "Grizzly Bay", "Confluence",
                                                            "Lower Sacramento River", "Honker Bay",
                                                            "Sacramento River near Rio Vista")) %>%
  mutate(Region = case_when(SubRegion %in% c("Mid Suisun Bay", "Honker Bay", "West Suisun Bay" )~ "Suisun Bay",
                     SubRegion %in% c("Lower Sacramento River", "Sacramento River near Rio Vista", "Confluence") ~ "River",
                     TRUE ~ SubRegion))

save(Regions, file = "data/SMSCGRegions.RData")


SMSCGzoops2.1 = st_as_sf(filter(SMSCGzoopsx, !is.na(Latitude)),
                         coords = c("Longitude", "Latitude"), 
                       crs = 4326) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  filter(!is.na(SubRegion)) 

ggplot()+
  geom_sf(data=Regions)+
  geom_sf(data=SMSCGzoops2.1, aes(shape = Source))


library(ggspatial)
ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightskyblue")+
  theme_bw()+
  coord_sf(ylim = c(38, 38.4), xlim = c(-122.2, -121.5))+
  annotation_north_arrow(location = "tl")+
  annotation_scale()


SMSCGzoops2 = mutate(SMSCGzoops2.1, Region = case_when(SubRegion %in% c("Mid Suisun Bay", "Honker Bay")~ "Suisun Bay",
                                                     SubRegion %in% c("Lower Sacramento River", "Sacramento River near Rio Vista") ~ "River",
                                                     TRUE ~ SubRegion),
                     Month = month(Date),
                     Taxa = case_when(!is.na(Order) ~ Order,
                                      TRUE ~ Taxname)) %>%
  filter(Month %in% c(6:10))


ggplot(SMSCGzoops2, aes(x = Year, y = CPUE, fill = Taxa))+
  geom_col()+
  scale_fill_brewer(palette = "Dark2")+
  facet_wrap(~Region)+
  theme_bw()

#Calculate total CPUE# per sample

SMSCGzoopstot = group_by(SMSCGzoops2, SampleID, Month,Region, Year, Station) %>%
  st_drop_geometry() %>%
  summarize(CPUE = sum(CPUE, na.rm =T))

#calculate average CPUE per taxa

SMSCGzoopsmean = group_by(SMSCGzoops2, SampleID, Month, Taxa, Region, Year) %>%
  st_drop_geometry() %>%
  summarize(CPUE = sum(CPUE, na.rm =T)) %>%
  group_by(Taxa, Region, Year) %>%
  summarize(CPUE = mean(CPUE, na.rm =T))

#Plot it a few different ways

ggplot(filter(SMSCGzoopsmean, Year != 2022), aes(x = Year, y = CPUE, fill = Taxa))+
  geom_col()+
  scale_fill_brewer(palette = "Dark2", labels = c("Calanoid copepods", "Barnacle larvae",
                                                  "Cladocera", "Cyclopoid copepods",
                                                  "Shrimp and crab larvae", "Harpactacoid copepods"))+
  facet_wrap(~Region)+
  theme_bw()

ggplot(filter(SMSCGzoopsmean, Year != 2022), aes(x = Region, y = CPUE, fill = Taxa))+
  geom_col()+
  scale_fill_brewer(palette = "Dark2", labels = c("Calanoid copepods", "Barnacle larvae",
                                                  "Cladocera", "Cyclopoid copepods",
                                                  "Shrimp and crab larvae", "Harpactacoid copepods"))+
  facet_wrap(~Year)+
  theme_bw()

# #now some example plots so folks know what I'm talking about
testdata = read_csv("data/Testdata.csv")

ggplot(testdata, aes(x = Year, y = CPUEm, fill = YearType))+
  geom_col(color =  "black")+
  geom_errorbar(aes(ymin = CPUEm -sdcpue, ymax = CPUEm + sdcpue))+
  facet_wrap(Scenario~Region)+
  theme_bw()+
  ylab("Example Metric")+
  scale_y_continuous(breaks = NULL)+
  scale_fill_manual(values = c("orange", "darkred", "lightblue", "palegreen", "limegreen"))+
                    scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024))+
  theme(axis.text.x = element_text(angle = 45, hjust =1))+
  xlab(NULL)

#################################################################

#convert year to factor
SMSCGzoopstot$Year = as.factor(SMSCGzoopstot$Year)

#run a mixed model with random effect of month
lm1 = lmer(log(CPUE+1)~ Year*Region + (1|Month) , data =  SMSCGzoopstot)
summary(lm1)
library(car)
Anova(lm1, type = "III")

#plot the output
library(effects)
plot(allEffects(lm1), x.var = "Region")



#pairwise comparisons
library(emmeans)
emmeans(lm1, pairwise ~ "Region", by = "Year")
emmeans(lm1, pairwise ~ "Year", by = "Region")
emmeans(lm1, pairwise ~ "Year")

#prettier effects plot
effs = allEffects(lm1)[[1]]

effs_df = data.frame(effs$x, SE = effs$se, Fit = effs$fit)

ggplot(effs_df, aes(x = Region, y = Fit)) + geom_point()+
  facet_wrap(~Year)+
  geom_errorbar(aes(ymin = Fit-SE, ymax = Fit+SE))+ theme_bw()

#############################################
#quick plot of psudodiaptomus versus salinity

SMSCGzoops = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FMWT", "STN", "DOP"),
                         Years = 2000:2022, Size_class = "Meso") %>%
  filter(!Undersampled)

SMSCGzoops2023 = read_csv("Data/SMSCG_CBNet_2018to2024CPUE_03Feb2025.csv") %>%
  filter(Year %in% c(2023, 2024)) %>%
  rename(Source = Project) %>%
  mutate(Region = case_when(Region %in% c("Suisun Bay", 
                                          "Suisun Bay (Honker)", "Honker Bay", "Suisun Bay (Mothball)", "Suisun Bay (Carquinez St.)")~ "Suisun Bay",
                            Region %in% c("Suisun Bay (Grizzly)", "Grizzly Bay", "Suisun Bay (Mont. Sl.)") ~ "Grizzly Bay",
                            Region %in% c("SuiMar", "Montezuma Slough")~ "Suisun Marsh",
                            Region %in% c("Lower Sacramento River", "Confluence")~ "River"),
         Date = mdy(Date)) %>%
  filter(!is.na(Region))


SMSCGzoops2023long = pivot_longer(SMSCGzoops2023, ACARTELA:last_col(), names_to = "taxon", values_to= "CPUE") %>%
  filter(taxon %in% c("PDIAPFOR", "PDIAPJUV")) %>%
  group_by(Source, Year, Month, Date, Station, Region) %>%
  summarize(CPUE = sum(CPUE))

load("Data/SMSCGRegions.RData")

SMSCGzoops2 = st_as_sf(filter(SMSCGzoops, !is.na(Latitude)), coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  filter(!is.na(SubRegion)) %>%
  st_drop_geometry()



SMSCGzoops2 = mutate(SMSCGzoops2, Region = case_when(SubRegion %in% c("Mid Suisun Bay", "Honker Bay", "West Suisun Bay")~ "Suisun Bay",
                                                     SubRegion %in% c("Lower Sacramento River", "Sacramento River near Rio Vista", "Confluence") ~ "River",
                                                     TRUE ~ SubRegion),
                     Month = month(Date)) %>%
  filter(Month %in% c(6:10), Species == "Pseudodiaptomus forbesi")

SMSCGzoops2sum = mutate(SMSCGzoops2, Region = case_when(SubRegion %in% c("Mid Suisun Bay", "Honker Bay", "West Suisun Bay")~ "Suisun Bay",
                                                     SubRegion %in% c("Lower Sacramento River", "Sacramento River near Rio Vista", "Confluence") ~ "River",
                                                     TRUE ~ SubRegion),
                     Month = month(Date)) %>%
  filter(Month %in% c(6:8), Species == "Pseudodiaptomus forbesi") %>%
  mutate(season = "Summer")

SMSCGzoops2f = mutate(SMSCGzoops2, Region = case_when(SubRegion %in% c("Mid Suisun Bay", "Honker Bay", "West Suisun Bay")~ "Suisun Bay",
                                                        SubRegion %in% c("Lower Sacramento River", "Sacramento River near Rio Vista", "Confluence") ~ "River",
                                                        TRUE ~ SubRegion),
                        Month = month(Date)) %>%
  filter(Month %in% c(9:10), Species == "Pseudodiaptomus forbesi") %>%
  mutate(season = "Fall")

SMSCGzoops2s = bind_rows(SMSCGzoops2sum, SMSCGzoops2f)  %>%
  filter(!is.na(season))


yrs = read_csv("data/wtryrtype.csv") %>%
  mutate(Year = WY)

SMSCGzoops3 = group_by(SMSCGzoops2s, Source, SampleID, Date, Station, Region, SalSurf, Month, Year, season) %>%
  summarize(CPUE = sum(CPUE)) %>%
bind_rows(SMSCGzoops2023long) %>%
  left_join(yrs) %>%
  mutate(Yrtype = factor(`Yr-type`, levels = c("C", "D", "BN", "AN", "W"))) %>%
  mutate(season = case_when(Month %in% c(9:10) ~ "Fall",
            Month %in% c(8:9) ~ "Summer",
            TRUE ~ season),
         Region = case_match(Region, "River" ~ "Sacramento River",
                                    .default = Region))

samples = group_by(SMSCGzoops3, Region, Year, season) %>%
  summarize(N = n()) %>%
  filter(!is.na(season))


ggplot(filter(SMSCGzoops3, !is.na(season), Year >2016), aes(x = as.factor(Year), y = log(CPUE+1), fill = Yrtype)) + geom_boxplot()+
  facet_grid(season~Region)+
  xlab("Year ")+
  ylab("P. forbesi log(CPUE+1)")+
  scale_fill_manual(values = c("darkred", "orange", "yellow", "lightgreen", "darkblue"),
                    name = "Year Type")+
  theme_bw()+
  geom_label(data = filter(samples, Year >2016), aes(x = as.factor(Year),label = N), y =1, inherit.aes = FALSE,
             label.padding = unit(0.1, "lines"), size =3)+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust =0.5),
        legend.position = "bottom") 

ggsave("Plots/Psudodiaptomus_24years.tiff", device = "tiff", width =10, height =6)


ggplot(SMSCGzoops3, aes(x = SalSurf, y = log(CPUE+1), fill = Yrtype)) + geom_point()+
  facet_grid(Month~Region)+
  xlab("Salinity")+
  ylab("Pseudodiaptomus log(CPUE+1)")+
  scale_fill_manual(values = c("darkred", "orange", "yellow", "lightgreen", "darkblue"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust =0.5))

ggplot(SMSCGzoops3, aes(x = yday(Date), y = log(CPUE+1), color = SalSurf)) + geom_point()+
  facet_wrap(~Region)+
  geom_smooth()+
  xlab("Day of Year")+
  scale_color_viridis_c()+
  ylab("Pseudodiaptomus log(CPUE+1)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust =0.5))


ggplot(SMSCGzoops3, aes(x = as.factor(Year), y = log(CPUE+1), fill = Yrtype)) + geom_boxplot()+
  facet_wrap(~Region)+
  xlab("Year ")+
  ylab("P. forbesi log(CPUE+1)")+
  scale_fill_manual(values = c("darkred", "orange", "yellow", "lightgreen", "darkblue"),
                    name = "Year Type")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust =0.5),
        legend.position = "bottom") 

################################################################################

ggplot(SMSCGzoops3, aes(x = SalSurf, y = log(CPUE+1)))+ geom_point()+
  facet_wrap(~Region)+
  geom_smooth(method = "lm")+
  theme_bw()+
  geom_vline(xintercept = 6, linetype =2, color = "darkred")+
  xlab("Salinity (psu)")+
  ylab("Log Psudodiaptoums CPUE")

# Should i be doing just forbesi? or doe s it matter?

Foo = group_by(SMSCGzoops, Species) %>%
  summarize(tot = sum(CPUE))

#######################################################################
#Pascale wants spring zooplankton

SMSCGzoops = Zoopsynther(Data_type = "Community", Sources = c("EMP", "20mm", "FMWT", "STN", "DOP"),
                         Years = 2000:2022, Size_class = "Meso") %>%
  filter(!Undersampled)


#filter the CPUE file so it only has the stations we're interested in 
Regions = filter(R_EDSM_Subregions_1718P1, SubRegion %in% c("Suisun Marsh", "Mid Suisun Bay", "Grizzly Bay",
                                                            "Lower Sacramento River", "Honker Bay", "Sacramento River near Rio Vista")) %>%
  mutate(Region = case_when(SubRegion %in% c("Mid Suisun Bay", "Honker Bay")~ "Suisun Bay",
                            SubRegion %in% c("Lower Sacramento River", "Sacramento River near Rio Vista") ~ "River",
                            TRUE ~ SubRegion))

save(Regions, file = "data/SMSCGRegions.RData")


SMSCGzoops2.1 = st_as_sf(filter(SMSCGzoops, !is.na(Latitude)), coords = c("Longitude", "Latitude"), 
                         crs = 4326) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  filter(!is.na(SubRegion)) 



load("data/Dayflow_allw2023.RData")
outflow = select(Dayflow, Year, Date, OUT) 

springzoops = SMSCGzoops2.1 %>%
  mutate(Month = month(Date)) %>%
  filter(Taxname %in% 
                       c("Pseudodiaptomus forbesi", "Pseudodiaptomus_UnID",
                         "Cladocera_UnID", "Daphnia_UnID","Daphniidae_UnID")) %>%
  st_drop_geometry()

#monthly means
sprzop = springzoops %>%
  filter(Region %in% c("Suisun Marsh", "Suisun Bay", "Grizzly Bay")) %>%
  group_by(Month, Year, Taxname) %>%
  summarize(CPUE = mean(CPUE))

#add outflow
outflowmonthly = outflow %>%
  mutate(Month = month(Date)) %>%
  group_by(Month, Year) %>%
  summarize(OUT = mean(OUT))


springzoops2 =  left_join(sprzop, outflowmonthly)

ggplot(filter(springzoops2, Month %in% c(3,4,5)), aes(x = log(OUT), y = log(CPUE), color = Taxname)) + geom_point()+
  theme_bw()+
  geom_smooth(method = "lm")



ggplot(filter(springzoops2, Month %in% c(6,7,8,9)), aes(x = log(OUT), y = log(CPUE), color = Taxname)) + geom_point()+
  theme_bw()+
  geom_smooth(method = "lm")

##################################
#spring flow versus summer pseudodiaptomus

sprzop2 = springzoops %>%
  filter(Region %in% c("Suisun Marsh", "Suisun Bay", "Grizzly Bay"), Month %in% c(6,7,8,9),
         Taxname == "Pseudodiaptomus forbesi") %>%
  group_by(Year, Taxname) %>%
  summarize(CPUEm = mean(CPUE), sdcpue = sd(CPUE), secpue = sdcpue/3)

  
outyearly = filter(outflowmonthly, Month %in% c(2,3,4,5)) %>%
  group_by(Year) %>%
  summarize(SpringOut = mean(OUT))

summerpseudo = left_join(sprzop2, outyearly) 

ggplot(summerpseudo, aes(x = log(SpringOut), y = log(CPUEm))) + geom_point()+
  geom_smooth(method = "lm")+
  geom_errorbar(aes(ymin = log(CPUEm -secpue ), ymax = log(CPUEm + secpue)))+
  theme_bw()+
  ylab("Log-Transformed Pseudodiaptomus CPUE")+
  xlab("Log-Transformed mean Feb-May Delta Outflow")
  

#OK, now spring flow versus spring aboundacne
sprzop3 = springzoops %>%
  filter(Region %in% c("Suisun Marsh", "Suisun Bay", "Grizzly Bay"), Month %in% c(2,3,4,5),
         Taxname == "Pseudodiaptomus forbesi") %>%
  group_by(Year, Taxname) %>%
  summarize(CPUEm = mean(CPUE), sdcpue = sd(CPUE), secpue = sdcpue/3) %>%
  left_join(outyearly)


ggplot(sprzop3, aes(x = log(SpringOut), y = log(CPUEm))) + geom_point()+
  geom_smooth(method = "lm")+
  geom_errorbar(aes(ymin = log(CPUEm -secpue ), ymax = log(CPUEm + secpue)))+
  theme_bw()+
  ylab("Log-Transformed Spring Pseudodiaptomus CPUE")+
  xlab("Log-Transformed mean Feb-May Delta Outflow")

psduedo = filter(springzoops, Genus == "Pseudodiaptomus")
ggplot(psduedo, aes(x = Month, y = log(CPUE+1)))+
  geom_col()+
  facet_wrap(~Lifestage)

#break out by life stage

sprzop2 = springzoops %>%
  filter(Region %in% c("Suisun Marsh", "Suisun Bay", "Grizzly Bay"), Month %in% c(6,7,8,9),
         Genus == "Pseudodiaptomus") %>%
  group_by(Year, Taxname, Lifestage) %>%
  summarize(CPUEm = mean(CPUE), sdcpue = sd(CPUE), secpue = sdcpue/3)


outyearly = filter(outflowmonthly, Month %in% c(2,3,4,5)) %>%
  group_by(Year) %>%
  summarize(SpringOut = mean(OUT))

summerpseudo = left_join(sprzop2, outyearly) 

ggplot(summerpseudo, aes(x = log(SpringOut), y = log(CPUEm))) + geom_point()+
  geom_smooth(method = "lm")+
  geom_errorbar(aes(ymin = log(CPUEm -secpue ), ymax = log(CPUEm + secpue)))+
  facet_wrap(~Lifestage)+
  theme_bw()+
  ylab("Log-Transformed Summer Pseudodiaptomus CPUE")+
  xlab("Log-Transformed mean Feb-May Delta Outflow")


sprzop3 = springzoops %>%
  filter(Region %in% c("Suisun Marsh", "Suisun Bay", "Grizzly Bay"), Month %in% c(2:5),
         Genus == "Pseudodiaptomus") %>%
  group_by(Year, Taxname, Lifestage) %>%
  summarize(CPUEm = mean(CPUE), sdcpue = sd(CPUE), secpue = sdcpue/3)


outyearly = filter(outflowmonthly, Month %in% c(2,3,4,5)) %>%
  group_by(Year) %>%
  summarize(SpringOut = mean(OUT))

summerpseudo = left_join(sprzop3, outyearly) 

ggplot(summerpseudo, aes(x = log(SpringOut), y = log(CPUEm))) + geom_point()+
  geom_smooth(method = "lm")+
  geom_errorbar(aes(ymin = log(CPUEm -secpue ), ymax = log(CPUEm + secpue)))+
  facet_wrap(~Lifestage)+
  theme_bw()+
  ylab("Log-Transformed Spring Pseudodiaptomus CPUE")+
  xlab("Log-Transformed mean Feb-May Delta Outflow")

#########################################################
#Matt wants pseudo by month, LSZ seperate

Pesudo = Zoopsynther(Data_type = "Community", Sources = c("EMP", "20mm", "FMWT", "STN", "DOP"),
                         Years = 2000:2022, Size_class = "Meso") %>%
  filter(!Undersampled, Taxname %in% c("Pseudodiaptomus forbesi", "Pseudodiaptomus_UnID"))

Pesudo = mutate(Pesudo, Salbin = case_when(SalSurf < 0.5 ~ "Fresh <0.5",
                                           SalSurf >= 0.5 & SalSurf < 6 ~ "LSZ 0.5-6",
                                           SalSurf >=6 ~ "Salty >6"),
                Month = month(Date), Year = year(Date))

Pesudo2 =Pesudo %>%
  group_by(Year, Month, Taxname, Lifestage, Salbin) %>%
  summarize(CPUEm = mean(CPUE, na.rm =T)) %>%
  filter(!is.na(Salbin))

ggplot(Pesudo2, aes(x = Month, y = CPUEm, color = Lifestage)) + geom_point()+
  geom_smooth()+
  facet_wrap(~Salbin)


####################################################################
#limnoithona
Limno = Zoopsynther(Data_type = "Community", Sources = c("EMP", "20mm", "FMWT", "STN", "DOP"),
                     Years = 2017:2023, Size_class = c("Meso", "Micro")) %>%
  filter(Taxname %in% c("Limnoithona tetraspina", "Limnoithona_UnID"))

Limno = mutate(Limno, Salbin = case_when(SalSurf < 0.5 ~ "Fresh <0.5",
                                           SalSurf >= 0.5 & SalSurf < 6 ~ "LSZ 0.5-6",
                                           SalSurf >=6 ~ "Salty >6"),
                Month = month(Date), Year = year(Date))

Limno2 =Limno %>%
  group_by(Year, Month, Taxname, Lifestage, Salbin) %>%
  summarize(CPUEm = mean(CPUE, na.rm =T), nsamples = n()) %>%
  filter(!is.na(Salbin))
ggplot(Limno2, aes(x = Month, y = CPUEm, color = Lifestage)) + geom_point()+
  geom_smooth()+
  facet_wrap(~Salbin)

#now by regions
Limno3 = filter(SMSCGzoops2.1, Taxname %in%c("Limnoithona tetraspina", "Limnoithona_UnID", "Limnoithona sinensis")) %>%
  st_drop_geometry() %>%
  mutate(Month = month(Date)) %>%
  filter(Lifestage == "Adult", Month %in% c(6:10)) %>%
  group_by(Region, Source, SampleID, Date, Year, Month) %>%
  summarize(CPUE = sum(CPUE)) %>%
  group_by(Year, Month, Region) %>%
  summarize(CPUE = mean(CPUE), N = n())


ggplot(Limno3, aes(x = Month, y = CPUE)) +
  facet_grid(Year~Region)+ geom_col(fill = "lightblue")+
  scale_y_log10()+
  geom_text(aes(label = N, y = 10))

ggplot(Limno3, aes(x = Month, y = CPUE)) +
  facet_grid(Year~Region)+ geom_col(fill = "lightblue")+
  #scale_y_log10()+
  geom_text(aes(label = N, y = 10))

ggplot(Limno3, aes(x = Year, y = CPUE)) +
  facet_grid(Month~Region)+ geom_col(fill = "lightblue")+
  scale_y_log10()+
  geom_text(aes(label = N, y = 10))

LImnoannual = Limno3 %>%
  group_by(Year, Region) %>%
  summarize(CPUE = mean(CPUE))


#############################################################
#quick QC of DOP data

DOPzoops = read_excel("Data/SupplementalDOPData.xlsx", na = "NA")

DOPzoops2 = read_excel("Data/SupplementalDOPData.xlsx", na = "NA", sheet = "MesozooplanktonAbundance") %>%
  pivot_longer(cols = c(Acartiella_sinensis_adult:last_col()), names_to = "Taxon", values_to = "CPUE")

DOPzoops2B = read_excel("Data/SupplementalDOPData.xlsx", na = "NA", sheet = "MesozooplanktonBiomass") %>%
  pivot_longer(cols = c(Acartiella_sinensis_adult:last_col()), names_to = "Taxon", values_to = "BPUE")

DOPzoop = mutate(DOPzoops, DO = as.numeric(DO_Surface),  Station = str_sub(StationID, 9, 11)) %>%
  left_join(DOPzoops2)


DOPzoopB = mutate(DOPzoops, DO = as.numeric(DO_Surface),  Station = str_sub(StationID, 9, 11)) %>%
  left_join(DOPzoops2B) %>%
  mutate(Type = "Meso")
ggplot(DOPzoop, aes(x = DateTime, y = DO, color = Station)) + geom_line()
ggplot(DOPzoop, aes(x = Temperature_Surface, y = DO, color = Station)) + geom_line()

ggplot(DOPzoop,aes(x = DateTime, y = CPUE, fill = Taxon))+geom_area(position = "fill")+
  facet_wrap(~Station, nrow =2)


DOPzoops2mac = read_excel("Data/SupplementalDOPData.xlsx", na = "NA", sheet = "MacrozooplanktonAbundance") %>%
  pivot_longer(cols = c(Americorophium_spinicorne:last_col()), names_to = "Taxon", values_to = "CPUE")

DOPmac =  mutate(DOPzoops, DO = as.numeric(DO_Surface),  Station = str_sub(StationID, 9, 11)) %>%
  left_join(DOPzoops2mac)


ggplot(DOPmac,aes(x = DateTime, y = CPUE, fill = Taxon))+geom_area()+
  facet_wrap(~Station, nrow =2)



DOPzoops2macB = read_excel("Data/SupplementalDOPData.xlsx", na = "NA", sheet = "MacrozooplanktonBiomass") %>%
  pivot_longer(cols = c(Americorophium_spinicorne:last_col()), names_to = "Taxon", values_to = "BPUE")

DOPmacB =  mutate(DOPzoops, DO = as.numeric(DO_Surface),  Station = str_sub(StationID, 9, 11)) %>%
  left_join(DOPzoops2macB) %>%
  mutate(Type = "Macro", BPUE2 = BPUE, BPUE = BPUE*1000)


ggplot(DOPmacB,aes(x = DateTime, y = BPUE, fill = Taxon))+geom_area()+
  facet_wrap(~Station, nrow =2)

Allbiomass = bind_rows(DOPmacB, DOPzoopB)


ggplot(Allbiomass,aes(x = DateTime, y = BPUE, fill = Taxon))+geom_area()+
  facet_wrap(~Station, nrow =2)
