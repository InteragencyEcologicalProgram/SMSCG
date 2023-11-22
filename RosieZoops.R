#Play aroudn with zooplankton data

library(tidyverse)
library(lubridate)
library(zooper)
library(deltamapr)
library(sf)

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
                         Years = 2017:2022, Size_class = "Meso") %>%
  filter(!Undersampled)

limno = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FMWT", "STN", "DOP"),
                                 Years = 2017:2022, Size_class = "Meso") %>%
  filter(Taxname %in% c("Limnoithona tetraspina", "Limnoithona_UnID", "Limnoithona sinensis"))

SMSCGzoopsx = bind_rows(SMSCGzoops, limno)

zoop_biomass <- read_csv("./Data/zoop_Copepod and Cladoceran Biomass Values.csv")

#filter the CPUE file so it only has the stations we're interested in 
Regions = filter(R_EDSM_Subregions_1617P1, SubRegion %in% c("Suisun Marsh", "Mid Suisun Bay",
                                                            "Lower Sacramento River", "Honker Bay", "Sacramento River near Rio Vista")) %>%
  mutate(Region = case_when(SubRegion %in% c("Mid Suisun Bay", "Honker Bay")~ "Suisun Bay",
                     SubRegion %in% c("Lower Sacramento River", "Sacramento River near Rio Vista") ~ "River",
                     TRUE ~ SubRegion))

save(Regions, file = "SMSCGRegions.RData")


SMSCGzoops2 = st_as_sf(filter(SMSCGzoopsx, !is.na(Latitude)), coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  filter(!is.na(SubRegion)) 

ggplot()+
  geom_sf(data=Regions)+
  geom_sf(data=SMSCGzoops2, aes(shape = Source))

SMSCGzoops2 = mutate(SMSCGzoops2, Region = case_when(SubRegion %in% c("Mid Suisun Bay", "Honker Bay")~ "Suisun Bay",
                                                     SubRegion %in% c("Lower Sacramento River", "Sacramento River near Rio Vista") ~ "River",
                                                     TRUE ~ SubRegion),
                     Month = month(Date),
                     Taxa = case_when(!is.na(Order) ~ Order,
                                      TRUE ~ Taxname)) %>%
  filter(Month %in% c(6:10))


ggplot(filter(SMSCGzoops2, Year != 2022), aes(x = Year, y = CPUE, fill = Taxa))+
  geom_col()+
  scale_fill_brewer(palette = "Dark2")+
  facet_wrap(~Region)+
  theme_bw()

#avergaes

SMSCGzoopsmean = group_by(SMSCGzoops2, SampleID, Month, Taxa, Region, Year) %>%
  summarize(CPUE = sum(CPUE, na.rm =T)) %>%
  group_by(Taxa, Region, Year) %>%
  summarize(CPUE = mean(CPUE, na.rm =T))


ggplot(filter(SMSCGzoopsmean, Year != 2022), aes(x = Year, y = CPUE, fill = Taxa))+
  geom_col()+
  scale_fill_brewer(palette = "Dark2", labels = c("Calanoid copepods", "Barnacle larvae",
                                                  "Cladocera", "Cyclopoid copepods",
                                                  "Shrimp and crab larvae", "Harpactacoid copepods"))+
  facet_wrap(~Region)+
  theme_bw()

#now some example plots so folks know what I'm talking about
testdata = read_csv("data/Testdata.csv")

ggplot(testdata, aes(x = Year, y = CPUEm, fill = YearType))+
  geom_col()+
  geom_errorbar(aes(ymin = CPUEm -sdcpue, ymax = CPUEm + sdcpue))+
  facet_wrap(Scenario~Region)+
  theme_bw()+
  ylab("Example metric")+
  scale_y_continuous(breaks = NULL)


library(lme4)
library(lmerTest)

lm1 = lmer(Response ~ Region*Year + (1|DOY) +(1|Station), data = df)