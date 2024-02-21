## Script dedicated to updating the SMSCG zoop graphs and analysis for the Summer- Fall Action Report each year

library(tidyverse)
library(readxl)
library(ggthemes)
library(ggplot2)

#read in CPUE data from the ftp site. This also includes EMP data for the specified stations in the SMSCG footprint
#found path to data by right clicking file in the project file, saying import dataset, and copying path

SMSCG_CPUE = read.csv("D:/Data/Suisun Marsh Salinity Gate Project/2021 Report/SMSCG Zoop 2021 Report/Data/SMSCG_CBNet_2018to2020CPUE_16Oct2021.csv")

#read in file to convert to biomass

zoop_biomass = read.csv("D:/Data/Suisun Marsh Salinity Gate Project/2021 Report/SMSCG Zoop 2021 Report/Data/Copepod and Cladoceran Biomass Values.csv")

#Editing file------

#filter the CPUE file so it only has the stations we're interested in 

stations = data.frame(Station = as.character(c(602, "Grizz", "NZ028", 519, "Honk", 605, 606, "NZ032", "NZS42", 609, 610, "Mont", 508, 513, 520, 801, 802, 704, 706, "NZ054", "NZ060", "NZ064")),
                  Region = c(rep("Grizzly", 3), rep("Honker", 2), rep("West Marsh", 4), rep("East Marsh", 3), rep("River", 10)))


stations = mutate(stations, Region = factor(Region, levels = c("Grizzly", "Honker", "West Marsh", "East Marsh", "River")))

#edited file with regions designated and added to csv

SMSCG_CPUE2 = filter(merge(select(SMSCG_CPUE, -Region), stations, by = "Station"), Year %in% c(2018, 2019, 2020), Month == 7 | Month ==8 | Month==9| Month==10) 

View(SMSCG_CPUE2)

#Converting to BPUE------

Zoop_BPUE = pivot_longer(SMSCG_CPUE2, cols = ACARTELA:CUMAC, 
                         names_to = "taxon", values_to = "CPUE") %>%
  left_join(zoop_biomass) %>%
  mutate(BPUE = CPUE*mass_indiv_ug) %>%
  filter(!is.na(BPUE)) %>%
  pivot_wider(id_cols = c(Station:Volume, Region), names_from = taxon, values_from = BPUE)

#Pick out just the columns we are interested in.

zoop_BPUE2= mutate(Zoop_BPUE, Acartiella = ACARTELA + ASINEJUV, 
                         Tortanus = TORTANUS + TORTJUV,
                         Limnoithona = LIMNOSPP + LIMNOSINE + LIMNOTET + LIMNOJUV,
                         Pseudodiaptomus = PDIAPFOR + PDIAPMAR + PDIAPJUV + PDIAPNAUP,
                         `Other Calanoids` = ACARTIA + DIAPTOM + EURYTEM + OTHCALAD + 
                           SINOCAL + EURYJUV + OTHCALJUV + SINOCALJUV + ACARJUV + DIAPTJUV + SINONAUP + EURYNAUP,
                         `Other Cyclopoids` = ACANTHO + OITHDAV + OITHSIM + OITHSPP + OTHCYCAD + OITHJUV + OTHCYCJUV + OITHSPP,
                         Other = BOSMINA + DAPHNIA + DIAPHAN + OTHCLADO + HARPACT + OTHCOPNAUP)
zoop_BPUE2a = zoop_BPUE2[,c(1,3,5, 6, 22, 60:66)]

#create a sample ID
zoop_BPUE2a$sample = 1:nrow(zoop_BPUE2a)

#turn month into a factor
zoop_BPUE2a$Month = factor(zoop_BPUE2a$Month, labels = c("Jul", "Aug", "Sep", "Oct"))

#write a csv 
write.csv(zoop_BPUE2a, "SMSCG_2018to2020_BPUE.csv")

#Convert to long format
zooplong = gather(zoop_BPUE2a, key = "Taxa", value = "BPUE", -Station, -Month, - Year, -Region, -Date, -sample)

#File with summary stats
zoopsummary = group_by(zooplong, Region, Year, Month, Taxa) %>% summarize(meanB = mean(BPUE), sdB = sd(BPUE), n = length(BPUE))

#Reorder taxa so more abundant taxa on bottom and can put n in there

zoopsummary2 =  zoopsummary %>% mutate(Taxa = factor(Taxa, levels = c("Other","Limnoithona", "Other Cyclopoids","Other Calanoids", 
                                                                                          "Pseudodiaptomus","Tortanus", "Acartiella", 
                                                                                          ordered = TRUE)))
#Making plot
bpue1 = ggplot(zoopsummary2, aes(x = Month, y = meanB))
bpue2 = bpue1+ geom_bar(stat = "identity", aes(fill = Taxa)) + 
  facet_wrap(Year~Region, ncol = 5)+(coord_cartesian (ylim = c(1, 15000))) +
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("Mean BPUE (µgC/m3)") +
  geom_label(aes(x = Month, y = 100, label = paste(n)))+
  theme_few() + theme(text = element_text(family = "sans", size = 10),
                      legend.text = element_text(face = "italic"))


ggsave(plot = bpue2, filename = "SMSCG2018to2020.tiff", device = "tiff", width = 6, height =5, units = "in", dpi = 300)

##Removing n from graph b/c its super big and can't figure out how to reduce the text

bpue1_noN = ggplot(zoopsummary2, aes(x = Month, y = meanB))
bpue2_noN = bpue1_noN+ geom_bar(stat = "identity", aes(fill = Taxa)) + 
  facet_wrap(Year~Region, ncol = 5)+ (coord_cartesian (ylim = c(1, 15000))) +
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("Mean BPUE (?gC/m3)") + 
  theme_few() + theme(text = element_text(family = "sans", size = 9),
                      legend.text = element_text(face = "italic"))

ggsave(plot = bpue2_noN, filename = "SMSCG2018to2020_noN.tiff", device = "tiff", width = 6, height =4, units = "in", dpi = 300)


##Analysis----

#GLM of total BPUE

zooptots = group_by(zooplong, Region, Year, Month, Station, sample) %>% 
  summarize(BPUE = sum(BPUE), logBPUE = log(BPUE))
zooptots$Month = factor(zooptots$Month, labels = c("July", "August", "Sept", "Oct"))
hist(zooptots$BPUE)
hist(zooptots$logBPUE)

#pretty skewed without transforming, but doing shapiro test to confirm

shapiro.test(zooptots$BPUE)
#way significant usin non transformed data

shapiro.test(zooptots$logBPUE)
#need log transformed data, but still pretty significant----- transform again?

#With Region*Month interaction. Region*Month only significant for River in Sept, don't need interaction
zlm = glm(logBPUE~Region*Month + Year, data = zooptots)
summary(zlm)
plot(zlm)

#No interaction terms
zlm2 = glm(logBPUE~Region + Month + Year, data= zooptots)
summary(zlm2)
plot(zlm2)

######Combining Grizzly and Honker into Suisun------

stations_SB = data.frame(Station = as.character(c(602, "Grizz", "NZ028", 519, "Honk", 605, 606, "NZ032", "NZS42", 609, 610, "Mont", 508, 513, 520, 801, 802, 704, 706, "NZ054", "NZ060", "NZ064")),
                      Region = c(rep("Suisun Bay", 5), rep("West Marsh", 4), rep("East Marsh", 3), rep("River", 10)))


stations_SB = mutate(stations_SB, Region = factor(Region, levels = c("Suisun Bay", "West Marsh", "East Marsh", "River")))

SMSCG_CPUE_SB = filter(merge(select(SMSCG_CPUE, -Region), stations_SB, by = "Station"), Year %in% c(2018, 2019, 2020), Month == 7 | Month ==8 | Month==9| Month==10) 


zoop_BPUE_SB = pivot_longer(SMSCG_CPUE_SB, cols = ACARTELA:CUMAC, 
                         names_to = "taxon", values_to = "CPUE") %>%
  left_join(zoop_biomass) %>%
  mutate(BPUE = CPUE*mass_indiv_ug) %>%
  filter(!is.na(BPUE)) %>%
  pivot_wider(id_cols = c(Station:Volume, Region), names_from = taxon, values_from = BPUE)


zoop_BPUE_SB= mutate(zoop_BPUE_SB, Acartiella = ACARTELA + ASINEJUV, 
                   Tortanus = TORTANUS + TORTJUV,
                   Limnoithona = LIMNOSPP + LIMNOSINE + LIMNOTET + LIMNOJUV,
                   Pseudodiaptomus = PDIAPFOR + PDIAPMAR + PDIAPJUV + PDIAPNAUP,
                   `Other Calanoids` = ACARTIA + DIAPTOM + EURYTEM + OTHCALAD + 
                     SINOCAL + EURYJUV + OTHCALJUV + SINOCALJUV + ACARJUV + DIAPTJUV + SINONAUP + EURYNAUP,
                   `Other Cyclopoids` = ACANTHO + OITHDAV + OITHSIM + OITHSPP + OTHCYCAD + OITHJUV + OTHCYCJUV + OITHSPP,
                   Other = BOSMINA + DAPHNIA + DIAPHAN + OTHCLADO + HARPACT + OTHCOPNAUP)
zoop_BPUE_SBa = zoop_BPUE_SB[,c(1,3,5, 6, 22, 60:66)]

#create a sample ID
zoop_BPUE_SBa$sample = 1:nrow(zoop_BPUE_SBa)

#turn month into a factor
zoop_BPUE_SBa$Month = factor(zoop_BPUE_SBa$Month, labels = c("Jul", "Aug", "Sep", "Oct"))

#write a csv 
write.csv(zoop_BPUE_SBa, "SMSCG_2018to2020_BPUE2.csv")

#Convert to long format
zooplong_SB = gather(zoop_BPUE_SBa, key = "Taxa", value = "BPUE", -Station, -Month, - Year, -Region, -Date, -sample)

#File with summary stats
zoopsummary_SB = group_by(zooplong_SB, Region, Year, Month, Taxa) %>% summarize(meanB = mean(BPUE), sdB = sd(BPUE), n = length(BPUE))

#Reorder taxa so more abundant taxa on bottom and can put n in there

zoopsummary_SB2 =  zoopsummary_SB %>% mutate(Taxa = factor(Taxa, levels = c("Other","Limnoithona", "Other Cyclopoids","Other Calanoids", 
                                                                      "Pseudodiaptomus","Tortanus", "Acartiella", 
                                                                      ordered = TRUE)))
#Making plot
bpue_SB = ggplot(zoopsummary_SB2, aes(x = Month, y = meanB))
bpue_SB2 = bpue_SB+ geom_bar(stat = "identity", aes(fill = Taxa)) + 
  facet_wrap(Year~Region, ncol = 4)+ (coord_cartesian (ylim = c(1, 15000))) +
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("Mean BPUE (?gC/m3)") + 
  theme_few() + theme(text = element_text(family = "sans", size = 9),
                      legend.text = element_text(face = "italic"))

ggsave(plot = bpue_SB2, filename = "SMSCG2018to2020_SB.tiff", device = "tiff", width = 6, height =5, units = "in", dpi = 300)

#Combining to SB looks better, and not big changes in trend

##Analysis with Suisun Bay Region----

#GLM of total BPUE

zooptots_SB = group_by(zooplong_SB, Region, Year, Month, Station, sample) %>% 
  summarize(BPUE = sum(BPUE), logBPUE = log(BPUE))
zooptots_SB$Month = factor(zooptots_SB$Month, labels = c("July", "August", "Sept", "Oct"))
hist(zooptots_SB$BPUE)
hist(zooptots_SB$logBPUE)

#With Region*Month interaction. Region*Month not significant, don't need interaction
zlm_SB = glm(logBPUE~Region*Month + Year, data = zooptots_SB)
summary(zlm_SB)
plot(zlm_SB)

#No interaction terms
zlm_SB2 = glm(logBPUE~Region + Month + Year, data= zooptots_SB)
summary(zlm_SB2)
plot(zlm_SB2)

##Anovas with Suisun Bay Region------

anov_SB = aov(logBPUE~Region + Month, data = zooptots_SB)
summary(anov_SB)

TukeyHSD(anov_SB)


##Turn year into a factor-----

#problem with year. it says its a number, need to be a factor?

#turn year into a factor
zoop_BPUE_SBa$Year = factor(zoop_BPUE_SBa$Year, labels = c("2018", "2019", "2020"))

#Convert to long format
zooplong_SB_Y = gather(zoop_BPUE_SBa, key = "Taxa", value = "BPUE", -Station, -Month, - Year, -Region, -Date, -sample)

#File with summary stats
zoopsummary_SB_Y = group_by(zooplong_SB_Y, Region, Year, Month, Taxa) %>% summarize(meanB = mean(BPUE), sdB = sd(BPUE), n = length(BPUE))

write.csv(zoopsummary_SB_Y, "ZoopSummarySB.csv")
zooptots_SB_Y = group_by(zooplong_SB_Y, Region, Year, Month, Station, sample) %>% 
  summarize(BPUE = sum(BPUE), logBPUE = log(BPUE))



#With Region*Month interaction. Region*Month not significant, don't need interaction
zlm_SB_Y = glm(logBPUE~Region*Month + Year, data = zooptots_SB_Y)
summary(zlm_SB_Y)
plot(zlm_SB)

#No interaction terms
zlm_SB_Y2 = glm(logBPUE~Region + Month + Year, data= zooptots_SB_Y)
summary(zlm_SB_Y2)
plot(zlm_SB_Y2)

##Anovas with Suisun Bay Region------

anov_SB_Y = aov(logBPUE~Region + Month + Year, data = zooptots_SB_Y)
summary(anov_SB_Y)

TukeyHSD(anov_SB_Y)





















