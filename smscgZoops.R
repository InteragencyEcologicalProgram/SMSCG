#let's take a look at the zooplankton data from the 2018 action
#and add the 2019 stuff too.

library(tidyverse)
library(readxl)
library(ggthemes)


SMSCG_zoops <- read_excel("Data/SMSCG_CBNet_2018to2019CPUE_24August2020.xlsx", 
                          sheet = "SMSCGZoopCPUE")


#carbon mass for individual zooplankton taxa (cladocerans, copepods)
zmass<-read.csv("Data/zoop_individual_mass.csv")


#Now filter it so it's just the stations and time period we are interested in
stas = data.frame(Station = as.character(c(513, 520, 801, 802, 606, 609, 610, "Mont", "NZ032")), 
                  Region = c(rep("River", 4), rep("Suisun Marsh", 5)))

#Note: We may want to include other stations, these were just the ones we used
#for Ted's paper.

stas = mutate(stas, Region = factor(Region, levels = c("Suisun Marsh", "River")))

zoopB = filter(merge(select(SMSCG_zoops, -Region), stas, by = "Station"), Year %in% c(2018, 2019), Month == 7 | Month ==8 | Month==9| Month==10) 

#Convert to biomass
zoopBM = pivot_longer(zoopB, cols = ACARTELA:CUMAC, 
                      names_to = "taxon", values_to = "CPUE") %>%
  left_join(zmass) %>%
  mutate(BPUE = CPUE*mass_indiv_ug) %>%
  filter(!is.na(BPUE)) %>%
  pivot_wider(id_cols = c(Station:Volume, Region), names_from = taxon, values_from = BPUE)


#Pick out just the columns we are interested in
zoopB2= mutate(zoopBM, Acartiella = ACARTELA + ASINEJUV, 
              Tortanus = TORTANUS + TORTJUV,
              Cladocera = BOSMINA + DAPHNIA + DIAPHAN + OTHCLADO,
                Limnoithona = LIMNOSPP + LIMNOSINE + LIMNOTET + LIMNOJUV,
              Pseudodiaptomus = PDIAPFOR + PDIAPMAR + PDIAPJUV + PDIAPNAUP,
              `Other Calanoids` = ACARTIA + DIAPTOM + EURYTEM + OTHCALAD + 
                SINOCAL + EURYJUV + OTHCALJUV + SINOCALJUV + ACARJUV + DIAPTJUV + SINONAUP + EURYNAUP,
              `Other Cyclopoids` = AVERNAL + OITHDAV + OITHSIM + OTHCYCAD + OITHJUV + OTHCYCJUV,
              Other = HARPACT + OTHCOPNAUP)
zoopB2a = zoopB2[,c(1,3,5,6,22,25, 63:69)]

#create a sample ID
zoopB2a$sample = 1:nrow(zoopB2a)

#turn month into a factor
zoopB2$Month = factor(zoopB2$Month, labels = c("Jul", "Aug", "Sep", "Oct"))

#write a csv for publication
#write.csv(zoopB2, "plankton_2018_smscg.csv")


#now from wide to long
zooplong = gather(zoopB2a, key = "Taxa", value = "BPUE", -Station, -Month, - Year, -Region, -sample, -Date, - Microcystis)

#Now the summary version
zoopsum = group_by(zooplong, Region, Year, Month, Taxa) %>% summarize(meanB = mean(BPUE), sdB = sd(BPUE), n = length(BPUE))

#reorder the factor levels so they look nicer
zoopsum$Taxa = factor(zoopsum$Taxa, levels = c("Other","Limnoithona", "Other Cyclopoids",
                                               "Other Calanoids", 
                                               "Tortanus", "Pseudodiaptomus","Acartiella"))

b1 = ggplot(zoopsum, aes(x = Month, y = meanB))
b2 = b1 + geom_bar(stat = "identity", aes(fill = Taxa)) + 
  facet_wrap(Year~Region) +
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("Mean BPUE (µgC/m3)") +
  geom_label(aes(x = Month, y = 100, label = paste("n=", n))) + 
  theme_few() + theme(text = element_text(family = "sans", size = 12),
    legend.text = element_text(face = "italic"))


b2

#ggsave(plot = b2, filename = "zoopsplot.tiff", device = "tiff", width = 6, height =4, units = "in", dpi = 300)


#####################################################################################################
#now a glm of total BPUE
zooptots = group_by(zooplong, Region, Year, Month, Station, sample) %>% 
  summarize(BPUE = sum(BPUE), logBPUE = log(BPUE))
zooptots$Month = factor(zooptots$Month, labels = c("Jul", "Aug", "Sep", "Oct"))
hist(zooptots$BPUE)
hist(zooptots$logBPUE)

#quick boxplot to see whether we would expect any difference in total BPUE
tot = ggplot(data = zooptots, aes(x= Month, y = BPUE))
tot + geom_boxplot() + facet_wrap(Year~Region)

shapiro.test(zooptots$BPUE)
shapiro.test(zooptots$logBPUE)
# need the log-transformed data. Even then it's marginal

zlm1 = glm(BPUE~Month*Region + Year, data = zooptots)
summary(zlm1)
plot(zlm1)

zlm2 = glm(BPUE~Month + Region + Year, data = filter(zooptots, Month != "Oct"))
summary(zlm2)
plot(zlm2)

zlm3 = lm(logBPUE~Month*Region + Year, data = filter(zooptots, Month != "Oct"))
summary(zlm3)
plot(zlm3)
visreg(zlm3, xvar = "Month", by = "Region.y")

zlm4 = glm(logBPUE~Month+Region + Year, data = filter(zooptots, Month != "Oct"))
summary(zlm4)
plot(zlm4)
visreg(zlm4)


#Let's try an exploitory power analysis to see if we
#are collecting enough samples in the future
library(sjstats)
library(pwr)

a1 =aov(logBPUE~Month+Region, data = filter(zooptots, Month != "Oct"))
effectsize::cohens_f(a1)

#power to detect differences between regions
pwr.anova.test(2, n = 25, f = 0.5, sig.level = 0.05)
#observed power is 0.93!

#What effect size can we expect with a power of 0.8?
pwr.anova.test(2, n = 25, sig.level = 0.05, power = 0.8)
#We can get at least f = 0.4

#power to detect differences between months
pwr.anova.test(3, n = 13, f = 0.4, sig.level = 0.05)
#observed power is only 0.56, not great.

#how many samples give us a power of 0.8?
pwr.anova.test(3,  f = 0.4, sig.level = 0.05, power = 0.8)
#Need at least 21 samples. 

#but in the future, we'll have a longer "during" period, not just one month. 
#It will be harder to extract seasonal trends from the action, but...

#################################################################################################
#What does an NMDS look like?

library(vegan)
zNMDS = metaMDS(zoopB2[,7:13], trymax = 50)
zNMDS

source("plotNMDS.R")

PlotNMDS(zNMDS, data = zoopB2, group = "Region.y")

PlotNMDS(zNMDS, data = zoopB2, group = "Month")
