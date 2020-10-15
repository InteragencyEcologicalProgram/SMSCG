#let's take a look at the zooplankton data from the 2018 action


library(tidyverse)
library(readxl)
library(ggthemes)

#import the biomass data
zoopB <- read_excel("Data/FMWT_TNSZooplanktonBPUEMarch2019.xlsx", 
                    sheet = "FMWT&TNS ZP BPUE")

#Now filter it so it's just the stations and time period we are interested in
stas = data.frame(Station = as.character(c(513, 520, 801, 802, 606, 609, 610, "Mont", "NZ032")), 
                  Region = c(rep("River", 4), rep("Suisun Marsh", 5)))

stas = mutate(stas, Region = factor(Region, levels = c("Suisun Marsh", "River")))

zoopB = filter(merge(zoopB, stas, by = "Station"), Year == 2018, Month == 7 | Month ==8 | Month==9| Month==10) 

#Pick out just the columns we are interested in
zoopB2= mutate(zoopB, Acartiella = ACARTELA + ASINEJUV, 
              Tortanus = TORTANUS + TORTJUV,
                Limnoithona = LIMNOSPP + LIMNOSINE + LIMNOTET + LIMNOJUV,
              Pseudodiaptomus = PDIAPFOR + PDIAPMAR + PDIAPJUV + PDIAPNAUP,
              `Other Calanoids` = ACARTIA + DIAPTOM + EURYTEM + OTHCALAD + 
                SINOCAL + EURYJUV + OTHCALJUV + SINOCALJUV + ACARJUV + DIAPTJUV + SINONAUP + EURYNAUP,
              `Other Cyclopoids` = AVERNAL + OITHDAV + OITHSIM + OTHCYCAD + OITHJUV + OTHCYCJUV,
              Other = HARPACT + OTHCOPNAUP + ALLCLADOCERA)
zoopB2 = zoopB2[,c(1,3,5,6,22, 67:74)]
zoopB2$sample = 1:nrow(zoopB2)
zoopB2$Month = factor(zoopB2$Month, labels = c("Jul", "Aug", "Sep", "Oct"))

#write a csv for publication
#write.csv(zoopB2, "plankton_2018_smscg.csv")

#figure out what samples/stations we might be missing
zooptest = group_by(zoopB2, Station, Month) %>% summarize(n = length(sample))
#write.csv(zooptest, "Zoopsamples.csv", row.names = F)

#now from wide to long
zooplong = gather(zoopB2, key = "Taxa", value = "BPUE", -Station, -Month, - Year, -Region.y, -sample, -Date, - Microcystis)

#Now the summary version
zoopsum = group_by(zooplong, Region.y, Month, Taxa) %>% summarize(meanB = mean(BPUE), sdB = sd(BPUE), n = length(BPUE))

#reorder the factor levels so they look nicer
zoopsum$Taxa = factor(zoopsum$Taxa, levels = c("Other","Limnoithona", "Other Cyclopoids",
                                               "Other Calanoids", 
                                               "Tortanus", "Pseudodiaptomus","Acartiella"))
zoopsum$Month = factor(zoopsum$Month, labels = c("Jul", "Aug", "Sep", "Oct"))

b1 = ggplot(zoopsum, aes(x = Month, y = meanB))
b2 = b1 + geom_bar(stat = "identity", aes(fill = Taxa)) + facet_wrap(~Region.y) +
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("Mean BPUE (µgC/m3)") +
  geom_label(aes(x = Month, y = 100, label = paste("n=", n))) + 
  theme_few() + theme(text = element_text(family = "sans", size = 12),
    legend.text = element_text(face = "italic"))


b2

#ggsave(plot = b2, filename = "zoopsplot.tiff", device = "tiff", width = 6, height =4, units = "in", dpi = 300)


#####################################################################################################
#now a glm of total BPUE
zooptots = group_by(zooplong, Region.y, Month, Station, sample) %>% 
  summarize(BPUE = sum(BPUE), logBPUE = log(BPUE))
zooptots$Month = factor(zooptots$Month, labels = c("Jul", "Aug", "Sep", "Oct"))
hist(zooptots$BPUE)
hist(zooptots$logBPUE)

#quick boxplot to see whether we would expect any difference in total BPUE
tot = ggplot(data = zooptots, aes(x= Month, y = BPUE))
tot + geom_boxplot() + facet_wrap(~Region.y)

shapiro.test(zooptots$BPUE)
shapiro.test(zooptots$logBPUE)
#with october added in with need the log-transformed data. 

zlm1 = glm(BPUE~Month*Region.y, data = zooptots)
summary(zlm1)
plot(zlm1)

zlm2 = glm(BPUE~Month + Region.y, data = filter(zooptots, Month != "Oct"))
summary(zlm2)
plot(zlm2)

zlm3 = lm(logBPUE~Month*Region.y, data = filter(zooptots, Month != "Oct"))
summary(zlm3)
plot(zlm3)
visreg(zlm3, xvar = "Month", by = "Region.y")

zlm4 = glm(logBPUE~Month+Region.y, data = filter(zooptots, Month != "Oct"))
summary(zlm4)
plot(zlm4)
visreg(zlm4)


#Let's try an exploitory power analysis to see if we
#are collecting enough samples in the future
library(sjstats)
library(pwr)

a1 =aov(logBPUE~Month+Region.y, data = filter(zooptots, Month != "Oct"))
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
