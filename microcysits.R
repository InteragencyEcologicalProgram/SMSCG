#LEt's look at microcystis
#I'm not sure which data TEd used for the origional graph, but I'll use the zooplankton data that has mycrocystis on 
#the data set. I could also use FMWT, Townet, or EMP.

library(tidyverse)
library(readxl)
library(ggthemes)

#import the data
mysids = read_excel("SMSCG MysidAmphipodCPUEJuly2019.xlsx", sheet = "Mysid CPUE")
Amps = read_excel("SMSCG MysidAmphipodCPUEJuly2019.xlsx", sheet = "AmphipodCPUE")
zoops = read_excel("FMWT TNSZooplanktonDataCPUE25Feb2019.xls", sheet = 5)

#Subset so it is just the stations we are interested in
stas = data.frame(Station = c(513, 520, 801, 802, 606, 609, 610, "Mont", "NZ032"), 
                  Region = c(rep("River", 4), rep("Suisun Marsh", 5)))
unique(mysids$Station)
stas = mutate(stas, Region = factor(Region, levels = c("Suisun Marsh", "River")))


#Just the salinity control gate stations in Suisun and the Confluence in 2018
zoops = merge(zoops, stas, by = "Station")
zoops = filter(zoops, Year == 2018)

#we're just interested in microcystis abundance, for now
Micro = zoops[,c(1,3,5,6,23,79)]

#average microcystis abundance by region and month
micro2 = group_by(Micro, Region.y, Month) %>% 
  summarize(meanM = mean(Microcystis, na.rm = T), sdM = sd(Microcystis, na.rm = T), n = length(Microcystis))
micro2$Month = factor(micro2$Month, labels = c("Jul", "Aug", "Sep", "Oct"))

m1 = ggplot(micro2, aes(x = Month, y = meanM))

m2 = m1 + facet_wrap(~Region.y) + geom_bar(stat = "identity", fill = "green") +
  geom_errorbar(aes(ymin = meanM-sdM, ymax = meanM + sdM), width = 0.2) + 
  coord_cartesian(ylim = c(1,5)) + 
  scale_y_continuous(breaks = c(1,2,3,4,5), 
                     labels = c("1\n Absent", "2\n Low", "3 \n Medium", "4 \n High", "5 \n Very High")) +
  theme_few() +
  ylab("Qualitative Microcsystis level") +
  theme(text = element_text(family = "sans", size = 12),
        axis.text.y = element_text(hjust = .5), legend.position = "bottom") +
  geom_label(aes(x=Month, y =0.95, label = paste("n=", n)))

m2

ggsave(plot = m2, filename = "Microcystisplot.tiff", device = "tiff", width = 6, height =4, units = "in", dpi = 300)

#################################################################################
#Microcysis GLM
Micro$Month = factor(Micro$Month, labels = c("Jul", "Aug", "Sep", "Oct"))
m1 = lm(Microcystis~Month*Region.y, data = Micro)
summary(m1)             
visreg(m1, xvar = "Month", by = "Region.y")

####################################################################
#reviewers suggested an ordered probit/logit model instead

library(MASS)
library(AER)

Micro$Microcystis = as.ordered(Micro$Microcystis)
m2 = polr(Microcystis~Month*Region.y, data = Micro)
summary(m2)
coeftest(m2)

m3 = polr(Microcystis~Month+Region.y, method = "probit",data = Micro)
summary(m3)

coeftest(m3)

#I'm going to convert it to present/absent, just to make life easier
Micro$PA = NA
Micro$PA[which(Micro$Microcystis=="1")] = "Absent"
Micro$PA[which(Micro$Microcystis %in% c("2", "4"))] = "Present"
Micro = mutate(Micro, PA = as.ordered(PA),
               Region = factor(Region.y, levels = c("River", "Suisun Marsh")))


m4 = glm(PA~Month+Region, family=binomial(link='logit'), data = Micro)
summary(m4)

m5 = glm(PA~Month*Region, family=binomial(link='logit'), data = Micro)
summary(m5)

m6 = glm(PA~NULL, family=binomial(link='logit'), data = Micro)

library(MuMIn)
AICc(m4, m5, m6)
