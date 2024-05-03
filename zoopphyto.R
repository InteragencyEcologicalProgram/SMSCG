#zooplankton/phytoplankton relationships

library(tidyverse)
library(lubridate)
library(corrplot)

zooplankton = read_csv("smscgto2023_cals_long.csv")
phyto = read_csv("EDI/data_output/smscg_phytoplankton_samples_2020-2023.csv")
phytotax = read_csv("EDI/data_output/smscg_phytoplankton_taxonomy.csv")
#ugh, i guess i need to attach the regions and summarize by month before I can get started comparing to zoops
stationslookup = read_csv("Data/smscg_stations_phyto.csv")

phyto2 = left_join(phyto, phytotax) %>%
  left_join(select(stationslookup, station, alias, region))%>%
  mutate(SampleID = paste(station, date, time_pst))

#summarize by genus
phyto3 = group_by(phyto2, genus, station, latitude, longitude, date) %>%
  summarize(bv = sum(biovolume_per_ml))

ggplot(phyto3, aes(x = date, y = bv, color = genus)) + geom_point()

foo = filter(phyto3, bv > 60000000)

#summarize by class
phyto3c = group_by(phyto2, class, station, latitude, longitude, date, region) %>%
  summarize(bv = sum(biovolume_per_ml)) %>%
  mutate(Year = year(date), Month = month(date))

ggplot(phyto3c, aes(x = date, y = bv, color = class)) + geom_point()+
  facet_wrap(~region)

ggplot(phyto3c, aes(x = as.factor(Year), y = bv, color = class)) + geom_boxplot()+
  facet_wrap(~region)+
  scale_y_log10()

ggplot(filter(phyto3c, region != "FLO"), aes(x = region, y = bv, color = as.factor(Year))) + geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~class, scales = "free")

#summarize by algal group
phyto3ag = group_by(phyto2, algal_group, station, latitude, longitude, date, region) %>%
  summarize(bv = sum(biovolume_per_ml)) %>%
  mutate(Year = year(date), Month = month(date))

ggplot(phyto3ag, aes(x = date, y = bv, color = algal_group)) + geom_point()+
  facet_wrap(~region)

ggplot(phyto3ag, aes(x = as.factor(Year), y = bv, color = algal_group)) + geom_boxplot()+
  facet_wrap(~region)+
  scale_y_log10()

ggplot(filter(phyto3ag, region != "FLO"), aes(x = region, y = bv, color = as.factor(Year))) + geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~algal_group, scales = "free")

#add in zeros
phytoAG = pivot_wider(phyto3ag, names_from = algal_group, values_from = bv, values_fill = 0) %>%
  pivot_longer(cols = c(`Centric Diatoms`:last_col()), names_to = "algal_group", values_to = "bv") %>%
  rename(Date = date)

#pair up zooplankton and phytoplanton

unique(phyto3ag$station)
unique(zooplankton$Station)
#OK, first I'll make a dataset of the pared phyto/zoop samples, then I'll do monthly regional averages
#just do one species at a time, start with psudos

zoops = mutate(zooplankton, station = paste(Source, Station, sep = "_"), SampleID = paste(Source, Station, Date)) %>%
  filter(Source != "DOP")
phyto3ag$station %in% zoops$station

pseudo = filter(zoops, station %in% phyto3ag$station, Taxa_Group == "Pseudodiaptomus")

phyto_pseudo = left_join(pseudo, phytoAG) %>%
  filter(!is.na(algal_group))

ggplot(phyto_pseudo, aes(x = log(bv/1000+1), y = log(BPUE+1)))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")+
  facet_wrap(~algal_group, scales = "free")+
  ylab("Log-transformed Pseudodiaptomus BPUE")+
  xlab("log-transformed biovolume")+
  theme_bw()
#so there is maybe a weak positive relationship with pennate diatoms, cyanobacteria, and green algae.

phyto_pseudo2 = pivot_wider(phyto_pseudo, names_from = algal_group, values_from = bv)

lm1 = lm(log(BPUE+1)~ log(`Green Algae`/1000+1), data = phyto_pseudo2)
summary(lm1)
plot(lm1)
#it's significant, but the r-squared is only 0.05

lm2 = lm(log(BPUE+1)~ log(`Cyanobacteria`/1000+1), data = phyto_pseudo2)
summary(lm2)
plot(lm2)
#not significant

lm3 = lm(log(BPUE+1)~ log(`Pennate Diatoms`/1000+1), data = phyto_pseudo2)
summary(lm3)
plot(lm3)
#significant, but r-squared is 0.02

#what about total biovolume?

phyto_pseudo3 = group_by(phyto_pseudo, station, SampleID, Date, Region, Year, Month, BPUE, Taxa_Group) %>%
  summarize(bv = sum(bv))

lm4 = lm(log(BPUE+1)~ log(bv/1000+1), data = phyto_pseudo3)
summary(lm4)
#nope.

#what if I stuck month and region in there?
lm5 = lm(log(BPUE+1)~ log(bv/1000+1)+Month+Region+as.factor(Year), data = phyto_pseudo3)
summary(lm5)
#nope. worse
?corrplot
?cor

zoopmat = filter(zoops, SampleID %in% phyto_pseudo$SampleID) %>%
  pivot_wider(id_cols = c(SampleID, station, Date), names_from = "Taxa_Group", values_from = BPUE, values_fill = 0)%>%
  arrange(station, Date) %>%
  select(Acartiella:last_col())

phytomat = arrange(phyto_pseudo2, station, Date) %>%
  select(`Centric Diatoms`:last_col())

zpcor = cor(zoopmat, phytomat)
corrplot(zpcor, outline = T)
corrplot(zpcor, "number")

###################################################
#now summarize by region and month

zoopsum = group_by(zooplankton, Region, Month, Taxa_Group, Year) %>%
  summarize(BPUE = mean(BPUE, na.rm =T))

phytosum = group_by(phytoAG, region, Month, algal_group, Year) %>%
  summarize(bv = mean(bv, na.rm =T)) %>%
  mutate(Region = case_when(region == "RIV" ~ "River",
                            region == "BAY" ~ "Suisun Bay",
                            region == "MAR" ~ "Suisun Marsh")) %>%
  filter(!is.na(Region))

psusum = filter(zoopsum, Taxa_Group == "Pseudodiaptomus") %>%
  left_join(phytosum) %>%
  filter(!is.na(algal_group))

ggplot(psusum, aes(x = log(bv/1000+1), y = log(BPUE+1)))+
  geom_point(aes(color = Month))+
  geom_smooth(method = "lm")+
  facet_wrap(~algal_group, scales = "free")+
  ylab("Log-transformed Pseudodiaptomus BPUE")+
  xlab("log-transformed biovolume")+
  theme_bw()


###################################
library(lme4)
library(lmerTest)
library(effects)

#just look at centric diatoms
cen = filter(phytoAG, algal_group == "Centric Diatoms", region != "FLO") %>%
  mutate(Yearf = as.factor(Year), Monthf = as.factor(Month))

cenlm = lmer(log(bv/1000+1) ~ Yearf + region + (1|station)+ (1|Month), data = cen)
summary(cenlm)
plot(allEffects(cenlm))

cenlm2 = lmer(log(bv/1000+1) ~ Yearf + region + Monthf + (1|station), data = cen)
summary(cenlm2)
plot(allEffects(cenlm2))



#pennate diatoms
pen = filter(phytoAG, algal_group == "Pennate Diatoms", region != "FLO") %>%
  mutate(Yearf = as.factor(Year), Monthf = as.factor(Month))

penlm = lmer(log(bv/1000+1) ~ Yearf*region + (1|station)+ (1|Month), data = pen)
summary(penlm)
plot(allEffects(penlm))

penlm2 = lmer(log(bv/1000+1) ~ Yearf + region + Monthf + (1|station), data = pen)
summary(penlm2)
plot(allEffects(penlm2))
