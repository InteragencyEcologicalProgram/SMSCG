#zooplankton/phytoplankton relationships

library(tidyverse)
library(lubridate)
library(corrplot)

zooplankton = read_csv("smscgto2023_cals_long.csv")
zooplankton2 = read_csv("smscgto2023_ungrp_long.csv")
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
phyto3ag = group_by(phyto2, algal_group, station, latitude, longitude, date, region, SampleID) %>%
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
  rename(Date = date) %>%
  mutate(SampleID = paste(station, date(Date)))

#pair up zooplankton and phytoplanton

unique(phyto3ag$station)
unique(zooplankton$Station)
#OK, first I'll make a dataset of the pared phyto/zoop samples, then I'll do monthly regional averages
#just do one species at a time, start with psudos

zoops = mutate(zooplankton, station = paste(Source, Station, sep = "_"), SampleID = paste(station, Date)) %>%
  filter(Source != "DOP")
phyto3ag = mutate(phyto3ag, SampleID = paste(station, date(date)))

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
############################################
zoop = filter(zoops, station %in% phyto3ag$station) %>%
  mutate(Type = "zoops") %>%
  select(!Source, !Station)

phyto3AG = rename(phytoAG, Taxa_Group = algal_group) %>%
  mutate(Type = "phyto", BPUE = bv/1000)

zoopag = bind_rows(zoop, phyto3AG) %>%
  mutate(Region = case_when(region == "RIV" ~ "River",
                            region == "BAY" ~ "Suisun Bay",
                            region == "MAR" ~ "Suisun Marsh"),
         DOY = yday(Date)) %>%
  filter(!is.na(Region))

ggplot(zoopag, aes(x = Date, y = BPUE, color = Taxa_Group))+
  stat_summary(geom = "line")+
  facet_wrap(~Region)+
  scale_y_log10()


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

################################################################################
#multivariate stuff
#Maybe I should put both phytoplankton and zooplankton together and do an NMDS or cluster analysis or something.
zoopmata = filter(zoops, SampleID %in% phyto_pseudo$SampleID) %>%
  pivot_wider(id_cols = c(SampleID, station, Date), names_from = "Taxa_Group", values_from = BPUE, values_fill = 0)%>%
  arrange(station, Date) %>%
  select(SampleID, Acartiella:last_col())

#grab some more environmental data
zoopdataall <- read_csv("Data/SMSCG_CBNet_2018to2023CPUE_07Feb2024.csv", 
                        col_types = cols(Date = col_date(format = "%m/%d/%Y")))

zoopdat = mutate(zoopdataall, SampleID = paste(Project, Station, Date)) %>%
  select(SampleID, CondSurf, PPTSurf, TempSurf, Turbidity, Secchi)

allmat = left_join(select(phyto_pseudo2, -BPUE, -Taxa_Group), zoopmata)

taxamat = as.matrix(select(allmat, `Centric Diatoms`:last_col()))

library(vegan)
zpnmds = metaMDS(taxamat, trymax = 200)
samplesmds = zpnmds$points
species = data.frame(zpnmds$species, Species = row.names(zpnmds$species), Type = c(rep("phyto", 9), rep("copepod", 5)))

envmat = select(all, !`Centric Diatoms`:last_col()) %>%
  left_join(zoopdat)

#look at fit of environmental variables on the nmds
monthfit = envfit(zpnmds, env = envmat, na.rm =T)

#extract the vectors
scrs <- as.data.frame(scores(monthfit, display = "vectors"))
scrs <- cbind(scrs, variable = rownames(scrs))

#put the NMDS results onthe origional dataframe for plotting
all = bind_cols(allmat, samplesmds)
ggplot(all, aes(x = MDS1, y = MDS2))+ geom_point(aes(shape= Region, color = Region))+
  stat_ellipse(aes(color = Region))+
  geom_text(data = species, aes(label = Species, color = Type))+  
  geom_text(data = filter(scrs, variable != "CondSurf", variable != "Year"), aes(x = NMDS1, y = NMDS2, label = variable),
            size = 5)+
  geom_segment(data = filter(scrs, variable != "CondSurf", variable != "Year"),
              aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
              arrow = arrow(length = unit(0.25, "cm")), colour = "blue") +

  
  theme_bw()

ggplot(all, aes(x = MDS1, y = MDS2))+ geom_point(aes(shape= as.factor(Year), color = as.factor(Year)))+
  stat_ellipse(aes(color = as.factor(as.factor(Year))))+
 # geom_text(data = species, aes(label = Species, color = Type))+  
  #geom_text(data = filter(scrs, variable != "CondSurf", variable != "Year"), aes(x = NMDS1, y = NMDS2, label = variable),
  #          size = 5)+
  #geom_segment(data = filter(scrs, variable != "CondSurf", variable != "Year"),
  #             aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "blue") +
  
  
  theme_bw()


##################################################
#try again with more taxa
zooptax = read_csv("data/zooptax.csv")
zooplankton2 = left_join(zooplankton2, zooptax)

zoops2 = mutate(zooplankton2,  station = paste(Source, Station, sep = "_"), SampleID = paste(Source, Station, Date)) %>%
  filter(!(Lifestage == "Larva" & (Category %in%c("Cyclopoid", "Calanoid"))))%>%
  group_by(SampleID, Station, station, Date, Region, Year, Month, Cat2) %>%
  summarize(BPUE = sum(BPUE))
  

zoopmat2 = filter(zoops2, SampleID %in% phyto_pseudo$SampleID) %>%
  pivot_wider(id_cols = c(SampleID, station, Date), names_from = "Cat2", values_from = BPUE, values_fill = 0)%>%
  arrange(station, Date) %>%
  ungroup() %>%
  select(Bosmina:last_col())

zoopmat2.1 = filter(zoops2, SampleID %in% phyto_pseudo$SampleID) %>%
  pivot_wider(id_cols = c(SampleID, station, Date), names_from = "Cat2", values_from = BPUE, values_fill = 0)%>%
  arrange(station, Date) %>%
  ungroup()

phytomat = arrange(phyto_pseudo2, station, Date) %>%
  ungroup() %>%
  select(`Centric Diatoms`:last_col())

zpcor2 = cor(zoopmat2, phytomat)
corrplot(zpcor2, outline = T)
corrplot(zpcor2, "number")


#############################################
#what about a cluster analysis
dist1 = dist(log(t(select(allmat, `Centric Diatoms`:last_col()))+1))
clus1 = hclust(dist1)
plot(clus1)

#######################################################
#pete suggests cooccur

#grab some more environmental data

allmat = left_join(select(phyto_pseudo2, -BPUE, -Taxa_Group, -SampleID), zoopmat2.1)

taxamatPA = as.matrix(select(allmat, `Centric Diatoms`:last_col()))
sitemask = matrix(1, nrow = 254, ncol = 24)

cooccur(taxamat, type = "site_spp")

########################################################
#Gabe suggested doing an NMDS on zoops and then using enfit with phyto.

nmds2 = metaMDS(zoopmat2, trymax = 200)

envmat2 = select(all, !`Bosmina`:last_col()) %>%
  arrange(station, Date)
samplesmds2 = nmds2$points
species2 = data.frame(nmds2$species, Species = row.names(nmds2$species))



fit2 = envfit(nmds2, env = envmat2, na.rm =T)


#extract the vectors
scrs2 <- as.data.frame(scores(fit2, display = "vectors"))
scrs2 <- cbind(scrs2, variable = rownames(scrs2)) %>%
  filter(!variable %in% c("Year", "Month", "longitude", "latitude"))

#put the NMDS results onthe origional dataframe for plotting
all2 = left_join(zoopmat2.1, envmat2) %>%
  bind_cols(samplesmds2)
ggplot(all2, aes(x = MDS1, y = MDS2))+ geom_point(aes(shape= Region, color = Region))+
  stat_ellipse(aes(color = Region))+
  geom_text(data = species2, aes(label = Species))+  
  geom_text(data = filter(scrs2, variable != "CondSurf", variable != "Year"), 
            aes(x = NMDS1*10, y = NMDS2*10, label = variable),
            size = 3, color = "blue")+
  geom_segment(data = filter(scrs2, variable != "CondSurf", variable != "Year"),
               aes(x = 0, xend = NMDS1*10, y = 0, yend = NMDS2*10),
               arrow = arrow(length = unit(0.25, "cm")), colour = "blue") +
  
  
  theme_bw()

ggplot(all, aes(x = MDS1, y = MDS2))+ geom_point(aes(shape= as.factor(Year), color = as.factor(Year)))+
  stat_ellipse(aes(color = as.factor(as.factor(Year))))+
  # geom_text(data = species, aes(label = Species, color = Type))+  
  #geom_text(data = filter(scrs, variable != "CondSurf", variable != "Year"), aes(x = NMDS1, y = NMDS2, label = variable),
  #          size = 5)+
  #geom_segment(data = filter(scrs, variable != "CondSurf", variable != "Year"),
  #             aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "blue") +
  
  
  theme_bw()

#what about a cca the same way?
pen = as.matrix(select(envmat2, `Centric Diatoms`:last_col()))
ccatest = cca(zoopmat2, pen)
ccaplot = plot(ccatest, scaling = "symmetric")

ccascoresSP = scores(ccatest, display = c("sp", "wa", "bp"),
             scaling = "species", tidy = T)

ggplot()+
  geom_point(data = filter(ccascoresSP, score== "sites"), aes(x = CCA1/10, y = CCA2/10))+
  geom_text(data = filter(ccascoresSP, score== "species"), aes(x = CCA1, y = CCA2, label = label), size =3, color = "red")+
  geom_segment(data = filter(ccascoresSP, score== "biplot"), aes(x = 0, xend = CCA1, y = 0, yend = CCA2), 
               arrow = arrow(length = unit(0.1, "inches")))+
geom_text(data = filter(ccascoresSP, score== "biplot"), aes(x = CCA1, y =  CCA2, label = label))
