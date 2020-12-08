# Script to run fish community analysis for DWR ITP requirement to report on Suisun Marsh salinity control gate action
# For 2020
# Brian Mahardja - 11/20/2020

library(lubridate)
library(vegan)
library(tidyverse)
library(readxl)
library(arsenal)

#Path to local drive
root <- "D:/Projects/Suisun Marsh Salinity Control Gate Action/2020 DWR Suisun Marsh Salinity Control Gate Action Report"
setwd(root)

data_root<-file.path(root,"Data")
output_root <- file.path(root,"Output")

End_year=2020

edsm <- read.csv(file.path(data_root, "EDSM_KDTR.csv"))
edsm_2020 <- read_excel(file.path(data_root, "2020-11-17_EDSM June-October 2020.xlsx"),sheet="Kodiak data June-Oct 2020")

edsm$Date<-as.Date(edsm$Date, "%Y-%m-%d")
edsm_2020$Date<-as.Date(edsm_2020$Date, "%Y-%m-%d")

#Edit 2020 data to match EDI dataset
edsm_2020<- edsm_2020 %>% rename(GearConditionCode=Cond)
edsm_2020$SampleID=NULL

#Create data just for summer-fall (June to October)
edsm_combined<-edsm %>% filter(month(Date) %in% c(6:10))

edsm_2020<-as.data.frame(edsm_2020)

comparedf(edsm_combined, edsm_2020)
str(edsm_2020)
str(edsm_combined)

#Combine EDI data with 2020 data (NOTE: only Summer-Fall data between June and October)
edsm_combined<-rbind(edsm_combined,edsm_2020)
edsm_combined<-edsm_combined%>% filter(GearConditionCode %in% c(1,2)) 

unique(edsm_combined$Stratum)
unique(edsm_combined$SubRegion)

#Strata based on Catherine's figure:
#Suisun Marsh: Suisun Marsh and Grizzly Bay subregions
#Lower Sacramento: Sac River near Rio Vista, Lower Sac Rivere
#Suisun Bay: West, Mid Suisun Bay, Honker Bay, and Confluence

#Check sampling effort across years
sampling_effort <- edsm_combined %>% group_by(Region,SubRegion,Stratum,Station,Date,Tow) %>% summarise(Count=sum(SumOfCatchCount,na.rm = T))
sampling_effort$TowCount=1
sampling_effort$Year=year(sampling_effort$Date)

sampling_effort <- sampling_effort %>% filter(Stratum %in% c("Suisun Marsh","Suisun Bay","Lower Sacramento")) %>% group_by(Year,Stratum)%>% summarise(TowCount=sum(TowCount,na.rm = T))
#Relatively consistent sampling between 2018 and 2020, but need to remove 2017 since only Lower Sac was sampled then

#Create dataset based on just SMSCG regions of interest and remove 2017
edsm_subset<- edsm_combined %>% filter(Stratum %in% c("Suisun Marsh","Suisun Bay","Lower Sacramento")) %>% filter(year(Date)>2017)

#Configure data, with CPUE as the value for use in analysis
str(edsm_subset)
edsm_subset<- edsm_subset %>% select(Region,SubRegion,Stratum,Station,Date,Time,Dur,Tow,TargetLat,TargetLong,Dir,Volume,OrganismCode,SumOfCatchCount) %>%
  group_by(Region,SubRegion,Stratum,Station,Date,Time,Dur,Tow,TargetLat,TargetLong,Dir,Volume,OrganismCode) %>% summarise(Count=sum(SumOfCatchCount)) %>%
  mutate(CPUE=Count/Volume) %>% select(-Count) %>% spread(OrganismCode, CPUE)

#Remove data without volume
edsm_subset<- edsm_subset %>% filter(complete.cases(Volume))

#Add zeroes to NA
edsm_subset<-as.data.frame(edsm_subset)
edsm_subset[is.na(edsm_subset)] <- 0
str(edsm_subset)

#Check organism count data
organism_total <- edsm_combined %>% filter(Stratum %in% c("Suisun Marsh","Suisun Bay","Lower Sacramento")) %>% filter(year(Date)>2017) %>%
  group_by(OrganismCode,CommonName) %>% summarise(Count=sum(SumOfCatchCount))

#Select non-fish species
#Set arbitrary cutoff numbers at less than 50 count total in the dataset
list_organism_to_be_removed<- organism_total %>% mutate(FishOrNot=ifelse(OrganismCode %in% c("MMARGI","EXP","Cspp","NOFISH","BVIRGI","PBACHE","ORSH","PPENIC","Pspp","DSH","SHRIMP","UNID"),"NO","YES")) %>%
  filter(Count<50|FishOrNot=="NO")

#Remove non-fish and rare species from subset data
edsm_subset_v2 <-edsm_subset %>% select(-one_of(list_organism_to_be_removed$OrganismCode))

####Summarize by station because running analysis for each tow took forever
str(edsm_subset_v2)
edsm_subset_v2 <-edsm_subset_v2 %>% select(-one_of(c("Time","Dur","Tow","TargetLat","TargetLong","Volume","Dir")))
edsm_subset_v2 <- edsm_subset_v2 %>% group_by(Region,Stratum,SubRegion,Station,Date) %>% summarise_all(funs(mean))

#Remove samples with no catch
edsm_subset_v2 <-edsm_subset_v2[rowSums(edsm_subset_v2[, -(1:12)])>0, ]
#This removed ~400 samples

str(edsm_subset_v2)
edsm_subset_v2$Stratum<-as.factor(edsm_subset_v2$Stratum)
edsm_subset_v2$Year<-as.factor(year(edsm_subset_v2$Date))
edsm_subset_v2$Month<-as.factor(month(edsm_subset_v2$Date))

#Create data for PERMANOVA
edsm.fish.data<-edsm_subset_v2[,c(6:17)]
edsm.env.data<-edsm_subset_v2[,c("Stratum","Year","Month")]
edsm.fish.data_sqrt<-sqrt(edsm.fish.data)

#PERMANOVA using adonis function
permanova_edsm<-adonis2(edsm.fish.data_sqrt ~ Year + Month + Stratum, data=edsm.env.data, permutations=999,by = "margin")
permanova_edsm

#SIMPER Analysis--------
#SIMPER by month
(sim_month <- with(edsm.env.data, simper(edsm.fish.data_sqrt, Month)))
summary(sim_month)

#SIMPER by year
(sim_fish_year <- with(edsm.env.data, simper(edsm.fish.data_sqrt, Year)))
summary(sim_fish_year)

#SIMPER by Strata
(sim_fish_strata <- with(edsm.env.data, simper(edsm.fish.data_sqrt, Stratum)))
summary(sim_fish_strata)


#########
#Pairwise adonis function
pairwise.adonis <- function(x,factors, sim.method, p.adjust.m)
{
  co = as.matrix(combn(unique(factors),2))
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in% c(as.character(co[1,elem]),as.character(co[2,elem])),] ~
                  factors[factors %in% c(as.character(co[1,elem]),as.character(co[2,elem]))] ,
                permutations = 9999, method =sim.method);
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}

pairwise.adonis(edsm.fish.data_sqrt,edsm.env.data$Stratum,sim.method="mahalanobis",p.adjust.m = "bonferroni")
#pairs   F.Model          R2 p.value p.adjusted
#1   Suisun Bay vs Lower Sacramento  9.334525 0.03415055   1e-04      3e-04
#2       Suisun Bay vs Suisun Marsh  3.446285 0.01068794   1e-04      3e-04
#3 Lower Sacramento vs Suisun Marsh 12.169770 0.03388306   1e-04      3e-04

pairwise.adonis(edsm.fish.data_sqrt,edsm.env.data$Year,sim.method="mahalanobis",p.adjust.m = "bonferroni")
#pairs  F.Model         R2 p.value p.adjusted
#1 2018 vs 2019 4.272799 0.01237514   1e-04      3e-04
#2 2018 vs 2020 4.583253 0.01719258   1e-04      3e-04
#3 2019 vs 2020 6.978350 0.02089462   1e-04      3e-04

#NMDS, Stress level continues to be high at .17-.19; ignore for now
#edsm.fish.nmds=metaMDS(edsm.fish.data_sqrt,dist="bray",k=2, try = 900, trymax = 1000,autotransform=F,plot = T)
#edsm.fish.nmds

#########################################
###---SUISUN MARSH ANALYSIS ONLY
edsm_subset_v3 <- edsm_subset_v2 %>% filter(SubRegion=="Suisun Marsh")

#Create data 
edsm.sm.fish.data<-edsm_subset_v3[,c(6:17)]
edsm.sm.env.data<-edsm_subset_v3[,c("Year","Month")]
edsm.sm.fish.data_sqrt<-sqrt(edsm.sm.fish.data)

#PERMANOVA using adonis function
permanova_edsm_suisunmarsh<-adonis2(edsm.sm.fish.data_sqrt ~ Year + Month, data=edsm.sm.env.data, permutations=999,by = "margin")
permanova_edsm_suisunmarsh

#NMDS
edsm.fish.suisun.marsh.nmds=metaMDS(edsm.sm.fish.data_sqrt,dist="bray",k=2, try = 99, trymax = 100,autotransform=F,plot = T)
edsm.fish.suisun.marsh.nmds

#Start graph process
data.scores <- as.data.frame(scores(edsm.fish.suisun.marsh.nmds)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores

###
data.scores$Year<-edsm.sm.env.data$Year
data.scores$Month<-edsm.sm.env.data$Month
str(data.scores)

species.scores <- as.data.frame(scores(edsm.fish.suisun.marsh.nmds, "species")) #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores) # create a column of species, from the rownames of species.scores
head(species.scores)

#FOR VECTORS
vf<-envfit(edsm.fish.suisun.marsh.nmds, edsm.sm.fish.data_sqrt, perm=999)
vf

#So the r2 data is used to scale the values in columns NMDS1 and NMDS2. The final plot is produced with:
spp.scrs <- as.data.frame(scores(vf, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))
spp.scrs <- spp.scrs[order(spp.scrs$Species),]
spp.scrs

#Let's remove the species that don't matter, check the vf object for info
#Only ones with p-value <0.01 for now
spp.scrs.shiny<-spp.scrs[c("AMS","BKS","MSS","NAN","SPLT","STB","TFS","TSM","TSS","WAG"),]
spp.scrs.shiny

spp.scrs.shiny$NMDS1<-spp.scrs.shiny$NMDS1*(max(data.scores$NMDS1))
spp.scrs.shiny$NMDS2<-spp.scrs.shiny$NMDS2*(max(data.scores$NMDS2))

#Set palette
cbbPalette <- c("#e6194B", "#4363d8", "#ffe119")

ggMDS_year<- ggplot() + geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,fill=Year),size=8,colour="black",shape=21,alpha=0.4) +
  theme_bw() + geom_segment(data = spp.scrs.shiny, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  scale_fill_manual(values=cbbPalette) + 
  geom_text(data = spp.scrs.shiny, aes(x = NMDS1, y = NMDS2, label = Species),size=5,nudge_x = .001,nudge_y = -.005) +
  theme(axis.text.x = element_text(size=19, color="black"),axis.text.y = element_text(size=19, color="black")) +
  theme(legend.background=element_rect(colour="black"),legend.position = c(0.85,0.75),legend.title=element_text(size=20),legend.text=element_text(size=20))+
  theme(axis.text.x = element_text(size=21, color="black"),axis.text.y = element_text(size=20, color="black"),axis.title.x = element_text(size = 27),axis.title.y = element_text(size = 27)) +
  annotate("text", size=5, x = -1.1, y = 2, label = c(paste("2D Stress:",round(edsm.fish.suisun.marsh.nmds$stress,2),sep=" ")))
ggMDS_year


#Print figure
tiff(filename=file.path(output_root,"Figure_NMDS_EDSM_SuisunMarsh.tiff"),
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=9, #22*1, 
     pointsize=5, #12, 
     res=300,
     compression="lzw")
ggMDS_year
dev.off()

