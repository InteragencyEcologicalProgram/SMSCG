#Get the Fall Midwater Trawl data to put together
#with the gate operation data

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(lubridate)
library(visreg)

#FMWT data is avaialable here: ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/
#I will eventually remember how to automatically download it,
#but in the meantime, I"ll just do it myself.

#uplaod the fish catch data
                   
FMWT <- read_excel("FMWT 1967-2018 Catch Matrix_updated.xlsx", sheet = "FlatFile", 
                   col_types = c("numeric","date", "numeric", "text", "date",  
                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text", "text", "text", rep("numeric", times =112)))

#put it in long format instead of wide
FMWTl = gather(FMWT, key = "Species", value = "catch", `Aequorea spp.`:`Yellowfin Goby`)

#rename all the stupid names
names(FMWTl)
names(FMWTl) = c("Year" ,"Date","Survey","Station", "StartTime","Index", 
                 "TopTemp", "TopEC","BottomEC","Turb",
                 "Secchi" , "Depthft","TowVolume","Tide","TowDirection",
                 "Weather","Microcystis","Wave", "Species" , "catch" )

#Ideally, we'd do this analysis on CPUE instead of raw catch
#They didn't caculate volumes until later, so I'll use the average
#volume per station for the older tows

#first replace any zero volumes with NAs, because zero volumes don't make sense
FMWTl$TowVolume[which(FMWTl$TowVolume==0)] = NA

meanvol = group_by(FMWTl, Station) %>% summarize(mvol = mean(TowVolume, na.rm = T))
FMWTl2 = merge(FMWTl, meanvol)
FMWTl2$TowVolume[which(is.na(FMWTl2$TowVolume))] = FMWTl2$mvol[which(is.na(FMWTl2$TowVolume))]

#Calculate CPUE 
FMWTl2 = mutate(FMWTl2, CPUE = catch*TowVolume)

#For starters, I'll just look at Delta Smelt
FMWT_DS = filter(FMWTl2, Species == "Delta Smelt")

#Just Delta Smelt from the stations in MOntezuma Slough
#Note: I need to find out which side of the gates staion 608 is on
FMWT_DSm = filter(FMWT_DS, Station == 605 |Station == 606| Station == 608 )

#load the water quality and gate operations data
load("~/salinity control gates/SMSCG/operations.RData")
load("~/salinity control gates/SMSCG/waterquality.RData")

#merge the gate operations with the fish data
FMWT_DSm$Date = as.Date(FMWT_DSm$Date)
op.daily$Date = as.Date(op.daily$Date)
FMWT_DSmg = merge(FMWT_DSm, op.daily, by = "Date", all.x = T)

#quick exploritory plots
op.daily$julian = yday(op.daily$Date)
ggplot(op.daily, aes(x=julian, fill = Operating)) + geom_bar(stat = "Count")
ggplot(op.daily, aes(x=Date, fill = Operating)) + geom_bar(stat = "Count")

#now with fish, first just the time we have gate data
FMWT_DSmg2 = filter(FMWT_DSmg, !is.na(Operating))
ggplot(FMWT_DSmg2, aes(x = Operating, y = log(CPUE+1))) + geom_boxplot()

#try catch instead of CPUE
ggplot(FMWT_DSmg2, aes(x = Operating, y = log(catch+1))) + geom_boxplot()

ggplot(FMWT_DSmg2, aes(x = Operating, y = log(catch+1))) + geom_boxplot() + facet_wrap(~Station)

#model it
dsglm = glm(catch~ Station + Operating + TopEC, family = poisson, data = FMWT_DSmg2)
summary(dsglm)
visreg(dsglm)

#But we are probably zero-inflated and probably overdisperssed too.
#look at overdispersion
dsglm2 = glm(catch~ Station + Operating + TopEC, family = quasipoisson, data = FMWT_DSmg2) 
summary(dsglm2)$dispersion # dispersion coefficient
pchisq(summary(dsglm2)$dispersion * dsglm$df.residual, dsglm$df.residual, lower = F)
#it's very overdisperssed

dsglm3 = glm(round(CPUE)~ Station + Operating + TopEC, family = quasipoisson, data = FMWT_DSmg2) 
summary(dsglm3)$dispersion # dispersion coefficient
#CPUE is even worse

summary(dsglm2)
summary(dsglm3)

#maybe a hurdle model
library(pscl)
library(MASS)
dshurdle = hurdle(catch~ Station + Operating + TopEC, data = FMWT_DSmg2)
summary(dshurdle)

#might be better with a negative binomial model for the CPUE
dshurdle2 = hurdle(round(CPUE)~ Station + Operating + TopEC, dist = "negbin", data = FMWT_DSmg2)
summary(dshurdle2)
dshurdle3 = hurdle(catch~ Station + Operating + TopEC, dist = "negbin", data = FMWT_DSmg2)
summary(dshurdle3)
#I'm not quite sure why the EC is giving me NAs

