#Get the Fall Midwater Trawl data to put together
#with the gate operation data

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(lubridate)
library(visreg)
library(pscl)
library(MASS)
library(MuMIn)

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

#we may want to analyze year as a factor instead of continuous
FMWTl$Year2 = as.factor(FMWTl$Year)

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


#Change the "operating" values so there are fewer groups


op.daily = op.daily %>% mutate(Operating2 = case_when(
  str_detect(Operating, "(Operating normally)|(Operating with one or more gates closed)") ~ "Operating",
  str_detect(Operating, "(Operating with special conditions)|(Operating with one or more gates open)|(Operating with flashboards out)") ~ "Operating",
  str_detect(Operating, "(Open$)|(Open with flashboards in)|(Open with one or more gates closed)") ~ "Open",
  str_detect(Operating, "(Closed with flashboards in)|(Closed with flashboards out)") ~ "Operating"
))



#merge the gate operations with the fish data
FMWT_DSm$Date = as.Date(FMWT_DSm$Date)
op.daily$Date = as.Date(op.daily$Date)
FMWT_DSmg = merge(FMWT_DSm, op.daily, by = "Date", all.x = T)
FMWT_DSmg$julian = yday(FMWT_DSmg$Date)
FMWT_DSmg$Operating2 = as.factor(FMWT_DSmg$Operating2)

#ggplot(op.daily, aes(x=julian, fill = Operating2)) + geom_bar(stat = "Count")
#ggplot(op.daily, aes(x=Date, fill = Operating2)) + geom_bar(stat = "Count")
  ggplot(op.daily, aes(x=julian, fill = Operating2)) + geom_bar(stat = "Count", position = "fill") +
    facet_wrap(.~Year, scales = "free") + scale_y_discrete(breaks = NULL)

#now with fish, first just the time we have gate data, and just the fall because we have more trawls in the fall
FMWT_DSmg2 = filter(FMWT_DSmg, !is.na(Operating), julian >200, Year < 2012)
ggplot(FMWT_DSmg2, aes(x = Operating2, y = log(CPUE+1))) + geom_boxplot()

ggplot(FMWT_DSmg2, aes(x=catch)) + geom_histogram()

#try catch instead of CPUE
#ggplot(FMWT_DSmg2, aes(x = Operating2, y = log(catch+1))) + geom_boxplot()

#ggplot(FMWT_DSmg2, aes(x = Operating2, y = log(catch+1))) + geom_boxplot() + facet_wrap(~month(Date), scales = "free_x")

#But we are probably zero-inflated and probably overdisperssed too.
#look at overdispersion
dsglm2 = glm(catch~ Station + Operating + TopEC, family = quasipoisson, data = FMWT_DSmg2) 
summary(dsglm2)$dispersion # dispersion coefficient
pchisq(summary(dsglm2)$dispersion * dsglm$df.residual, dsglm$df.residual, lower = F)
#it's very overdisperssed

dsglm3 = glm(round(CPUE)~ Station + Operating + TopEC, family = quasipoisson, data = FMWT_DSmg2) 
summary(dsglm3)$dispersion # dispersion coefficient
#CPUE is even worse
#maybe a hurdle model

###########################################################################################################

ggplot(data = FMWT_DSmg2, aes(x=julian, y=TopEC)) + facet_wrap(~Operating) + geom_point()
#Some of those conductivity values look way off. I'm going to check against the water quality from the sondes

histday = filter(historical.daily, Analyte == "Salinity")
FMWT_DSmg2a = mutate(FMWT_DSmg2, salinity = TopEC*0.64/1000, Datetime = Date)
FMWTwSal = merge(filter(FMWT_DSmg2a, Station != 608), histday[,-1], by = "Datetime")

ggplot(FMWTwSal, aes(x=salinity, y= Mean, color = Station)) + 
  geom_point() + xlab("Salinity from Sonde at Beldens") +
  ylab("Salinity measured by FMWT")
#so it's close, but not great.


FMWT_DSmg2$fishStation = FMWT_DSmg2$Station
FMWT_DSmg2$Station[which(FMWT_DSmg2$Station == 608)] = "(S-71)  Montezuma Slough at Roaring River"
FMWT_DSmg2$Station[which(FMWT_DSmg2$Station == 606)] = "(S-49)  Beldens Landing"
FMWT_DSmg2$Station[which(FMWT_DSmg2$Station == 605)] = "(S-54)  Hunter Cut" 
FMWT_DSmg2 = mutate(FMWT_DSmg2, Datetime = Date)

FMWTwSal2 = unique(merge(FMWT_DSmg2, histday))

ggplot(FMWTwSal2, aes(x=salinity, y= Mean, color = Station)) + 
  geom_point() + ylab("Salinity from nearest Sonde") +
  xlab("Salinity measured by FMWT") + geom_smooth(method = lm)
#so it's close, but not great.

sal4 = lm(Mean~Operating*julian + Station, data = FMWTwSal2)
summary(sal4)

############################################################################################
#unfortunatley, we can't sure sonde salnity for the whole
#dataset

#we had lower catch in some years than others, let's put year in there too

#I don't really want to use the quasi-poisson version, or plain old neg binomial, because of the zero-inflation problem.
dsglm2 = glm(catch~ Station + Operating2*julian+ TopEC + Year, family = quasipoisson, data = FMWT_DSmg2) 
summary(dsglm2)

dsnb2 = glm.nb(catch~ Station + Operating2*julian+ TopEC + Year,  data = FMWT_DSmg2) 
summary(dsnb2)

#The zero-inflated model might help with teh overdispersion too.
dszip3 = zeroinfl(catch~ Operating2*julian + TopEC + Year, dist = "poisson", data = FMWT_DSmg2)
summary(dszip3)
visreg(dszip3)
visreg(dszip3, xvar = "julian", by = "Operating")

dszip4 = zeroinfl(catch~ Station + Operating2+julian + TopEC + Year, dist = "negbin", data = FMWT_DSmg2)
summary(dszip4)
visreg(dszip4)
visreg(dszip4, xvar = "julian", by = "Operating")

#compare the zeroinflated models with a likelihood ratio test to see if we've delt with the overdispersion
library(lmtest)
lrtest(dszip3, dszip4)
#it's telling me I'm still overdisperssed, I should use the negative binomial version

#hurdle models don't differentiate between "false" zeros, and "real" zeros, so I don't really want that.
dshurdle = hurdle(catch~ Station + Operating*julian + Mean + Year, data = FMWTwSal2)
summary(dshurdle)

dshurdle2 = hurdle(catch~ Station + Operating*julian + Mean + Year, dist  ="negbin", data = FMWTwSal2)
summary(dshurdle2)
#Putting the year affect in there makes it less cute

# Do not write in your report or paper that you used a quasi-Poisson distribution. Just say that
#you did a Poisson GLM, detected overdispersion, and corrected the standard errors
#using a quasi-GLM model where the variance is given by φ × μ, where μ is the
#mean and φ the dispersion parameter. Zuur et al. 2009

#####################################################################################################

dszip5 = zeroinfl(catch~ Station + Operating2*julian + 
                    TopEC + Year, dist = "negbin", data = FMWT_DSmg2)
dszip5a = zeroinfl(catch~ Station + Operating2+julian + 
                     TopEC + Year, dist = "negbin", data = FMWT_DSmg2)
dszip5b = zeroinfl(catch~ Station + julian + TopEC +
                     Year, dist = "negbin", data = FMWT_DSmg2)
dszip5c = zeroinfl(catch~ Station + Operating2*julian + 
                     TopEC, dist = "negbin", data = FMWT_DSmg2)
dszip5d = zeroinfl(catch~ Station +   Operating2 +
                     Year, dist = "negbin", data = FMWT_DSmg2)
dszip5e = zeroinfl(catch~ Station +  TopEC +
                     Year, dist = "negbin", data = FMWT_DSmg2)
dszip5f = zeroinfl(catch~ Station +  julian + Operating2, dist = "negbin", data = FMWT_DSmg2)

dszip5g = zeroinfl(catch~ Station +  TopEC, dist = "negbin", data = FMWT_DSmg2)

dszip5h = zeroinfl(catch~ Year + TopEC, dist = "negbin", data = FMWT_DSmg2)

dszip5i = zeroinfl(catch~ julian+Operating2 + Year, dist = "negbin", data = FMWT_DSmg2)


AIC(dszip5, dszip5a, dszip5c, dszip5b, dszip5h,  dszip5i)
summary(dszip5b)

visreg(dszip5)
visreg(dszip5, xvar = "julian", by = "Operating2")
visreg(dszip5, xvar = "Operating2", by = "julian")

summary(dszip5a)
visreg(dszip5a)

##################################################################################
#Let's try adding in water year type.

yrtyp = read_excel("wtryrtype.xlsx")

#Year type should be an ordered factor
yrtyp$`Yr-type` = factor(yrtyp$`Yr-type`, levels = c("C", "D", "BN", "AN", "W"), ordered = T)

#Make "WY" match the "Year" comlum of the FMWT dataset
yrtyp = mutate(yrtyp, Year = WY, WY = NULL)

#Ted recommended lumping these into groups instead of using all the year types
yrtyp = mutate(yrtyp, YT2 = `Yr-type`) 
yrtyp$YT2[which(yrtyp$YT2 == "C")] = "D"
yrtyp$YT2[which(yrtyp$YT2 == "BN")] = "D"
yrtyp$YT2[which(yrtyp$YT2 == "AN")] = "W"
#attatch the year types to the data

FMWT_DSmg3 = merge(FMWT_DSmg2, yrtyp, by = "Year")


#Rerun the models with water year type


dszip6 = zeroinfl(catch~ Station + Operating2*julian + YT2 +
                    TopEC +  Year, dist = "negbin", data = FMWT_DSmg3, na.action = "na.fail")
dszip6a = zeroinfl(catch~ Station + Operating2+julian + 
                     TopEC + Year + YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6b = zeroinfl(catch~ Station + julian + TopEC +
                     Year + YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6c = zeroinfl(catch~ Station + Operating2*julian + 
                     TopEC + YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6d = zeroinfl(catch~ Station +   Operating2 +
                     Year + YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6e = zeroinfl(catch~ Station +  TopEC +
                     Year + YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6f = zeroinfl(catch~ Station +  julian + Operating2 + YT2, dist = "negbin", data = FMWT_DSmg3)

dszip6g = zeroinfl(catch~ Station +  TopEC + YT2, dist = "negbin", data = FMWT_DSmg3)

dszip6h = zeroinfl(catch~ Year + TopEC + YT2, dist = "negbin", data = FMWT_DSmg3)

dszip6i = zeroinfl(catch~ julian+Operating2 + Year + YT2, dist = "negbin", data = FMWT_DSmg3)

dszip6j = zeroinfl(catch~ Station + Operating2*julian + 
                    TopEC + YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6k = zeroinfl(catch~ Station + Operating2+julian + 
                     TopEC + YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6m = zeroinfl(catch~ Station + Operating2+ 
                     TopEC + YT2, dist = "negbin", data = FMWT_DSmg3)

dszip6l = zeroinfl(catch~ Station + julian + TopEC +
                      YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6n = zeroinfl(catch~ Station +   Operating2 +
                      YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6o = zeroinfl(catch~ Station +  TopEC +
                    YT2, dist = "negbin", data = FMWT_DSmg3)
dszip6q = zeroinfl(catch~ Station +  TopEC + YT2, dist = "negbin", data = FMWT_DSmg3)

dszip6r = zeroinfl(catch~ TopEC + YT2, dist = "negbin", data = FMWT_DSmg3)

dszip6s = zeroinfl(catch~ julian+Operating2 + YT2, dist = "negbin", data = FMWT_DSmg3)

foo = AIC(dszip6, dszip6a, dszip6c, dszip6b, dszip6f, dszip6h,  dszip6i, dszip5, dszip6j, dszip6k, dszip6l, 
    dszip6n, dszip6o, dszip6q, dszip6r, dszip6s,
    dszip5a, dszip5c, dszip5b,  dszip5h,  dszip5i)

library(MuMIn)
drezip = dredge(dszip6)

foo = foo[order(foo$AIC),]
foo
summary(dszip6a)
visreg(dszip6a)

summary(dszip6k)
visreg(dszip6k)

summary(dszip6m)
visreg(dszip6m)

########################################################################################
#The effect of operations seems to decline in Decembr for some reason, maybe because
#everything is really fresh. I might try to see if there is a better relationship with jus
#September-November

FMWT_DSmg3a = filter(FMWT_DSmg3, julian <320)
dszip7 = zeroinfl(catch~ Station + Operating2*julian + YT2 +
                    TopEC +  Year, dist = "negbin", data = FMWT_DSmg3a, na.action = "na.fail")
drezip2 = dredge(dszip7)

drezip7best = zeroinfl(catch~ Station+Operating2+julian + YT2 + TopEC, 
                       dist = "negbin", data = FMWT_DSmg3a, na.action = "na.fail")
summary(drezip7best)
visreg(drezip7best)
visreg(drezip7best, by = "Operating2", xvar = "julian")
#meh. Doesn't help much

#####################################################################################
#Quick plot of FMWT DS catch in montezuma slough by year

tots = group_by(FMWT_DSmg2, Year) %>% summarize(catch = sum(catch))
index = read.csv("smeltindex.csv")

ggplot(tots, aes(x=Year, y= catch)) + geom_bar(stat = "identity")
ggplot(index, aes(x=Year, y= Total)) + geom_bar(stat = "identity")

#####################################################################################'
#I'm still confused as to why we are getting a bunch of singularities. I'm going to try
#replacing day of year with month.

FMWT_DSmg3$month = month(FMWT_DSmg3$Date)
dszip8 = zeroinfl(catch~ Station + Operating2*month + YT2 +
                    TopEC, dist = "negbin", data = FMWT_DSmg3, na.action = "na.fail")
drezip3 = dredge(dszip8)
drezip3best = zeroinfl(catch~ Station + Operating2+month + YT2 +
                         TopEC, dist = "negbin", data = FMWT_DSmg3)
summary(drezip3best)
#meh. Not much help.

##################################################################################
#adding X2
X2 <- read_excel("supplemental_data_wr.1943-5452.0000617_hutton3.xlsx", sheet = "Daily")
X2$Date = as.Date(X2$Date)
FMWT_DSmg4 = merge(FMWT_DSmg3, X2)

dszip9 = zeroinfl(catch~ Station + Operating2+ julian + YT2 +
                    TopEC +  SacX2, dist = "negbin", data = FMWT_DSmg4, na.action = "na.fail")
drezip4 = dredge(dszip9)
#also not much help

###########################################################################################
#what about adding the FMWT index?
FMWT_DSmg4 = merge(FMWT_DSmg3, index)

dszip10 = zeroinfl(catch~ Station + Operating2+ julian + YT2 +
                    TopEC +  Total, dist = "negbin", data = FMWT_DSmg4, na.action = "na.fail")
drezip5 = dredge(dszip10)
summary(dszip10)
visreg(dszip10)

dszip10a = zeroinfl(catch~  Operating2+  YT2 +
                     TopEC +  Total, dist = "negbin", data = FMWT_DSmg4, na.action = "na.fail")
#dang. Now it's not significant at all. But I'm not sure how to interpret this anyway

##########################################################################################
#whay about just dry years?
FMWT_DSmg3b = filter(FMWT_DSmg3, YT2 == "D")
dszip11 = zeroinfl(catch~ Station + Operating2+julian + 
                    TopEC +  Year, dist = "negbin", data = FMWT_DSmg3b, na.action = "na.fail")

drezip6 = dredge(dszip11)
dszip11best = zeroinfl(catch~ Operating2+julian + 
                     TopEC, dist = "negbin", data = FMWT_DSmg3b, na.action = "na.fail")
summary(dszip11best)
visreg(dszip11best)
#huh

summary(dszip11)
visreg(dszip11)


#################################################################################
#Quick plot of year type versus gate operation
ggplot(FMWT_DSmg3, aes(x=Operating2)) + geom_bar() + facet_wrap(~YT2)


ops = filter(merge(op.daily, yrtyp), Year >1988, Year <2019)
ops$Operating2 = as.factor(ops$Operating2)

opsum = group_by(ops, Year, YT2) %>% summarize(ops = length(Operating2[which(Operating2 == "Operating")]))
opsmean = group_by(opsum, YT2) %>% summarize(mean = mean(ops), se = sd(ops)/length(ops), sd = sd(ops))

#plot again, better
ggplot(opsmean, aes(x=YT2, y= mean, fill = YT2)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) + ylab("days of operation") +
  scale_x_discrete(name = "Year type", labels = c("Dry", "Wet"))

#quick binomial model
opmod = glm(Operating2 ~ YT2 + julian, data = ops, family = "binomial")
summary(opmod)
visreg(opmod)

#or maybe make it easier
opmod2 = glm(ops~ YT2, data = opsum)
summary(opmod2)
visreg(opmod2)

