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

op.daily = op.daily %>% mutate(Operating = case_when(
  str_detect(Operating, "(Operating normally)|(Operating with one or more gates closed)") ~ "Operating",
  str_detect(Operating, "(Operating with special conditions)|(Operating with one or more gates open)|(Operating with flashboards out)") ~ "Operating partially",
  str_detect(Operating, "(Open$)|(Open with flashboards in)|(Open with one or more gates closed)") ~ "Open",
  str_detect(Operating, "(Closed with flashboards in)|(Closed with flashboards out)") ~ "Closed"
))

#merge the gate operations with the fish data
FMWT_DSm$Date = as.Date(FMWT_DSm$Date)
op.daily$Date = as.Date(op.daily$Date)
FMWT_DSmg = merge(FMWT_DSm, op.daily, by = "Date", all.x = T)
FMWT_DSmg$julian = yday(FMWT_DSmg$Date)
FMWT_DSmg$Operating2 = as.factor(FMWT_DSmg$Operating2)

#quick exploritory plots
op.daily$julian = yday(op.daily$Date)
ggplot(op.daily, aes(x=julian, fill = Operating2)) + geom_bar(stat = "Count")
ggplot(op.daily, aes(x=Date, fill = Operating2)) + geom_bar(stat = "Count")

#now with fish, first just the time we have gate data, and just the fall because we have more trawls in the fall
FMWT_DSmg2 = filter(FMWT_DSmg, !is.na(Operating), julian >200)
ggplot(FMWT_DSmg2, aes(x = Operating, y = log(CPUE+1))) + geom_boxplot()

#try catch instead of CPUE
ggplot(FMWT_DSmg2, aes(x = Operating, y = log(catch+1))) + geom_boxplot()

ggplot(FMWT_DSmg2, aes(x = Operating2, y = log(catch+1))) + geom_boxplot() + facet_wrap(~month(Date), scales = "free_x")

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

dshurdle = hurdle(catch~ Station + Operating + TopEC, data = FMWT_DSmg2)
summary(dshurdle)

#might be better with a negative binomial model for the CPUE
dshurdle2 = hurdle(round(CPUE)~ Station + Operating + TopEC, dist = "negbin", data = FMWT_DSmg2)
summary(dshurdle2)
dshurdle3 = hurdle(catch~ Station + Operating + TopEC, dist = "negbin", data = FMWT_DSmg2)
summary(dshurdle3)
#I'm not quite sure why the EC is giving me NAs

#a zero-inflated model might be better
dszinb = zeroinfl(round(CPUE)~ Station + Operating + TopEC, dist = "negbin", data = FMWT_DSmg2)
summary(dszinb)

dszip = zeroinfl(round(CPUE)~ Station + Operating, dist = "poisson", data = FMWT_DSmg2)
summary(dszip)
dszip2 = zeroinfl(catch~ Station + Operating, dist = "poisson", data = FMWT_DSmg2)
summary(dszip2)

############################################################################################################
#There is probably some wierd stuff going on with an interaction between gate operations and date too.
#I'm not exactly sure how to account for it.

dszip3 = zeroinfl(catch~ Station + Operating*julian, dist = "poisson", data = FMWT_DSmg2)
summary(dszip3)
visreg(dszip3)
visreg(dszip3, xvar = "julian", by = "Operating")

dsglm2 = glm(catch~ Station + Operating*julian+ TopEC, family = quasipoisson, data = FMWT_DSmg2) 
summary(dsglm2)
visreg(dsglm2)
visreg(dsglm2, xvar = "julian", by = "Operating")
#bleh

#let's try plotting that
ggplot(FMWT_DSmg2, aes(x = julian, y = catch)) + geom_point() + facet_wrap(~Operating2) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson"))+ coord_cartesian(ylim = c(-1, 20))

###########################################################################################################

#Maybe a binomial model of smelt presence/absence would be more informative.

FMWT_DSmg2$DS = as.logical(FMWT_DSmg2$catch)
dsb = glm(DS~ Station + Operating2*julian+ TopEC, family = binomial, data = FMWT_DSmg2) 
summary(dsb)
visreg(dsb)
visreg(dsb, xvar = "julian", by = "Operating")
#No. Definitely not.

dsb = glm(DS~ Station + Operating+TopEC, family = binomial, data = FMWT_DSmg2) 
summary(dsb)
visreg(dsb)
visreg(dsb, xvar = "julian", by = "Operating")


#is salinity correlated to day of the year?
sal = lm(TopEC~julian, data = FMWT_DSmg2)
summary(sal)
sal2 = lm(TopEC~Operating*julian, data = FMWT_DSmg2)
summary(sal2)
#OK, now I'm just confused. Maybe station 608 is throwing us off?
sal2 = lm(TopEC~Operating*julian, data = filter(FMWT_DSmg2, Station != 608))
summary(sal2)

#block by statin
sal3 = lm(TopEC~Operating*julian + Station, data = FMWT_DSmg2)
summary(sal3)


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
#rerun our catch models with the sonde salinity

#we had lower catch in some years than others, let's put year in there too

#I don't really want to use the quasi-poisson version, or plain old neg binomial, because of the zero-inflation problem.
dsglm2 = glm(catch~ Station + Operating*julian+ Mean + Year, family = quasipoisson, data = FMWTwSal2) 
summary(dsglm2)

dsnb2 = glm.nb(catch~ Station + Operating*julian+ Mean + Year,  data = FMWTwSal2) 
summary(dsnb2)

#The zero-inflated model might help with teh overdispersion too.
dszip3 = zeroinfl(catch~ Station + Operating*julian + Mean + Year, dist = "poisson", data = FMWTwSal2)
summary(dszip3)
visreg(dszip3)
visreg(dszip3, xvar = "julian", by = "Operating")

dszip4 = zeroinfl(catch~ Station + Operating*julian + Mean + Year, dist = "negbin", data = FMWTwSal2)
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
#OK, so I need a zero-inflated negative binomial model. Which is best?
dszip4 = zeroinfl(catch~ Station + Operating*julian + 
                    Mean + Year, dist = "negbin", data = FMWTwSal2)
dszip4a = zeroinfl(catch~ Station + Operating+julian + 
                    Mean + Year, dist = "negbin", data = FMWTwSal2)
dszip4b = zeroinfl(catch~ Station + julian + Mean +
                    Year, dist = "negbin", data = FMWTwSal2)
dszip4c = zeroinfl(catch~ Station + Operating*julian + 
                    Mean, dist = "negbin", data = FMWTwSal2)
dszip4d = zeroinfl(catch~ Station +   Operating +
                     Year, dist = "negbin", data = FMWTwSal2)
dszip4e = zeroinfl(catch~ Station +  Mean +
                     Year, dist = "negbin", data = FMWTwSal2)
dszip4f = zeroinfl(catch~ Station +  julian, dist = "negbin", data = FMWTwSal2)

dszip4g = zeroinfl(catch~ Station +  Mean, dist = "negbin", data = FMWTwSal2)

dszip4g = zeroinfl(catch~ Year + Mean, dist = "negbin", data = FMWTwSal2)

dszip4g = zeroinfl(catch~ julian+Operating + Year, dist = "negbin", data = FMWTwSal2)


AIC(dszip4, dszip4a,dszip4b, dszip4c, dszip4d, dszip4e, dszip4f, dszip4g)
#the model with Station, salinity, and year is the best so far.
summary(dszip4e)
#but I don't know why we have NAs for the "year" standard error
foo = vcov(dszip4e)
diag(vcov(dszip4e))
sqrt(diag(vcov(dszip4e)))
#Apparently it's because I have a negative number on the diagonal of my vcov matrix
#but I don't knwo what causes that.

tots = group_by(FMWTwSal2, Year, Station) %>% summarise(tot = sum(catch))

# I think it is because there was zero catch in later years

FMWTwSal3 = filter(FMWTwSal2, Year < 2012)
##########################################################################################

dszip4 = zeroinfl(catch~ Station + Operating*julian + 
                    Mean + Year, dist = "negbin", data = FMWTwSal3)
dszip4a = zeroinfl(catch~ Station + Operating+julian + 
                     Mean + Year, dist = "negbin", data = FMWTwSal3)
dszip4b = zeroinfl(catch~ Station + julian + Mean +
                     Year, dist = "negbin", data = FMWTwSal3)
dszip4c = zeroinfl(catch~ Station + Operating*julian + 
                     Mean, dist = "negbin", data = FMWTwSal3)
dszip4d = zeroinfl(catch~ Station +   Operating +
                     Year, dist = "negbin", data = FMWTwSal3)
dszip4e = zeroinfl(catch~ Station +  Mean +
                     Year, dist = "negbin", data = FMWTwSal3)
dszip4f = zeroinfl(catch~ Station +  julian, dist = "negbin", data = FMWTwSal3)

dszip4g = zeroinfl(catch~ Station +  Mean, dist = "negbin", data = FMWTwSal3)

dszip4h = zeroinfl(catch~ Year + Mean, dist = "negbin", data = FMWTwSal3)

dszip4i = zeroinfl(catch~ julian+Operating + Year, dist = "negbin", data = FMWTwSal3)


AIC( dszip4a, dszip4b,dszip4c,  dszip4e,dszip4f, dszip4g, dszip4h)

#best model
summary(dszip4a)
visreg(dszip4a)
#Those partial-residual plots look good!!!
#though the "year" effect is still having problems. 
#Also, I'm not sure what all those computational singularities were about.


##################################################################################################
#Other thoughts:
#Maybe look at "before" verusus "after" gate operations instead of "operating" versus "not operating"?

#Add wateryear type rather than just "year"?

#Do we have gate data from before 1999?

#now use years 1986-2012, with salinity data from FMWT
FMWT_DSmg2 = filter(FMWT_DSmg2, Year <2012)

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
dszip5f = zeroinfl(catch~ Station +  julian, dist = "negbin", data = FMWT_DSmg2)

dszip5g = zeroinfl(catch~ Station +  TopEC, dist = "negbin", data = FMWT_DSmg2)

dszip5h = zeroinfl(catch~ Year + TopEC, dist = "negbin", data = FMWT_DSmg2)

dszip5i = zeroinfl(catch~ julian+Operating2 + Year, dist = "negbin", data = FMWT_DSmg2)


AIC(dszip5, dszip5a,dszip5b, dszip5e, dszip5f, dszip5g,  dszip5h,  dszip5i)
summary(dszip5)

visreg(dszip5)
visreg(dszip5, xvar = "julian", by = "Operating2")

summary(dszip5a)
