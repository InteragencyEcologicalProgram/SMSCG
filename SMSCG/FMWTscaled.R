#now let's really iron out those statistics

library(tidyverse)
library(readxl)
library(lubridate)
library(visreg)
library(pscl)
library(MASS)
library(MuMIn)


source("data manip FMWT.R")

#Global model including the delta smelt index, with just the dry years
#scale the electracal conductibity, since it's so big.
#also adding tow volume as an offset
FMWT_DSmg4a$Vol = scale(FMWT_DSmg4a$TowVolume)

dsznb1 = zeroinfl(catch~ Station + Operating2+julian + 
                     ECscaled + index + Year + offset(Vol), 
                  dist = "negbin",
                  data = FMWT_DSmg4a, na.action = "na.fail")

#Test all possible models
dreznb = dredge(dsznb1)

#this was the best
dsznb1best = zeroinfl(catch~ Operating2+julian + 
                        ECscaled + index + Year, 
                      dist = "negbin", data = FMWT_DSmg4a, na.action = "na.fail")
summary(dsznb1best)
visreg(dsznb1best)

#try it with the scaled julian day, year, and index too
FMWT_DSmg4a$Year3 = scale(FMWT_DSmg4a$Year)
dsznb2 = zeroinfl(catch~ Station+ Operating2+julianscaled + 
                    Indexscaled + SacX2 + ECscaled + offset(Vol), 
                  dist = "negbin", data = FMWT_DSmg4a, na.action = "na.fail")

dreznb2 = dredge(dsznb2)
summary(dsznb2)
dsznb2best = zeroinfl(catch~ Operating2+julianscaled + 
                        ECscaled + Indexscaled+ offset(Vol),
                      dist = "negbin", data = FMWT_DSmg4a, na.action = "na.fail")
summary(dsznb2best)
visreg(dsznb2best)

dsznb2a = zeroinfl(catch~ Operating2, 
                      dist = "negbin", data = FMWT_DSmg4a, na.action = "na.fail")
summary(dsznb2a)

aggregate(catch~Operating2, data = FMWT_DSmg4a, mean)
aggregate(catch~Operating2, data = filter(FMWT_DSmg4a, catch != 0), mean)
table(FMWT_DSmg4a$catch, FMWT_DSmg4a$Operating2)

#do some plots of pearson residuals versus values plots
#to try and assess model fit. 

tests = data.frame(DSresid = residuals(dsznb2best, type = "pearson"), 
                   DSfit = dsznb2best$fitted.values,
                   ops = dsznb2best$model["Operating2"],
                   EC = dsznb2best$model["ECscaled"],
                   Ind = dsznb2best$model["Indexscaled"],
                   day = dsznb2best$model["julianscaled"],
                   catch = FMWT_DSmg4a$catch)


ggplot(data = filter(tests, catch !=0), aes(x=DSresid, y = DSfit)) + geom_point()
ggplot(data = tests, aes(x=DSresid, y = ECscaled)) + geom_point()
ggplot(data = tests, aes(x=DSresid, y = Operating2)) + geom_point()
ggplot(data = tests, aes(x=DSresid, y = julianscaled)) + geom_point()
ggplot(data = tests, aes(x=DSresid, y = Indexscaled)) + geom_point()


###########################################################################################
#can we compare pre-gate data to post-gate data?

FMWT_DSmg4ax$prepost = NA
FMWT_DSmg4ax$prepost[which(FMWT_DSmg4ax$Year <1988)] = "pre"
FMWT_DSmg4ax$prepost[which(is.na(FMWT_DSmg4ax$prepost))] = "post"

FMWT_DSmg4ax = merge(FMWT_DSmg4ax, X2) %>%
  filter(!is.na(SacX2))

dsznb3 = zeroinfl(catch~Station + prepost + julianscaled + 
                    ECscaled +SacX2 + Indexscaled + offset(Vol), dist = "negbin", 
                  data = FMWT_DSmg4ax,na.action = "na.fail")
summary(dsznb3)
drepp = dredge(dsznb3)
head(drepp)
#It liked the full model, except for the volume offset. 

visreg(dsznb3,  gg = T)
visreg(dsznb3, gg = T, type = "contrast")

#Try it again without the volume offset
dsznb4 = zeroinfl(catch~Station + prepost + julianscaled + 
                    ECscaled +SacX2 + Indexscaled, dist = "negbin", 
                  data = FMWT_DSmg4ax,na.action = "na.fail")
summary(dsznb4)
visreg(dsznb4)

```
#I don't really know which I like better there. It lookslike the take-home message
