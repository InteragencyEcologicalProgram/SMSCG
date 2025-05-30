#Try some different analyses


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
FMWT_DSmg$Vol = scale(FMWT_DSmg$TowVolume)

#now with fish, first just the time we have gate data, and just the fall because we have more trawls in the fall
FMWT_DSmg2 = filter(FMWT_DSmg, !is.na(Operating), julian >200, Year < 2012)

#Now the fall data for the whole thing
FMWT_DSmg2a = filter(FMWT_DSmg, julian >200, Year < 2012)
op.daily$Year = year(op.daily$Date)
op.daily$julian = yday(op.daily$Date)

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
FMWT_DSmg3a = merge(FMWT_DSmg2a, yrtyp, by = "Year")


#total catch in montezuma slough
tots = group_by(FMWT_DSmg2, Year) %>% summarize(catch = sum(catch))
index = read.csv("smeltindex.csv")

#wide to long
indexl = gather(index, key = "month", value = "index", -Year)
indexl$month = factor(indexl$month, levels = c("Sept", "Oct", "Nov", "Dec"), labels = c(9,10,11,12))

#add the month

FMWT_DSmg3$month = month(FMWT_DSmg3$Date)
FMWT_DSmg3a$month = month(FMWT_DSmg3a$Date)
#adding X2
X2 <- read_excel("supplemental_data_wr.1943-5452.0000617_hutton3.xlsx", sheet = "Daily")
X2$Date = as.Date(X2$Date)

#what about adding the FMWT index?
FMWT_DSmg4 = merge(FMWT_DSmg3, X2)
FMWT_DSmg4 = merge(FMWT_DSmg4, indexl)

FMWT_DSmg4x = merge(FMWT_DSmg3a, X2)
FMWT_DSmg4x$month = month(FMWT_DSmg4x$Date)
FMWT_DSmg4x = merge(FMWT_DSmg4x, indexl)


#Standardize continuous variables (salinity)
FMWT_DSmg4$ECscaled = scale(FMWT_DSmg4$TopEC)
FMWT_DSmg4$Indexscaled = scale(FMWT_DSmg4$index)
FMWT_DSmg4$julianscaled = scale(FMWT_DSmg4$julian)
FMWT_DSmg4$Year3 = scale(FMWT_DSmg4$Year)

#just the dry years
FMWT_DSmg4a = filter(FMWT_DSmg4, YT2 == "D")


#Standardize continuous variables (salinity)
FMWT_DSmg4x$ECscaled = scale(FMWT_DSmg4x$TopEC)
FMWT_DSmg4x$Indexscaled = scale(FMWT_DSmg4x$index)
FMWT_DSmg4x$julianscaled = scale(FMWT_DSmg4x$julian)


#just the dry years
FMWT_DSmg4ax = filter(FMWT_DSmg4x, YT2 == "D")

#quick plot to demonstrate something for the water year conditions report
historical.daily$Year = year(historical.daily$Datetime)
historical.daily$Month = month(historical.daily$Datetime)
historical.daily$Year[which(historical.daily$Month>9)] = historical.daily$Year[which(historical.daily$Month>9)] +1
hist.daily =merge(historical.daily, yrtyp, by = "Year")
hist.daily$julian = yday(hist.daily$Datetime)
hist.daily$wday = hist.daily$julian-274
hist.daily$wday[which(hist.daily$wday<0)] = hist.daily$julian[which(hist.daily$wday<0)]+ 92

ggplot(data = filter(hist.daily, Analyte == "Salinity"), aes(x = wday, y = Mean, color = YT2))+
  geom_smooth( se = FALSE) +
  scale_color_manual(values = c("lightblue", "lightgreen"), name = "Water Year Type", labels = c("Below Average", "Above average"))+
  geom_point(data = filter(hist.daily, Analyte == "Salinity" & Year == 2017), aes(x = wday, y = Mean), color = "black", alpha = 0.05) +
  geom_smooth(data = filter(hist.daily, Analyte == "Salinity" & Year == 2017), aes(x = wday, y = Mean), color = "black")+
  ylab("Mean salinity") + xlab("Day of water year")
