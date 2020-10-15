#This file compares water quality data from Grizzly Bay
#it hasn't been used in any analysis yet.
#Last updated by Rosemary Hartman, 10/14/2020

library(cder)
library(tidyverse)
library(wql)

#data from Montezuma Slough that MIchael got for me
load("waterquality.RData")

#query cdec for the grizzly bay and grizzly bouy station
#"GZB" is the code for the grizzly buoy station
GZB = cdec_query("GZB", 100, "E", as.Date("2018-07-01"),  as.Date("2018-10-31"))

#Grizzly pile station
GZL = cdec_query("GZL", 100, "E", as.Date("2018-07-01"),  as.Date("2018-10-31"))

#combine the data sets
Grizz = rbind(GZB, GZL)

#quick exploritory plot
p = ggplot(Grizz, aes(x = DateTime, y = Value, color = StationID))
p+ geom_line() + ylab("Conductivity")


#Take the daily averages to get rid of some of the noise,
#and convert conductivity to salinity
GrizzDaily = mutate(Grizz, Date = as.Date(DateTime), Salinity = ec2pss(Value/1000, 25)) %>%
  group_by(StationID, Date) %>%
  summarize(Mean = mean(Salinity, na.rm = T)) %>%
  rename(Station = StationID) %>%
  select(Station, Date, Mean)

#Reorganize data from Montezuma
Saldaily = rename(action.daily, Date = Datetime) %>%
  filter(Analyte == "Salinity") %>%
  select(Station, Date, Mean)

SuisunDay = rbind(GrizzDaily, Saldaily)

p2 = ggplot(SuisunDay, aes(x = Date, y = Mean, color = Station))
p2+ geom_line()

p2 = ggplot(GrizzDaily, aes(x = Date, y = Mean, color = Station))
p2+ geom_line() + ylab("mean daily salinity")


#Compare temperature at Rio Vista to Belden's landing

BDLt = cdec_query("BDL", 25, "E", as.Date("2000-07-01"),  as.Date("2018-10-31"))
SRVt = cdec_query("SRV", 25, "E", as.Date("2000-07-01"),  as.Date("2018-10-31"))

Temps = rbind(BDLt, SRVt)
Temps = mutate(Temps, julian = yday(DateTime), Month = month(DateTime)) %>%
  filter(Value <80, Value >0)

ggplot(Temps, aes(x=julian, y = Value, color = StationID)) + geom_point(alpha = 0.3) + geom_smooth()


ggplot(Temps, aes(x=julian, y = Value, color = StationID)) + geom_smooth()

############################################################
#now let's compare the grizzly pile station to Tule REd for 2020


#query cdec for the grizzly bay station

GZL2020 = cdec_query("GZL", 100, "E", as.Date("2020-08-01"),  as.Date("2020-09-01"))
GZL2020sal = mutate(GZL2020,  Salinity = ec2pss(Value/1000, 25))%>%
  rename(SpecCond = Value)%>%
  select(-ObsDate, -Duration, -DataFlag, -SensorUnits, -SensorNumber, -SensorType)

TulePond <- read.csv("Data/HydroVu_Tule_Red_-_Pond_C_2020-08-04_12-23-46_Export.csv")
TulePond$StationID = "TulePond"

TuleBreach <- read_csv("Data/HydroVu_Tule_Red_-_Breach_2020-08-04_12-23-46_Export.csv")
TuleBreach$StationID = "TuleBreach"

Tule = rbind(TulePond, TuleBreach)
Tule$DateTime = as.POSIXct(Tule$DateTime, format = "%m/%d/%Y %H:%M", tz = "UTC")

TuleGriz = rbind(Tule, GZL2020sal)


p4 = ggplot(TuleGriz, aes(x = DateTime, y = Salinity, color = StationID))
p4+ geom_line() + ylab("Salinity") + 
  coord_cartesian(xlim = c(as.POSIXct("2020-08-03 00:00:00"), 
                           as.POSIXct("2020-08-30 00:00:00")),
                  ylim = c(8,16))

                                                     