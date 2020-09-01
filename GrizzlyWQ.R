#take a closer look at water quality in grizzly bay

library(cder)
library(tidyverse)
library(wql)

#query cdec for the grizzly bay and grizzly bouy station

GZB = cdec_query("GZB", 100, "E", as.Date("2018-07-01"),  as.Date("2018-10-31"))
GZL = cdec_query("GZL", 100, "E", as.Date("2018-07-01"),  as.Date("2018-10-31"))

Grizz = rbind(GZB, GZL)

p = ggplot(Grizz, aes(x = DateTime, y = Value, color = StationID))
p+ geom_line() + ylab("Conductivity")

GrizzDaily = mutate(Grizz, Date = as.Date(DateTime), Salinity = ec2pss(Value/1000, 25)) %>%
  group_by(StationID, Date) %>%
  summarize(Mean = mean(Salinity, na.rm = T)) %>%
  rename(Station = StationID) %>%
  select(Station, Date, Mean)

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

