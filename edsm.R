#fix ted's edsm data
library(readxl)
library(tidyverse)
library(lubridate)

#this was the crappy version Ted did in Excel. 
edsmcatch = read_excel("Data/EDSMcatch2018.xlsx", sheet = "catch")

edsmcatch2 = pivot_longer(edsmcatch, cols = c("Suisun Bay":"Lower San Joaquin"), 
                          names_to = "region", values_to = "catch")

edsmeffort = read_excel("Data/EDSMcatch2018.xlsx", sheet = "effort")
edsmeffort2 = pivot_longer(edsmeffort, cols = c("Suisun Bay":"Lower San Joaquin"), 
                          names_to = "region", values_to = "Trawls")

edsmdata = merge(edsmcatch2, edsmeffort2)
edsmdata = rename(edsmdata, EndWeek = `End Of Week`)
edsmdata$catch[which(is.na(edsmdata$catch))] = 0

#Make a better version to publish on EDI
write.csv(edsmdata, "Data/EDSMsmelt_summer_2018.csv")


#######################################################
#quickly check raw EDSM data from 2018
#it looked like there were some problems with the number of trawls

EDSM = read.csv("Data/EDSM_KDTR.csv")

EDSM2 = mutate(EDSM, Date = ymd(Date), Year = year(Date), 
               Month = month(Date), Week = week(Date)) %>%
  filter(Year == 2018, Month %in% c(7,8,9,10), !is.na(Tow))


EDSM2x = mutate(EDSM, Date = mdy(Date), Year = year(Date), 
                Month = month(Date), Week = week(Date)) %>%
  filter(Year == 2018, Month %in% c(7,8,9,10))



EDSM3 = group_by(EDSM2, Stratum, Station, Date, Tow, Month, Year, Week) %>%
  summarize(totcatch = length(ForkLength), 
            DSM = length(ForkLength[which(OrganismCode == "DSM")]))

EDSM4 = group_by(EDSM3, Week, Stratum, Station) %>%
  summarize(tows = length(Tow), DSM = sum(DSM)) %>%
  group_by(Stratum, Week) %>%
  summarize(tows = sum(tows, na.rm = T), 
            Stations = length(Station), DSM = sum(DSM),
            DSMcpue = DSM/tows)

EDSMwide = pivot_wider(EDSM4, id_cols = c(Week), 
                       names_from = Stratum, values_from = DSMcpue,
                       values_fill = 0)

weeks = read.csv("weeks.csv")
names(weeks) = c("Week", "EndWeek")
EDSM4 = merge(EDSM4, weeks) %>%
  mutate(EndWeek = mdy(EndWeek))

library(RColorBrewer)

pE = ggplot(EDSM4, aes(x = EndWeek, y = DSMcpue, fill = Stratum)) +
  annotate("rect", xmin = ymd("2018-08-01"), xmax = ymd("2018-09-06"), 
           ymin = 0, ymax = 1, alpha = 0.5, fill = "grey" )+
  annotate("text", x= ymd("2018-08-15"), y = .9, label = "Action \nPeriod")+
  geom_bar(stat = "identity") + xlab("Week of Year") +
  scale_fill_brewer(palette = "Set1", name = "Region") +
  ylab("Delta Smelt Catch per Trawl") +
  theme_bw() + theme(text = element_text(size = 14))

ggsave("EDSM_suisun.png", plot = pE, width = 7, height = 5, dpi = 300)


#################################################################
#let's look at all years
EDSM2x = mutate(EDSM, Date = ymd(Date), Year = year(Date), 
                Month = month(Date), Week = week(Date)) %>%
  filter(Month %in% c(7,8,9,10), !is.na(Tow))



EDSM3 = group_by(EDSM2x, Stratum, Station, Date, Tow, Month, Year, Week) %>%
  summarize(totcatch = length(ForkLength), 
            DSM = length(ForkLength[which(OrganismCode == "DSM")]))

EDSM4 = group_by(EDSM3, Week, Stratum, Station, Year, Month) %>%
  summarize(tows = length(Tow), DSM = sum(DSM)) %>%
  group_by(Stratum, Week, Month, Year) %>%
  summarize(tows = sum(tows, na.rm = T), 
            Stations = length(Station), DSM = sum(DSM),
            DSMcpue = DSM/tows)


ggplot(EDSM4, aes(x=Month, y = DSM)) + geom_bar(stat = "identity") + facet_grid(Stratum~Year)

ggplot(EDSM4, aes(x=Week, y = DSM, fill = Stratum)) + geom_bar(stat = "identity") + facet_grid(.~Year)


ggplot(filter(EDSM3, Stratum=="Suisun Marsh"), aes(x=Date, y = DSM)) + 
  geom_bar(stat = "identity") 

#################################################################################
#import data from 2020
EDSMx = read.csv("Data/EDSM2012-2020.csv")

#Add teh more recent 2020 data
EDSM2020 = read.csv("Data/70EDSMDailyReport30Oct20.csv", na.strings = "n/p")


EDSMxa = mutate(EDSMx, Date = as.Date(Date, format = "%d-%b-%y"), Year = year(Date), 
                Month = month(Date), Week = week(Date)) %>%
  filter(Month %in% c(7,8,9,10), !is.na(Tow)) %>%
  select(Region, SubRegion, Station, Date, Stratum, 
         Tow, Month, Year, Week, ForkLength, OrganismCode)

#get the 2020 data in the same format as the other data
EDSM2020x = mutate(EDSM2020, Date = as.Date(Date, format = "%m/%d/%Y"), Year = year(Date), 
                   Month = month(Date), Week = week(Date))%>%
  mutate(Tow = 1:nrow(EDSM2020)) %>%
  ungroup() %>%
  rename(SubRegion = Sub.Region, Station = Station.Code, tows = Number.of.Tows) %>%
  filter(Month %in% c(7,8,9,10)) %>%
  select(Region, SubRegion, Station, Date, Stratum, Tow, Month, 
         Year, Week, Species, Catch, tows)

EDSM2020x$Catch[which(is.na(EDSM2020x$Catch))] = 0

EDSM2020xa = group_by(EDSM2020x, Stratum, Station, Date, tows, Month, Year, Week) %>%
  summarize(DSM = sum(Catch[which(Species == "DSM")], na.rm = T))

EDSM3a = group_by(EDSMxa, Stratum, Station, Date, Tow, Month, Year, Week) %>%
  summarize(DSM = length(ForkLength[which(OrganismCode == "DSM")])) %>%
  group_by(Stratum, Station, Date, Month, Year, Week) %>%
summarize(tows = length(Tow), DSM = sum(DSM))

EDSMALL = rbind(EDSM3a, EDSM2020xa)

EDSM4a = group_by(EDSMALL, Week, Stratum, Year, Month) %>%
  summarize(Stations = length(Station),Tows = sum(tows), DSM = sum(DSM, na.rm = T),
            DSMcpue = DSM/Tows) %>%
  ungroup()

test = group_by(EDSM4a, Week, Year, Month) %>%
  summarize(ntows = sum(Tows))

ggplot(EDSM4a, aes(x=Month, y = DSM)) + geom_bar(stat = "identity") + facet_grid(Stratum~Year)

ggplot(EDSM4a, aes(x=Week, y = DSM, fill = Stratum)) + geom_bar(stat = "identity") + facet_grid(.~Year)

library(RColorBrewer)

mypal = c(brewer.pal(8, "Set1"), brewer.pal(8, "Dark2"))

ggplot(EDSM4a, aes(x=Week, y = DSMcpue, fill = Stratum)) + 
  geom_bar(stat = "identity") + facet_grid(.~Year) +
  ylab("Delta Smelt Catch per Trawl")+
  scale_x_continuous(breaks = c(26, 30, 34, 38, 42), 
                     labels = c("Jun","Jul","Aug","Sep","Oct"))+
  scale_fill_manual(values = mypal)

#just the 2020 data
ggplot(filter(EDSM4a, Year == 2020), aes(x=Week, y = DSMcpue, fill = Stratum)) + 
  geom_bar(stat = "identity") +
  ylab("Delta Smelt Catch per Trawl")+
  scale_x_continuous(breaks = c(26, 30, 34, 38, 42), 
                     labels = c("Jun","Jul","Aug","Sep","Oct"))+
  scale_fill_manual(values = mypal)


ggplot(EDSM4a, aes(x=Week, y = DSM, fill = Stratum)) + 
  geom_bar(stat = "identity") + facet_grid(.~Year) + 
  ylab("total Delta Smelt caught") +
  scale_x_continuous(breaks = c(26, 30, 34, 38, 42), 
                     labels = c("Jun","Jul","Aug","Sep","Oct"))+
  scale_fill_manual(values = mypal)


towplot = ggplot(EDSM4a, aes(x=Week, y = Tows, fill = Stratum)) + 
  geom_bar(stat = "identity") + facet_grid(.~Year) #+ 
#  scale_x_continuous(breaks = c(26, 30, 34, 38, 42), labels = c("Jun","Jul","Aug","Sep","Oct"))

z2020 = filter(EDSM4a, Year == 2020)

library(ggthemes)
towplot + theme_excel_new()

