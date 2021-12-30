#Water quality time series plots for 2020.

#Rosemary Hartman 11/18/2020

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(wql)

library(cder)


instFlow <- readNWISdata(sites="05114000", service="iv", 
                         parameterCd="00060", 
                         startDate="2014-05-01T00:00Z",endDate="2014-05-01T12:00Z")


#load data 
WQdat = read.csv("Data/des_data.csv")
str(WQdat)

WQdat = mutate(WQdat, time = as.POSIXct(time, format = "%m/%d/%Y HH:MM"), Date = date(time))

#apparently Decker doesn't have chlorophyll from the summer
USGSDeckerCHL = readNWISdata(service = "iv", site = "11455478",  parameterCd = "32316", 
                          startDate="2020-06-01T00:00Z",endDate="2021-10-31T12:00Z")
USGSDeckerCHL2 = mutate(USGSDeckerCHL, analyte_name = "Fluorescence",  cdec_code = "Decker") %>%
  rename(time = dateTime, qaqc_flag_id = X_32316_00000_cd, value = X_32316_00000)

USGSDeckerSC = readNWISdata(service = "iv", site = "11455478",  parameterCd = "00095", 
                            startDate="2021-06-01T00:00Z",endDate="2021-10-31T12:00Z")
USGSDeckerSC2 = mutate(USGSDeckerSC, analyte_name = "Specific Conductance",  cdec_code = "Decker") %>%
  rename(time = dateTime,  qaqc_flag_id = X_BGC.PROJECT_00095_00000_cd, 
         value = X_BGC.PROJECT_00095_00000)

USGSDeckerTemp = readNWISdata(service = "iv", site = "11455478",  parameterCd = "00010", 
                            startDate="2021-06-01T00:00Z",endDate="2021-10-31T12:00Z")
USGSDeckerTemp2 = mutate(USGSDeckerTemp, analyte_name = "Temperature", cdec_code = "Decker") %>%
  rename(time = dateTime,qaqc_flag_id = X_BGC.PROJECT_00010_00000_cd, 
         value = X_BGC.PROJECT_00010_00000)


USGSDeckerTurb = readNWISdata(service = "iv", site = "11455478",  parameterCd = "63680", 
                              startDate="2020-06-01T00:00Z",endDate="2020-10-31T12:00Z")
USGSDeckerTurb2 = mutate(USGSDeckerTurb, analyte_name = "Turbidity",  cdec_code = "Decker") %>%
  rename(time = dateTime, qaqc_flag_id = X_BGC.PROJECT_63680_00000_cd, 
         value = X_BGC.PROJECT_63680_00000)


USGSdeker = rbind(USGSDeckerTemp2, USGSDeckerCHL2, USGSDeckerSC2, USGSDeckerTurb2)


WQdatx = select(USGSdeker, cdec_code, time, value, qaqc_flag_id, analyte_name) %>%
  mutate(Date = date(time)) %>%
               rbind(select(WQdat, -X, -unit_name))

sal = filter(WQdatx, analyte_name== "Specific Conductance") %>%
  mutate(salinity = ec2pss(value/1000, t = 25), analyte_name = "Salinity", value = NULL) %>%
  rename(value = salinity)

WQdatax2 = rbind(WQdatx, sal)

WQdat2 = group_by(WQdatax2, cdec_code, Date, analyte_name) %>%
  summarize(meanV = mean(value, na.rm =T))

WQplot = function(data, param) {
  dat = filter(data, analyte_name == param)
  ggplot(dat, aes(x = Date, y = meanV)) + 
           geom_point() + 
           geom_smooth()+
           ylab(param)+
    facet_wrap(~cdec_code)
}

WQplot(WQdat2, "Fluorescence") + ylab("Chlorophyll Fluorescence (RFU)")
WQplot(WQdat2, "Salinity")  + 
  geom_hline( yintercept = 6, color = "red", linetype = 2)+
 ylab("Salinity (PSU)")
WQplot(WQdat2, "Specific Conductance") 
WQplot(WQdat2, "Turbidity") +
  geom_hline( yintercept = 12, color = "red", linetype = 2)+
  ylab("Turbidity (NTU)")
WQplot(WQdat2, "Temperature") +
  geom_hline( yintercept = 23.9, color = "red", linetype = 2)+
  ylab("Temperature (C)")

WQplot2 = function(data, param) {
  dat = filter(data, analyte_name == param)
  ggplot(dat, aes(x = time, y = value)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth()+
    ylab(param)+
    facet_wrap(~cdec_code)
}


WQplot2(WQdatax2, "Fluorescence") + ylab("Chlorophyll Fluorescence")
WQplot2(WQdatax2, "Salinity") + geom_hline( yintercept = 6, color = "red", linetype = 2)+
  ylab("Salinity (PSU)")
WQplot2(WQdatax2, "Temperature") + geom_hline( yintercept = 23.6, color = "red", linetype = 2)+
  coord_cartesian(ylim = c(15,28))+
  ylab("Temperature (C)")
WQplot2(WQdatax2, "Specific Conductance") 
WQplot2(WQdatax2, "Turbidity") + coord_cartesian(ylim = c(0,150))+ 
  geom_hline( yintercept = 12, color = "red", linetype = 2)+
  ylab("Turbidity (NTU)")


####################################################################
#2021 data

SSI = cdec_query("SSI", 28, "E", start.date = as.Date("2021-01-01"), end.date = as.Date("2021-09-30"))


MAL = cdec_query("MAL", 28, "E", start.date = as.Date("2021-01-01"), end.date = as.Date("2021-09-30"))

all = cdec_query(c("MAL", "SSI", "GZB", "GZL", "GZM",  "NSL", "BDL", "HUN", "RVB"), 28, "E", start.date = as.Date("2021-01-01"), end.date = as.Date("2021-09-30"))

allmean = mutate(all, Date = date(ObsDate), analyte_name = "Fluorescence") %>%
  group_by(StationID, Date, SensorNumber, analyte_name) %>%
  summarize(Valuem = mean(Value, na.rm = T)) %>%
  filter(Date > as.Date("2021-06-01"), StationID %in% c("GZM", "SSI")) %>%
  rename(cdec_code = StationID)

ggplot(allmean, aes(x = Date, y = Chl)) + geom_point() + geom_smooth()+facet_wrap(~StationID)

summchl = group_by(allmean, StationID) %>%
  summarize(Mean = mean(Chl, na.rm = T))


#look at some different stations for fun
all = cdec_query(c( "SSI", "FRK", "GZL", "LIB",  "RVB"), 28, "E", start.date = as.Date("2021-01-01"), end.date = as.Date("2021-09-30"))

allmean = mutate(all, Date = date(ObsDate)) %>%
  group_by(StationID, Date, SensorNumber) %>%
  summarize(Chl = mean(Value, na.rm = T)) %>%
  filter(Date > as.Date("2021-06-01")) 


ggplot(allmean, aes(x = Date, y = Chl, color = StationID)) + 
  geom_point() + geom_smooth() +
  scale_color_discrete(labels = c("Franks Tract", "Grizzly Bay", "Rio Vista", "Sherman Isl"))+
  ylab("Chlorophyll ug/mL") + theme_bw()


#data from Jamel
Suisundata = read_csv("data/station_data 6.1.21 - 10.26.21.csv")
GZM = read_csv("GZM_data 6.1.21 - 10.26.21.csv")
Suisun = bind_rows(Suisundata, GZM)
WQall = mutate(Suisun, time = mdy_hm(time), Date = date(time)) %>%
  filter(qaqc_flag_id != "X", !(cdec_code == "GZB" & value == 0))

salBDL = filter(WQall, cdec_code == "BDL", analyte_name == "Specific Conductance") %>%
  mutate(Salinity = ec2pss(value/1000, t = 25))

ggplot(salBDL, aes(x = time, y = Salinity)) + geom_line() + 
  geom_hline(yintercept = 6, linetype = 2, color = "red")+ ylab("Salinity PSU") +
  theme_bw()
  


allmean2 =  group_by(WQall, cdec_code, Date, analyte_name) %>%
  summarize(Valuem = mean(value, na.rm = T), QAQC = sort(qaqc_flag_id)[1]) %>%
  filter(Date > as.Date("2021-06-01")) 

Chl = filter(allmean2, analyte_name == "Fluorescence")


ggplot(Chl, aes(x = Date, y = Valuem)) +geom_point()+#+ geom_point(aes(color = QAQC)) + 
  geom_smooth()+facet_wrap(~cdec_code) + ylab("Chlorophyll Fluorescence (RFU)")

summchl = group_by(allmean, StationID) %>%
  summarize(Mean = mean(Chl, na.rm = T))

test = filter(WQall, cdec_code == "GZB" & Value == 0)



Turb = filter(allmean2, analyte_name == "Turbidity")


ggplot(Turb, aes(x = Date, y = Valuem)) +geom_point()+#+ geom_point(aes(color = QAQC)) + 
  geom_smooth()+facet_wrap(~cdec_code) + ylab("Daily mean Turbidity (FNU)")



Sal = filter(allmean2, analyte_name == "Specific Conductance") %>%
  mutate(Salinity = ec2pss(Valuem/1000, t = 25))


ggplot(Sal, aes(x = Date, y = Salinity)) +geom_point()+#+ geom_point(aes(color = QAQC)) + 
  geom_smooth()+facet_wrap(~cdec_code) + ylab("Daily Salinity (PSU)")

#just pull temperature from CDEC because I"m tired

all = cdec_query(c("MAL", "SSI", "GZB", "GZL", "GZM",  "NSL", "BDL", "HUN", "RVB"), 25, "E", start.date = as.Date("2021-01-01"), end.date = as.Date("2021-09-30"))


allmean = mutate(all, Date = date(ObsDate), analyte_name = "Temperature") %>%
  group_by(StationID, Date, SensorNumber, analyte_name) %>%
  summarize(Valuem = (mean(Value, na.rm = T)-32)*5/9) %>%
  filter(Date > as.Date("2021-06-01"), StationID != "GZB", StationID!= "SSI") %>%
  rename(cdec_code = StationID)

ggplot(allmean, aes(x = Date, y = Valuem)) + geom_point() + 
  geom_smooth()+facet_wrap(~cdec_code) + ylab("Temperature (C)")+
  geom_hline(yintercept = 23.9, color = "red", linetype = 2)


