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

all = cdec_query(c("MAL",  "SSI", "GZB", "GZL", "GZM",  "NSL", "BDL", "HUN", "RVB"), 25, "E", start.date = as.Date("2021-01-01"), end.date = as.Date("2021-09-30"))


allmean = mutate(all, Date = date(ObsDate), analyte_name = "Temperature") %>%
  group_by(StationID, Date, SensorNumber, analyte_name) %>%
  summarize(Valuem = (mean(Value, na.rm = T)-32)*5/9) %>%
  filter(Date > as.Date("2021-06-01"), StationID != "GZB", StationID!= "SSI") %>%
  rename(cdec_code = StationID)

ggplot(allmean, aes(x = Date, y = Valuem)) + geom_point() + 
  geom_smooth()+facet_wrap(~cdec_code) + ylab("Temperature (C)")+
  geom_hline(yintercept = 23.9, color = "red", linetype = 2)

###############################################################
#plot the most recent months of data real quick

WQ = cdec_query(c("GZB", "GZM", "GZL", "BDL", "NSL", "RVB", "HUN", "MAL"), sensors = c(100, 25, 27, 28),
                start.date = as.Date("2022-06-01"), end.date = as.Date("2022-10-31"))
str(WQ)

ggplot(WQ, aes(x = DateTime, y = Value, color = StationID)) + facet_wrap(~SensorType, scales = "free_y")+
  geom_line()

WQ = mutate(WQ, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                               SensorNumber == 25 ~ (Value - 32)*5/9,
            TRUE~ Value),
            Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                             labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0)

ggplot(WQ, aes(x = DateTime, y = Value2, color = StationID)) + 
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line()   + theme_bw()       

#looks like there is some issue with GZM for a few days
test = filter(WQ ,StationID == "GZM", Analyte == "Temperature", Value2 < 10)
mindate = min(test$DateTime) - 1
maxdate = max(test$DateTime) +1

#Shaun says there was also some issues with the sonde before that, the spike in chlorophyll is probably bad
WQx = filter(WQ, !(StationID == "GZM"&DateTime > mindate & DateTime < maxdate), 
             !(Analyte == "Salinity" & Value2 < 1), !(Analyte == "Chlorophyll" & Value2 > 41))

#some of those other spikes might be bad too, but idduno

ggplot(WQx, aes(x = DateTime, y = Value2, color = StationID)) + 
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line()   + theme_bw()       


ggplot(filter(WQx, Analyte == "Turbidity"), aes(x = DateTime, y = Value2, color = StationID)) + 
  coord_cartesian(ylim = c(0, 250)) +
  geom_line()   + theme_bw()       +
  geom_hline(yintercept = 12, color = "red", linetype = "dashed", size = 1)+ 
  ylab("Turbidity NTU") + xlab("Date")

ggplot(filter(WQx, Analyte == "Chlorophyll"), aes(x = DateTime, y = Value2, color = StationID)) + 
  #coord_cartesian(ylim = c(0, 50)) +
  geom_line()   + theme_bw()       +
  geom_hline(yintercept = 10, color = "red", linetype = "dashed", size = 1)+ 
  ylab("Chlroophyll ug/L") + xlab("Date")

test = filter(WQx, Analyte == "Chlorophyll", Value2 >50)

test = filter(WQ, StationID == "GZM"&DateTime > ymd("2022-08-10") & DateTime < ymd("2022-08-20"))

ggplot(test, aes(x = DateTime, y = Value2, color = StationID)) + 
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line()   + theme_bw()       


#Do daily means instead
WQmean = WQx %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))


ggplot(WQmean, aes(x = Date, y = Value2, color = StationID)) + 
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line(size = 1)   + theme_bw() 

ggplot(filter(WQmean, Analyte == "Turbidity"), aes(x = Date, y = Value2, color = StationID)) + 
  coord_cartesian(ylim = c(0, 150)) +
  geom_line(size = 1)   + theme_bw()       +
  geom_hline(yintercept = 12, color = "red", linetype = "dashed", size = 1)+ 
  ylab("Turbidity NTU") + xlab("Date")

ggplot(filter(WQmean, Analyte == "Salinity"), aes(x = Date, y = Value2, color = StationID)) + 
  geom_line(size = 1)   + theme_bw()       +
  geom_hline(yintercept = 6, color = "red", linetype = "dashed", size = 1)+ 
  scale_color_manual(values = brewer.pal(5, "Set2"), 
                     name = "Station",
                     labels = c("Belden's Landing", "Grizzly Bay Buoy", "Grizly Bay Pile", "Mouth of Monetzuma", "National Steel"))+
  ylab("Salinity (ppt)") + xlab("Date")

ggplot(filter(WQmean, Analyte == "Chlorophyll"), aes(x = Date, y = Value2, color = StationID)) + 
  geom_line(size = 1)   + theme_bw()       +
  facet_wrap(~StationID)+
 # scale_color_manual(values = brewer.pal(5, "Set2"), 
  #                   name = "Station",
   #                  labels = c("Belden's Landing", "Grizzly Bay Buoy", "Grizly Bay Pile", "Mouth of Monetzuma", "National Steel"))+
  ylab("Chlorophyll (ug/L)") + xlab("Date - 2022")


ggplot(filter(WQmean, Analyte == "Chlorophyll"), aes(x = Date, y = Value2)) + 
  geom_point(aes(color = StationID), size = 1)   + theme_bw() + geom_smooth()+
  scale_color_manual(values = brewer.pal(5, "Set2"), 
                     name = "Station",
                     labels = c("Belden's Landing", "Grizzly Bay Buoy", "Grizly Bay Pile", "Mouth of Monetzuma", "National Steel"))+
  ylab("Chlorophyll (ug/L") + xlab("Date")

stage = cdec_query(c("GZB", "GZM", "GZL", "BDL", "NSL"), sensors = 1,
                start.date = as.Date("2022-06-01"), end.date = as.Date("2022-09-26"))

WQ2 = stage %>%
  rename(Stage = Value) %>%
  select(StationID, DateTime, ObsDate, Stage) %>%
  left_join(WQx)  %>%
  dplyr::filter(Analyte == "Chlorophyll")

ggplot(WQ2, aes(x = Stage, y = Value, color = StationID)) + geom_line()+ylab("Chlorophyll mg/L")
  
WQ3 = WQ2 %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T), Stage = max(Stage, na.rm = T))

ggplot(WQ3, aes(x = Stage, y = Value, color = StationID)) + geom_line()+ylab("Chlorophyll mg/L")

WQ4 = rename(WQ3, Chla = Value) %>%
  filter(StationID == "NSL") %>%
  pivot_longer(c(Stage, Chla), names_to = "Parameter", values_to = "Value")

ggplot(WQ4, aes(x = Date, y = Value, color = Parameter)) + geom_line()

#########################################################################
#Does salinity really stay high in the winter?


WQwinter = cdec_query(c("VOL", "HUN", "GZL", "BDL", "NSL"), sensors = c(100, 25),
                start.date = as.Date("2010-06-01"), end.date = as.Date("2022-09-26"))
str(WQ)

ggplot(WQ, aes(x = DateTime, y = Value, color = StationID)) + facet_wrap(~SensorType, scales = "free_y")+
  geom_line()

WQwinter = mutate(WQwinter, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                                   SensorNumber == 25 ~ (Value - 32)*5/9,
                                   TRUE~ Value),
            Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                             labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0, !(Analyte== "Temperature" & Value2 >100))


ggplot(WQwinter, aes(x = DateTime, y = Value2, color = StationID)) + 
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line()   + theme_bw()       



#Do daily means instead
WQmeanW = WQwinter %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T)) %>%
  mutate(DOY = yday(Date), Year = year(Date))

ggplot(filter(WQmeanW, Analyte == "Salinity", StationID != "GZM"), aes(x = Date, y = Value2, color = StationID)) + 
  geom_line()   + theme_bw()+
  geom_hline(yintercept = 6, size = 2, linetype =2)+ ylab("Salinity, PSU")

ggplot(filter(WQmeanW, Analyte == "Salinity", StationID == "BDL", Year == 2022), aes(x = DOY, y = Value2, color = as.factor(Year))) + 
  geom_line()   + theme_bw()+
  geom_hline(yintercept = 6, size = 2, linetype =2)+ ylab("Salinity, PSU")

#################################################################
#2022 plot

Tulered = read_csv("Data/2022-08-09_TuleRedBreach_2022-09-21 (formatted).csv") %>%
  mutate(time = mdy_hm(time), qaqc_flag_id = "G") %>%
  filter(!(analyte_name == "Specific Conductance" & value <15000))
WQ2022 = read_csv("Data/SMSG Data 2022.csv") 
  



WQ = cdec_query(c( "GZL",  "RVB","MAL"), sensors = c(100, 25, 27, 28),
                start.date = as.Date("2022-06-01"), end.date = as.Date("2022-10-31"))
WQp = mutate(WQ, time = DateTime) %>%
  rename(analyte_name = SensorType, cdec_code = StationID, value = Value, qaqc_flag_id = DataFlag) %>%
  mutate(analyte_name = case_when(analyte_name == "EL COND" ~ "Specific Conductance",
                                  analyte_name == "CHLORPH" ~ "Chlorophyll",
                                  analyte_name == "TURB W" ~ "Turbidity",
                                  analyte_name == "TEMP W" ~ "Water Temperature",
                                  TRUE ~ analyte_name)) %>%
  select(time, cdec_code, analyte_name, value, qaqc_flag_id) %>%
  mutate(value = case_when(analyte_name == "Water Temperature" ~ (value-31)*5/9,
                           TRUE ~ value))


WQall = bind_rows(Tulered, WQ2022, WQp) %>%
  filter(qaqc_flag_id != "X")


WQmean = WQall %>%
  mutate(Date = date(time)) %>%
  group_by(Date, cdec_code, analyte_name) %>%
  summarize(Value = mean(value, na.rm = T)) 


ggplot(WQmean, aes(x = Date, y = Value, color = cdec_code)) + 
  facet_wrap(~analyte_name, scales = "free_y")+
  geom_line(size = 1)   + theme_bw() 

test = filter(WQmean, cdec_code == "GZL", analyte_name == "Chlorophyll")

ggplot(filter(WQmean, analyte_name == "Chlorophyll", !cdec_code %in% c("CSE", "TRB", "MSL")), aes(x = Date, y = Value)) + 
  geom_point(aes(color = cdec_code), size = 1)   + theme_bw() + 
  geom_line(aes(color = cdec_code))+
  scale_color_discrete(guide = NULL)+
  facet_wrap(~cdec_code)+
   ylab("Chlorophyll (ug/L)") + xlab("Date")+
  coord_cartesian(xlim = c(ymd("2022-06-01", "2022-10-31")))

ggplot(filter(WQmean, analyte_name == "Turbidity", !cdec_code %in% c("CSE", "MSL")), aes(x = Date, y = Value)) + 
  geom_point(aes(color = cdec_code), size = 1)   + theme_bw() + 
  geom_line(aes(color = cdec_code))+
  scale_color_discrete(guide = NULL)+
  geom_hline(yintercept = 12, color = "red", linetype = 2)+
  facet_wrap(~cdec_code, scales = "free_y")+
  ylab("Turbidity (NTU)") + xlab("Date")+
  coord_cartesian(xlim = c(ymd("2022-06-01", "2022-10-31")), ylim = c(0,150))

test = filter(WQmean, analyte_name == "Turbidity")
test2 = filter(WQmean, analyte_name == "Turbidity" & Value >150)

ggplot(filter(WQmean, analyte_name == "Specific Conductance", !cdec_code %in% c("CSE", "MSL", "MAL", "NSL", "RVB")), aes(x = Date, y = Value)) + 
  geom_point(aes(color = cdec_code), size = 1)   + theme_bw() + 
  geom_line(aes(color = cdec_code))+
  #scale_color_discrete(guide = NULL)+
  #geom_hline(yintercept = 23.9, linetype = 2, color = "red")+
 # facet_wrap(~cdec_code)+
  ylab("Specific Conductance") + xlab("Date")+
  coord_cartesian(xlim = c(ymd("2022-06-01", "2022-10-31")))

salinity = filter(WQmean, analyte_name == "Specific Conductance") %>%
  mutate(Salinity = ec2pss(Value/1000, 22))

ggplot(filter(salinity, !cdec_code %in% c("CSE", "MSL")),
       aes(x = Date, y = Salinity)) + 
  geom_point(aes(color = cdec_code), size = 1)   + theme_bw() + 
  geom_line(aes(color = cdec_code))+
  scale_color_discrete(guide = NULL)+
  geom_hline(yintercept = 6, linetype = 2, color = "red")+
   facet_wrap(~cdec_code)+
  ylab("Saliinty") + xlab("Date")+
  coord_cartesian(xlim = c(ymd("2022-06-01", "2022-10-31")))


#just plot beldon's for a presentation
ggplot(filter(salinity, cdec_code %in% c("BDL")),
       aes(x = Date, y = Salinity)) + 
  geom_point( size = 1)   + theme_bw() + 
  geom_line()+
  scale_color_discrete(guide = NULL)+
  geom_hline(yintercept = 6, linetype = 2, color = "red")+
  ylab("Daily Mean Salinity") + xlab("Date")+
  coord_cartesian(xlim = c(ymd("2022-06-01", "2022-10-31")))

#temperature
Temp = cdec_query(c( "GZL",  "RVB","MAL", "GZM", "GZB", "BDL", "HUN", "NSL"), sensors = c(25),
                start.date = as.Date("2022-06-01"), end.date = as.Date("2022-10-31"))
Tempp = mutate(Temp, time = DateTime) %>%
  rename(analyte_name = SensorType, cdec_code = StationID, value = Value, qaqc_flag_id = DataFlag) %>%
  mutate(analyte_name = case_when(analyte_name == "EL COND" ~ "Specific Conductance",
                                  analyte_name == "CHLORPH" ~ "Chlorophyll",
                                  analyte_name == "TURB W" ~ "Turbidity",
                                  analyte_name == "TEMP W" ~ "Water Temperature",
                                  TRUE ~ analyte_name)) %>%
  select(time, cdec_code, analyte_name, value, qaqc_flag_id) %>%
  mutate(value = case_when(analyte_name == "Water Temperature" ~ (value-31)*5/9,
                           TRUE ~ value))


Tempall = bind_rows(Tulered, Tempp) %>%
  filter(qaqc_flag_id != "X")

Tempmean = Tempall %>%
  mutate(Date = date(time)) %>%
  group_by(Date, cdec_code, analyte_name) %>%
  summarize(Value = mean(value, na.rm = T)) %>%
  filter(Value > 15)


ggplot(filter(Tempmean, analyte_name == "Water Temperature", !cdec_code %in% c("CSE", "MSL")), aes(x = Date, y = Value)) + 
  geom_point(aes(color = cdec_code), size = 1)   + theme_bw() + 
  geom_line(aes(color = cdec_code))+
  geom_hline(yintercept = 23.9, linetype = 2, color = "red")+
  scale_color_discrete(guide = NULL)+
  facet_wrap(~cdec_code)+
  ylab("Tempearture (C)") + xlab("Date")+
  coord_cartesian(xlim = c(ymd("2022-06-01", "2022-10-31")))

###############################################################
#calculate number of days for each station over 23.9

Temp2 = Tempall %>%
  mutate(Date = date(time), Month = month(time)) %>%
  group_by(Date, Month, cdec_code) %>%
  summarize(Max = max(value, na.rm = T), Meantemp = mean(value, na.rm = T))

daysabove = group_by(Temp2, cdec_code) %>%
  summarize(n = n(), stress = length(Max[which(Max >23.9)]), stressmean = length(Meantemp[which(Meantemp >23.9)]))
