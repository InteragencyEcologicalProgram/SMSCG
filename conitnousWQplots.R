#Water quality time series plots for 2024.

#Rosemary Hartman last updated 6/26/2024

library(tidyverse)
library(lubridate)
library(dataRetrieval)

library(wql)
library(RColorBrewer)
library(readxl)
library(cder)

#x2 plot
X2 = cdec_query("CX2", sensors = 145,
                start.date = as.Date("2016-06-01"), end.date =  as.Date("2024-11-01"))

X2 = mutate(X2, X2km = case_when(DataFlag == "v" & DateTime > ymd_hm("2023-07-01 11:11")~ 81,
            TRUE~ Value))

ggplot(X2, aes(x = DateTime, y = X2km)) +
  geom_line()+theme_bw()+
  geom_point(aes(color = DataFlag))+
  scale_color_manual(values = c("black", "red"), labels = c("Value", ">81km"))


X2 = mutate(X2, Year = year(DateTime), Month = month(DateTime), DOY = yday(DateTime)) 

ggplot(X2, aes(x = DOY, y = Value, color = as.factor(Year))) +
  geom_point()+ geom_line()+
  geom_hline(yintercept = 74)+
  coord_cartesian(xlim = c(150, 320))+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
  

ggplot(filter(X2, Year ==2024), aes(x = DOY, y = Value, color = as.factor(Year))) +
  geom_point()+ geom_line()+
  geom_hline(yintercept = 74)+
  coord_cartesian(xlim = c(150, 320))+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))



#############################################################################################
#plot the most recent months of data real quick

WQ = cdec_query(c("GZB", "GZM", "GZL", "BDL", "NSL", "RVB",  "HUN", "CSE"), 
                sensors = c(100, 25, 27, 28),
                start.date = as.Date("2025-03-01"), end.date =  today())
str(WQ)

ggplot(WQ, aes(x = DateTime, y = Value, color = StationID)) + facet_wrap(~SensorType, scales = "free_y")+
  geom_line()+#geom_vline(xintercept = ymd_hm("2024-07-01 00:00"))+
  scale_color_brewer(palette = "Dark2")+theme_bw()

WQx = mutate(WQ, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                               SensorNumber == 25 ~ (Value - 32)*5/9,
                               #SensorNumber == 25 & Value >30 ~ NA,
                               SensorNumber == 25 & Value <40 ~ NA,
            TRUE~ Value),
            Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                             labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0, !(SensorNumber ==25 & Value2>26),
         !(SensorNumber ==25 & Value<60),
         !(SensorNumber ==25 & Value>90), 
         !(SensorNumber ==27 & Value2>200), 
         !(SensorNumber ==28 & Value2>20))


cuttoffs = data.frame(Analyte = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"),
                      cutoff = c(6, 10, 22, 12))

#15-minute data
ggplot(WQx, aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
   theme_bw()   #+
  #coord_cartesian(xlim = c(ymd_hms("2024-06-01 00:00:00"), now()))

#just BDL and RVB for smelt cages
ggplot(filter(WQx, StationID %in% c("RVB", "BDL")), aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
  theme_bw()   


ggplot(filter(WQx, StationID%in% c("RVB", "BDL"), Analyte == "Temperature"), aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(yintercept = 25, color = "red", linetype =2)+
  geom_hline(yintercept = 22, color = "black", linetype =2)+
  annotate("text", x = ymd_hms("2024-06-15 00:00:00"), y = 25.15, label = "Smelt start dying")+
  
  annotate("text", x = ymd_hms("2024-06-15 00:00:00"), y = 22.15, label = "Smelt stop growing")+
  theme_bw()   + ylab("Tempearature C")+
  coord_cartesian(xlim = c(ymd_hms("2024-06-01 00:00:00"), ymd_hms("2024-10-31 00:00:00")))


#Do daily means instead
WQmean = WQx %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))


#######################################################################
#just suisun bay

ggplot(filter(WQx, StationID %in% c("C16", "RYC", "BDL", "GZB", "GZL")), aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
 # geom_hline(yintercept = 25, color = "red", linetype =2)+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  
  facet_wrap(~Analyte, scales = "free_y")+
  theme_bw()   + ylab("Tempearature C")#+
 # coord_cartesian(xlim = c(ymd_hms("2024-06-01 00:00:00"), ymd_hms("2024-07-15 00:00:00")))

ggplot(filter(WQmean, StationID %in% c("CSE","BDL",  "GZL")), 
       aes(x = Date, y = Value2, color = StationID)) + 
  geom_line()+
  # geom_hline(yintercept = 25, color = "red", linetype =2)+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  
  facet_wrap(~Analyte, scales = "free_y")+
  theme_bw()   + ylab("Tempearature C")#+
 # coord_cartesian(xlim = c(ymd("2024-06-01"), ymd("2024-07-15")))




###############################################################################################
#BDL salinity to match the modeling graph

ggplot(filter(WQmean, StationID == "BDL", Analyte== "Salinity"), aes(x = Date, y = Value2))+
  geom_line()+
  coord_cartesian(ylim = c(0,5.2))+
  ylab("psu")+
  theme_bw()

#grab the modeled salinity
library(janitor)
BDL2017 = read_csv("data/bdl_salinity_2017.csv") %>%
  clean_names()

BDLmean = BDL2017 %>%
  mutate(Date = date(date), DOY = yday(Date)) %>%
  group_by(Date, DOY) %>%
  summarize(Value2 = mean(x100taf_15aug, na.rm = T)) %>%
  mutate(StationID = "Modeled BDL Salinity")

WQwmodel = bind_rows(BDLmean, filter(WQmean, StationID == "BDL", Analyte== "Salinity")) %>%
  mutate(DOY = yday(Date))

ggplot(WQwmodel, aes(x = DOY, y = Value2, color = StationID, linetype = StationID))+
  geom_line(size = 1)+
  coord_cartesian(ylim = c(0,5.2))+
  ylab("Salinity at Belden's Landing (PSU)")+
  coord_cartesian(xlim = c(152,310))+
  theme_bw()+ xlab("Date")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  scale_color_manual(values = c("black", "blue"), labels = c("Observed Value", "Modeled Value") )+
  scale_linetype_manual(values = c(1, 2), labels = c("Observed Value", "Modeled Value") )+
  theme(legend.position = "bottom")


ggsave("plots/BDL2023v2017model.tiff", device = "tiff", width =6.5, height =4.5)


###################################################################################
#britt randomly asked about the ship channel

library(dataRetrieval)


shipchannel = readNWISdata(site = c("11455095"),  service = "iv",
                           parameterCd = "00010",
                           startDate = "2024-05-01T00:00", endDate = "2024-07-15T12:00")

ggplot(shipchannel, aes(x = dateTime, y = X_00010_00000)) + geom_line()
########################################################################################


gatedates = data.frame(StartDate = c(ymd("2024-07-01"), ymd("2024-09-06"), ymd("2024-09-01"), ymd("2024-10-28")),
                       EndDate = c(ymd("2024-08-29"), ymd("2024-09-30"), ymd("2024-09-30"), ymd("2024-11-04")),
                       Type = c("SMSCG", "SMSCG", "X2@80km", "SMSCG"),
                       xval = c(ymd("2024-07-15"), ymd("2024-09-06"), ymd("2024-09-01"), ymd("2024-10-20")),
                       ynudge = c(0,-1, 1, 1))

yvals = data.frame(Analyte = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                   yval = c(11,8,25,95), yoff = c(0.08, 0.08, 0.015, 0.08))

gatedates2 = cross_join(gatedates, yvals)

#plot for monthly update
ggplot(WQmean, aes(x = Date, y = Value2, color = StationID)) + 
   geom_rect(data = gatedates, aes(ymin = -Inf, ymax = Inf,xmin = StartDate, xmax = EndDate,
                                   fill = Type), inherit.aes = FALSE, alpha = 0.4)+
scale_fill_manual(values = c("grey60", "tan1"), name = "Action\nPeriod")+
 geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line( linewidth =1)   + theme_bw() +ylab(NULL) #+
  #geom_text(data = gatedates2, aes(y = yval+yoff*yval*ynudge,x = xval, label = Type), 
  #          inherit.aes = FALSE, hjust =0)

ggsave("plots/ContWQ_2024.tiff", width = 8, height =6, device = "tiff")

#version with specific conductance for landowners
#6ppt is similar about 10600 uS/mm
cuttoffs2 =  data.frame(Analyte = c("Salinity", "Temperature", "Turbidity"),
                        cutoff = c(10600, 71.6, 12))

ggplot(filter(WQmean,  !StationID %in% c("GZB", "GZM")),
       aes(x = Date, y = Value, color = StationID)) + 
  scale_color_brewer(palette = c("Dark2"), labels = c("Beldens Landing", "Collinsville", "Grizzly Bay", "Hunter Cut", 
                                                      "National Steel", "Rio Vista"))+
  geom_hline(data = cuttoffs2, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  geom_vline(xintercept = ymd("2023-08-15"), linewidth =1)+
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line( linewidth =1)   + theme_bw() +ylab(NULL)+
  theme(legend.position = "bottom")


#just rio vista and bdl for smelt cages
ggplot(filter(WQmean, StationID %in% c("RVB", "BDL")), aes(x = Date, y = Value2, color = StationID)) + 
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line( linewidth =1)   + theme_bw()

#just salinity

ggplot(droplevels(filter(WQmean,Analyte == "Salinity")), aes(x = Date, y = Value2, color = StationID)) + 
  geom_hline(aes(yintercept = 6), color = "red", 
             linetype =2, linewidth =1)+
  geom_line( linewidth =1)   + theme_bw() 



ggplot(droplevels(filter(WQx, Analyte == "Salinity")), aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_hline(aes(yintercept = 6), color = "red", 
             linetype =2, linewidth =1)+
  coord_cartesian(xlim = c(ymd_hm("2023-09-01 00:00"), now()))+
  geom_line( linewidth =1)   + theme_bw() 

###########################################################################################
#plot of 2024 data for SF report
#add a few more stations 

WQ2 = cdec_query(c("GOD", "GZB", "HON", "MAL", "MSL", "RYC", "SSI"), sensors = c(100, 25, 27, 28),
                start.date = as.Date("2024-06-01"), end.date = "2024-11-01")
WQ2b = mutate(WQ2, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                                      SensorNumber == 25 ~ (Value - 32)*5/9,
                                      SensorNumber == 25 & Value >30 ~ NA,
                                      TRUE~ Value),
               Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                                labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0, !(SensorNumber ==25 & Value2>26), !(SensorNumber ==27 & Value2>200), 
         !(SensorNumber ==28 & Value2>20))

WQmean2 = WQ2b %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))

WQmeanall = bind_rows(WQmean, WQmean2)

stations = read_csv("Data/station_data.csv")

WQmeanallx = left_join(WQmeanall, stations, by = c("StationID"="station"))
  
WQmeanally = mutate(WQmeanallx, region = factor(region, levels = c("Bay", "Marsh", "River"), 
                         labels = c("Suisun Bay", "Suisun Marsh", "Sacramento River")),
                   Analyte2 = factor(Analyte, levels = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                                    labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU")))
cuttoffs$Analyte2 = factor(cuttoffs$Analyte, levels = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                          labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU"))

# gatedates = data.frame(StartDate = c(ymd("2024-07-01"), ymd("2024-09-06"), ymd("2024-09-01")),
#                        EndDate = c(ymd("2024-08-29"), ymd("2024-09-30"), ymd("2024-09-30")),
#                        Type = c("SMSCG", "SMSCG", "X2@80km"))

ggplot(WQmeanally, aes(x = Date, y = Value2))+
  geom_rect(data = gatedates, aes(xmin = StartDate, xmax = EndDate, 
                                  ymin = -Inf, ymax = Inf, fill = Type), 
            alpha = 0.4, inherit.aes = FALSE)+
  geom_line(aes(color = StationID))+
  facet_grid(Analyte2~region, scales = "free_y")+
  geom_hline(data = filter(cuttoffs, Analyte2 != "Chlorophyll ug/L"), aes(yintercept = cutoff), color = "black",
             linetype =2, linewidth =1)+
  geom_hline(data = filter(cuttoffs, Analyte2 == "Chlorophyll ug/L"), aes(yintercept = cutoff), color = "grey",
             linetype =3, linewidth =1)+
  coord_cartesian(xlim = c(ymd("2024-06-01"), ymd("2024-10-31")))+
  scale_fill_manual(values = c("lightblue", "grey"))+
  theme_bw()+
  ylab(NULL)

                  ggsave("plots/AVGwq2024.png", device = "png", width =8, height =8)
                  
save(WQmeanally, gatedates, file = "data/WQfor2024report_notQCd.Rdata")

#########################################################################################
#look up water stage at each point
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

###########################################################################

###############################################################
#calculate number of days for each station over 23.9

Temp2 = Tempall %>%
  mutate(Date = date(time), Month = month(time)) %>%
  group_by(Date, Month, cdec_code) %>%
  summarize(Max = max(value, na.rm = T), Meantemp = mean(value, na.rm = T))
daysabove = group_by(Temp2, cdec_code) %>%
  summarize(n = n(), stress = length(Max[which(Max >23.9)]), stressmean = length(Meantemp[which(Meantemp >23.9)]))
#############################################
#Grizzly bay comparison

Grizz = filter(WQmean, cdec_code %in% c("GZB", "HUN", "GZL", "GZM", "TRB"), 
               analyte_name %in% c("Chlorophyll", "Water Temperature", "Specific Conductance", "Turbidity"))

ggplot(Grizz, aes(x = Date, y = Value, color = cdec_code)) +
  geom_point()+geom_line()+
  facet_wrap(~analyte_name, scales = "free_y")

#########################################################
#how hot does it get various places

Temps = cdec_query(c( "BDL", "RVB", "LIS", "GZL"), sensors = c(25),
                start.date = as.Date("2017-06-01"), end.date = today())
TempsA = mutate(Temps, Value = (Value - 32)*5/9) %>%
  filter(Value <40, Value >2) %>%
  mutate(Yday = yday(ObsDate))

ggplot(TempsA, aes(x = ObsDate, y = Value, color = StationID))+
  geom_line()+
  geom_hline(yintercept = 25)+
  facet_wrap(~StationID)

ggplot(filter(TempsA, !StationID %in%  c("LIS", "GZL")), aes(x = ObsDate, y = Value, color = StationID))+
  geom_line()+
  geom_hline(yintercept = 22, linetype =2, color = "red")+
  
  geom_hline(yintercept = 25, linetype =1, color = "red")+
  coord_cartesian(xlim = c(ymd_hms("2023-08-01 00:00:00"), ymd_hms("2023-11-10 00:00:00")),
                  ylim = c(15, 27))+
  theme_bw()+
  geom_vline(xintercept = ymd_hm("2023-08-15 00:00"))

ggplot(filter(TempsA, !StationID %in%  c("LIS", "GZL")), aes(x = ObsDate, y = Value, color = StationID))+
  geom_line()+
  geom_hline(yintercept = 22, linetype =2, color = "red")+
  
  geom_hline(yintercept = 25, linetype =1, color = "red")+
  coord_cartesian(xlim = c(ymd_hms("2022-08-01 00:00:00"), ymd_hms("2022-11-10 00:00:00")),
                  ylim = c(15, 27))+
  theme_bw()+
  geom_vline(xintercept = ymd_hm("2023-08-15 00:00"))

dailytemps = mutate(TempsA, Date = date(ObsDate)) %>%
  group_by(Date, Yday, StationID) %>%
  summarize(Value = mean(Value, na.rm =T))


ggplot(filter(dailytemps, StationID != "LIS"), aes(x = Yday, y = Value, color = StationID))+
  geom_line()+
  geom_hline(yintercept = 23.9)+
  geom_hline(yintercept = 22, linetype =2, color = "red")+
  facet_wrap(~StationID)

#test to figure out when we get the biggest temperature difference bweteen beldens and rio vista

Tempsb = filter(TempsA, StationID %in% c("BDL", "RVB")) %>%
  pivot_wider(names_from = StationID, values_from = Value) %>%
  mutate(Diff = BDL-RVB, Mean = (BDL+RVB)/2) %>%
  filter(Diff <10)

ggplot(Tempsb, aes(x = Yday, y = Diff, color = year(ObsDate))) + geom_point()+
  scale_color_viridis_c(option = "B")

ggplot(Tempsb, aes(x = Yday, y = Diff, color = as.factor(year(ObsDate)))) + geom_point()+
  scale_color_viridis_d(option = "B")+
  facet_wrap(~year(ObsDate))+
  geom_hline(yintercept =0)


###########################################################################
#what about chlorophyll?


chl = cdec_query(c("BDL", "RVB", "NSL", "GZL"), sensors = c(28),
                   start.date = as.Date("2017-06-01"), end.date = today()) %>%
  filter(Value <300, Value >0) %>%
  mutate(Yday = yday(ObsDate))

ggplot(chl, aes(x = ObsDate, y = Value, color = StationID))+
  geom_line()+
  geom_hline(yintercept = 25)+
  facet_wrap(~StationID)

ggplot(chl, aes(x = ObsDate, y = Value, color = StationID))+
  geom_line()+
  coord_cartesian(xlim = c(ymd_hms("2023-08-15 00:00:00"), today()),
                  ylim=c(0,100))+
  theme_bw()

dailychl = mutate(chl, Date = date(ObsDate)) %>%
  group_by(Date, Yday, StationID) %>%
  summarize(Value = mean(Value, na.rm =T))


ggplot(dailychl, aes(x = Yday, y = Value, color = as.factor(year(Date))))+
  geom_line()+
  facet_wrap(~StationID, scales = "free_y")+
  ylab("Chlorophyl")+
  xlab("Day of year")

###########################################################
#make some plots for Lenny's stupid memo


#pull data from both sites, attach water year type, average by water year type and DOY, compare to this year

WQ = cdec_query(c("BDL","RVB"), sensors = c(100, 25, 27),
                start.date = as.Date("2000-01-01"), end.date = as.Date("2024-11-30"))
str(WQ)

ggplot(WQ, aes(x = DateTime, y = Value, color = StationID)) + facet_wrap(~SensorType, scales = "free_y")+
  geom_line()

WQx = mutate(WQ, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                                   SensorNumber == 25 ~ (Value - 32)*5/9,
                                   SensorNumber == 25 & Value >25 ~ NA,
                                   TRUE~ Value),
            Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                             labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0, !(SensorNumber == 25 & Value2>25))


cuttoffs = data.frame(Analyte = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"),
                      cutoff = c(6, 10, 22, 12))

WQdaily = WQx %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T)) %>%
  mutate(DOY = yday(Date), Year = year(Date), Month = month(Date),
         WY = case_when(Month %in% c(11,12) ~ Year +1,
                        TRUE ~ Year))

wyrs = read_csv("data/wtryrtype.csv") %>%
  select(WY, Index, `Yr-type`)

WQdaily = left_join(WQdaily, wyrs)

WQdaily2 = mutate(WQdaily, Yr_type = case_when(WY == 2023 ~ "2023",
                                              TRUE ~ `Yr-type`),
                 DOWY = case_when(DOY >305 ~ DOY-305,
                                  DOY<=305 ~ DOY +60)) %>%
  filter(DOWY < 365)

WQeyars = group_by(WQdaily2, DOWY, Yr_type, StationID, SensorType, Analyte) %>%
                   summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T)) %>%
  mutate(Station = factor(StationID, levels = c("RVB", "BDL"), labels = c("Rio Vista", "Suisun Marsh")),
         Yr_type = factor(Yr_type, levels = c("C", "D", "BN", "AN", "W" ,"2023"),
                          labels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2023")))

ggplot(WQeyars, aes(x = DOWY, y = Value2, color = Yr_type)) + geom_line()+
  facet_wrap(~StationID + Analyte, scales = "free_y")

#just summer-fall

WQyearsSF = filter(WQeyars, DOWY>200)

ggplot(filter(WQyearsSF, Yr_type !=2023), aes(x = DOWY, y = Value2, color = Yr_type)) + geom_smooth()+
  facet_wrap(~Station + Analyte, scales = "free_y")+
  scale_color_brewer(palette = "Dark2", name = "Water Year\nType")+
  geom_hline(data = filter(cuttoffs, Analyte != "Chlorophyll"), aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  geom_line(data=filter(WQyearsSF, Yr_type == "2023"), linewidth = 1, color = "black")+
  scale_x_continuous(breaks = c(212, 250, 280, 310, 340), labels = c("Jun", "Jul", "Aug", "Sep", "Oct"))+
  ylab(NULL)+xlab("Day of Year")+ theme_bw()+
  theme(legend.position = "bottom")


########################################################################
#look at just turbidity to see how often turbidity is higher in dry years

turb = cdec_query(c("BDL","RVB", "CSE", "MAL", "NSL", "OMR"), sensors = c(27),
                start.date = as.Date("2000-01-01"), end.date = today())

#filter out bad values and calculate daily mean
turbdaily = turb %>%
  mutate(Date = date(DateTime)) %>%
  filter(Value>0, Value<1000) %>%
  group_by(Date, StationID, SensorType) %>%
  summarize(Value = mean(Value, na.rm = T)) %>%
  mutate(DOY = yday(Date), Year = year(Date), Month = month(Date),
         WY = case_when(Month %in% c(11,12) ~ Year +1,
                        TRUE ~ Year))

ggplot(Data, aes(x = DOY, y = Value)) +
  geom_point()+
  geom_line()+
  facet_wrap(~Year)

wyrs = read_csv("data/wtryrtype.csv") %>%
  select(WY, Index, `Yr-type`)

#attach water year types
turbdaily = left_join(turbdaily, wyrs)

#calculate day of water year
turbdaily2 = mutate(turbdaily, Yr_type = case_when(WY == 2023 ~ "2023",
                                               TRUE ~ `Yr-type`),
                  DOWY = case_when(DOY >305 ~ DOY-305,
                                   DOY<=305 ~ DOY +60)) %>%
  filter(DOWY < 365)

turbyears = group_by(turbdaily2, DOWY, Yr_type, StationID, SensorType) %>%
  summarize(Value = mean(Value, na.rm = T)) %>%
  mutate(Station = factor(StationID, levels = c("RVB", "BDL","CSE", "MAL", "NSL", "OMR"), 
                          labels = c("Rio Vista", "Suisun Marsh", "Collinsville", "Mallard Island",
                                     "National Steel", "OMR")),
         Yr_type = factor(Yr_type, levels = c("C", "D", "BN", "AN", "W" ,"2023"),
                          labels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2023")))

ggplot(turbyears, aes(x = DOWY, y = Value, color = Yr_type)) + geom_line()+
  facet_wrap(~StationID, scales = "free_y")

#just summer-fall

turbyearssf = filter(turbyears, DOWY>200)

ggplot(filter(turbyears, Yr_type !=2023), aes(x = DOWY, y = Value, color = Yr_type)) + geom_smooth()+
  facet_wrap(~Station, scales = "free_y")+
  scale_color_brewer(palette = "Dark2", name = "Water Year\nType")+
  geom_line(data=filter(turbyears, Yr_type == "2023"), linewidth = 1, color = "black")+
  scale_x_continuous(breaks = c(60, 120, 180, 212, 250, 280, 310, 340), labels = c("Jan", "Mar", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  ylab(NULL)+xlab("Day of Year")+ theme_bw()


ggplot(filter(turbyears, Yr_type !=2023), aes(x = DOWY, y = Value, color = Yr_type)) + geom_line()+
  facet_wrap(~Station, scales = "free_y")+
  scale_color_brewer(palette = "Dark2", name = "Water Year\nType")+
  geom_line(data=filter(turbyears, Yr_type == "2023"), linewidth = 1, color = "black")+
  scale_x_continuous(breaks = c(60, 120, 180, 212, 250, 280, 310, 340), labels = c("Jan", "Mar", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  ylab(NULL)+xlab("Day of Year")+ theme_bw()

########################################################################
#Plot Delta Outflow
load("data/Dayflow_allw2023.RData")
wytype = read_csv("data/wateryeartypes.csv") 

dayflow2024 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/6a7cb172-fb16-480d-9f4f-0322548fee83/download/dayflowcalculations2024.csv")

DF = Dayflow %>%
  bind_rows(dayflow2024) %>%
  mutate( Year = year(Date)) %>%
  left_join(wytype)  

save(DF, file = "data/Dayflow_allw2024")
#how does outflow in spring of a dry year look compared to summer/fall of a dry year?


DFtest = DF %>%
  mutate(Month = month(Date), DOY = yday(Date)) %>%
  filter( Year %in% c(2000:2024)) %>%
  select(OUT, X2, CVP, SWP, Date, Month, DOY, Year, YT) %>%
  filter(OUT>0) %>%
  mutate(YT = factor(YT, levels = c("C", "D", "BN", "AN", "W")))


ggplot(DFtest, aes(x = DOY, y = OUT, group = as.factor(Year), color = YT)) + 
  geom_line()+
  theme_bw()+
  ylab("Delta Outflow Index (cfs)")+
  xlab("Day of Year")+
  scale_color_manual(values = c("orangered", "orange", "gold3", "springgreen3", "blue"), name = "Year Type")+
  scale_x_continuous(breaks = c(31, 90, 152, 182, 213, 244, 274, 305), labels = c("Feb", "Apr", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))


ggplot(DFtest, aes(x = DOY, y = OUT, color = YT)) + 
  geom_smooth()+
  theme_bw()+
  ylab("Delta Outflow Index (cfs)")+
  xlab("Day of Year")+
  scale_color_manual(values = c("orangered", "orange", "gold3", "springgreen3", "blue"), name = "Year Type")+
  scale_x_continuous(breaks = c(31, 90, 152, 182, 213, 244, 274, 305), labels = c("Feb", "Apr", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))


ggplot(DFtest, aes(x = DOY, y = OUT/43559, color = YT)) + 
  geom_smooth()+
  theme_bw()+
  ylab("Delta Outflow Index (acre feet per sec)")+
  xlab("Day of Year")+
  scale_color_manual(values = c("orangered", "orange", "gold3", "springgreen3", "blue"), name = "Year Type")+
  scale_x_continuous(breaks = c(31, 90, 152, 182, 213, 244, 274, 305), labels = c("Feb", "Apr", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))


#how many acre feet per second in each month?

afs = group_by(DFtest,Month, YT) %>%
  summarize(OUT = mean(OUT, na.rm =T))

#if we have 100 TAF, and we want to use it over a 7 day period, this would increase our outflow by...
100000/(86400*7)*43559

#it's probably not possible to use it that fast, but over a month

afs = mutate(afs, OUT2 = OUT + 100000/(86400*30)*43559, OUTpercent = OUT2/OUT*100)

100000/(86400*30)*43559

################################



 # Outflow2023 = read_excel("data/ITP_COA_8.20 - 2023 data.xlsx") %>%
 #   select(Date, OUT, CVP, SWP) %>%
 #  mutate(Year = year(Date), YT = "W") %>%
 #   filter(Date> ymd("2023-09-30"))
 # 
# Outflow2024 = read_excel("data/data for ITP COA 8.20 (2024).xlsx") %>%
#   select(Date, OUT, CVP, SWP) %>%
#   mutate(Year = year(Date), YT = "2024") %>%
#   filter(Date> ymd("2024-06-01"))


Outflow2024 = cdec_query("DTO",23, durations = "D", start.date = as.Date("2024-10-01"), as.Date("2024-10-31")) %>%
  rename(Date = DateTime, OUT = Value) %>%
  select(Date, OUT) %>%
  mutate(Year = year(Date), YT = case_when(Year ==2024 ~ "2024",
                                           Year == 2023 ~ "W"))

DFw2023 = bind_rows(DF, Outflow2023, Outflow2024) %>%
  mutate(Month = month(Date), DOY = yday(Date)) %>%
  filter(Month %in% c(6:10), Year %in% c(2017:2024)) %>%
  select(OUT, X2, CVP, SWP, Date, Month, DOY, Year, YT) %>%
  mutate(YT = case_when(Year == 2024 ~ "2024",
                        TRUE ~ YT)) %>%
  mutate(YT = factor(YT, levels = c("C", "D", "BN", "W", "2024"), 
                     labels = c("Critical", "Dry", "Below Normal", "Wet", "2024")))

ggplot(DFw2023, aes(x = DOY, y = OUT, group = as.factor(Year), color = YT, 
                    linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("Delta Outflow Index (cfs)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 7),  1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3", "blue", "black", "pink"), 
                     name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), 
                     labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))

ggsave("plots/NDOI2024.tiff", device = "tiff", width =6.5, height =4.5)

######extraflow###########################################
#Mike wants "unmanaged" flow versus "managed" flow

#D-1641 minimum flow reqiure,emts
DOYtomonths = data.frame(DOY = c(1:365), Month = c(rep(1, 31), rep(2, 28), rep(3, 31), rep(4, 30),
                                                   rep(5, 31), rep(6, 30), rep(7, 31), rep(8, 31),
                                                   rep(9, 30), rep(10, 31), rep(11, 30), rep(12, 31)))
D1641 = read_excel("data/D1641 standards.xlsx") %>%
  full_join(DOYtomonths) %>%
  mutate(YT = factor(YrType, levels = c("C", "D", "BN", "AN", "W", "2024"),
                     labels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2024"))) %>%
  filter(DOY %in% c(145:310))

#I'm not even sure this is right. We certainly don't meet the July standard in most wet years. Maybe it's not right
ggplot(DFw2023, aes(x = DOY, y = OUT, group = as.factor(Year), color = YT, 
                    linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("Delta Outflow Index (cfs)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 7),  1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3", "blue", "black", "pink"), 
                     name = "Year Type")+
  geom_line(data = D1641, aes(x = DOY, y = OUTminimum, color = YT), inherit.aes = F, linetype = 2)+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), 
                     labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))


# #plot for bay delta science conference talk - all months
# 
# DFw2023all = bind_rows(DF, Outflow2023, Outflow2024) %>%
#   mutate(Month = month(Date), DOY = yday(Date)) %>%
#   filter(Year %in% c(2017:2024), OUT >0) %>%
#   select(OUT, X2, CVP, SWP, Date, Month, DOY, Year, YT) %>%
#   mutate(YT = case_when(Year == 2023 ~ "W",
#                         Year == 2024 ~ "2024",
#                         TRUE ~ YT)) %>%
#   mutate(YT = factor(YT, levels = c("C", "D", "BN", "W", "2024"), labels = c("Critical", "Dry", "Below Normal", "Wet", "2023")))
# 
# ggplot(DFw2023all, aes(x = DOY, y = OUT, group = as.factor(Year), color = YT, linewidth = as.factor(Year))) + 
#   geom_line()+
#   theme_bw()+
#   ylab("Delta Outflow Index (cfs)")+
#   xlab("Day of Year")+
#   scale_linewidth_manual(values = c(rep(.7, 7),  1.4), guide = NULL)+
#   scale_color_manual(values = c("orangered", "orange", "gold3", "blue", "black"), name = "Year Type")+
#   scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
#   theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))
# 
# #maybe I just want 2023
# ggplot(filter(DFw2023all, Year == 2023), aes(x = DOY, y = OUT)) + 
#   geom_line(linewidth =1.5, color = "blue")+
#   theme_bw()+
#   ylab("Delta Outflow Index (cfs)")+
#   xlab("Day of Year")+
#   scale_x_continuous(breaks = c(31, 90, 152,  213, 274), labels = c("Feb", "Apr", "Jun",  "Aug", "Oct"))+
#   theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))
# 
# #maybe I just want 2024
# ggplot(filter(DFw2023all, Year == 2024), aes(x = DOY, y = OUT)) + 
#   geom_line(linewidth =1.5, color = "blue")+
#   theme_bw()+
#   ylab("Delta Outflow Index (cfs)")+
#   xlab("Day of Year")+
#   scale_x_continuous(breaks = c(31, 90, 152,  213, 274), labels = c("Feb", "Apr", "Jun",  "Aug", "Oct"),
#                      limits = c(0, 300))+
#   theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))

#2024 through April for presentation
ggplot(filter(DFw2023all, Year == 2024, DOY < 180), aes(x = DOY, y = OUT)) +
  geom_line(linewidth =1.5, color = "blue")+
  theme_bw()+
  ylab("Delta Outflow Index (cfs)")+
  xlab("Day of Year")+
  scale_x_continuous(breaks = c(31, 90, 152,  213, 274), labels = c("Feb", "Apr", "Jun",  "Aug", "Oct"),
                     limits = c(0, 300))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))
# 
# Exports2024 = cdec_query(stations = c("TRP", "HRO"), sensors = 70, start.date = ymd("2023-10-01"),
#                          end.date = today()) %>%
#   pivot_wider(id_cols = c(DateTime), names_from = StationID, values_from = Value) %>%
#   mutate(DOY = yday(DateTime), CVP = HRO, SWP = TRP, Year = year(DateTime), Date = date(DateTime)) 
# 
# Ex2024 = group_by(Exports2024, Date, DOY, Year) %>%
#   summarise(SWP = mean(SWP, na.rm = TRUE), CVP = mean(CVP, na.rm =T)) %>%
#   mutate(YT = case_when(Year == 2023 ~ "Wet",
#                         Year == 2024 ~ "2024"),
#          Month = month(Date)) %>%
#   filter(Month %in% c(6:10))

# DFw2024 = bind_rows(DFw2023, Ex2024) %>% 
#   filter(!is.na(SWP), !is.na(YT)) %>%
#   mutate(YT = factor(YT, levels = c("Critical", "Dry", "Below Normal", "Wet", "2024")))

ggplot(DFw2023, aes(x = DOY, y = CVP+SWP, group = as.factor(Year), 
                    color = YT, linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("CVP + SWP Exports (cfs)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 7), 1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3", "blue", "black", "pink"), name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))

ggsave("plots/exports2023.tiff", device = "tiff", width =6.5, height =4.5)

#now the plot of X2
# X2w2023 = mutate(X2, YT = "2023", Date = date(DateTime), Year = year(Date), DOY = yday(DateTime), X2 = X2km) %>%
#   select(X2, Date, YT, Year, DOY, DataFlag) %>%
#   bind_rows(DF) %>%
#   mutate(DOY = yday(Date), Month = month(Date),
#          YT = factor(YT, levels = c("C", "D", "BN", "W", "2023"), labels = c("Critical", "Dry", "Below Normal", "Wet", "2023")))%>%
#   filter(Year >2016, Month %in% c(6:10))
  
X2 = cdec_query("CX2", sensors = 145,
                start.date = as.Date("2024-09-30"), end.date =  as.Date("2024-11-01"))

X2 = mutate(X2, X2 = case_when(DataFlag == "v" & DateTime > ymd_hm("2023-07-01 11:11")~ 81,
                                 TRUE~ Value), Date = as.Date(DateTime), Month = month(Date),
            Year = year(Date), YT = case_when(Year == 2023 ~"Wet",
                                              Year == 2024 ~ "2024"),
            DOY = yday(Date))

X2w20232 = bind_rows(DFw2023, X2)  %>%
  filter(!is.na(X2), Month %in% c(6:10))
#what was the monthly average X2 in year year and month?

monthlyx2 = mutate(X2w20232, Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarize(X2 = mean(X2, na.rm = T))

write.csv(monthlyx2, "outputs/monthlyx2.csv")

ggplot(X2w20232, aes(x = DOY, y = X2, group = as.factor(Year), 
                     color = YT, linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("X2 (km)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 7), 1.4), guide = NULL)+
  scale_color_manual(values = c( "black","orangered", "orange", "gold3", "blue", "pink"), 
                     name = "Year Type")+
  geom_point(data = filter(X2w20232, DataFlag == "v"), 
             aes(x = DOY, y = X2, shape = "X2 >81"), color = "green3")+
  scale_shape_manual(values = 16, name = NULL)+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), 
                     labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))


ggsave("plots/X22024.tiff", device = "tiff", width =6.5, height =4.5)


######################################################################
#BDL salinity versus previous years

BDL = cdec_query("BDL", sensors = 100,
                start.date = as.Date("2011-06-01"), end.date = as.Date("2024-10-31"))
BDLdaily = filter(BDL, Value >1, Value < 30000, year(DateTime) >2016) %>%
  mutate(Date = date(DateTime), Year = year(DateTime), Month = month(DateTime)) %>%
  group_by(Date, Year, Month) %>%
  summarize(EC = mean(Value, na.rm = T), Salinity = ec2pss(EC/1000, 25)) %>%
  left_join(wytype) %>%
  mutate(DOY = yday(Date), YT = case_when(Year == 2024 ~ "2024",
                                          TRUE ~ YT),
         YT = factor(YT, levels = c("C", "D", "BN", "W", "2024"), 
                     labels = c("Critical", "Dry", "Below Normal", "Wet", "2024"))) %>%
  filter(Month %in% c(6:10))


ggplot(BDLdaily, aes(x = DOY, y = Salinity, group = as.factor(Year), color = YT, linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("Salinity at Belden's Landing (PSU)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 7), 1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3", "blue", "black"), name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  geom_hline(yintercept = 6, linetype =2, color = "green")+
  theme(legend.position = "bottom")

ggsave("plots/BDLsalinity2024.tiff", device = "tiff", width =6.5, height =4.5)


#now longer dataset with all the year types
BDLdaily2 = filter(BDL, Value >1, Value < 30000) %>%
  mutate(Date = date(DateTime), Year = year(DateTime), Month = month(DateTime)) %>%
  group_by(Date, Year, Month) %>%
  summarize(EC = mean(Value, na.rm = T), Salinity = ec2pss(EC/1000, 25)) %>%
  left_join(wytype) %>%
  mutate(DOY = yday(Date), YT = case_when(Year == 2024 ~ "AN",
                                          TRUE ~ YT),
         YT = factor(YT, levels = c("C", "D", "BN", "AN","W"), 
                     labels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))) %>%
  filter(Month %in% c(6:10))


ggplot(BDLdaily2, aes(x = DOY, y = Salinity, group = as.factor(Year), color = YT)) + 
  geom_line()+
  theme_bw()+
  ylab("Salinity at Belden's Landing (PSU)")+
  xlab("Day of Year")+
  scale_color_manual(values = c("orangered", "orange", "gold3", "cyan", "blue"), name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  geom_hline(yintercept = 4, linetype =2, color = "black")+
  theme(legend.position = "bottom")

################################################
DFw20232 = bind_rows(DF, Outflow2023) %>%
  mutate(Month = month(Date), DOY = yday(Date)) %>%
 # filter(Month %in% c(6:10), Year %in% c(2017:2023)) %>%
  select(OUT, X2, CVP, SWP, Date, Month, DOY, Year, YT) %>%
  mutate(YT = case_when(Year == 2023 ~ "2023",
                        TRUE ~ YT)) %>%
  mutate(YT = factor(YT, levels = c("C", "D", "BN", "W", "2023"), labels = c("Critical", "Dry", "Below Normal", "Wet", "2023")))


X2b = filter(DFw20232, !is.na(X2)) %>%
  ungroup()
X2monthly = group_by(X2b, Month, Year, YT) %>%
  summarize(X2 = mean(X2))

ggplot(X2b, aes(x = DOY, y = X2, color = YT, group = Year))+
  geom_line()

library(readxl)

oldX2 = read_excel("data/supplemental_data_wr.1943-5452.0000617_hutton3.xlsx")
names(oldX2) = c("Date", "X2", "SJRX2")

X2all = filter(oldX2, year(Date)<1997) %>%
  bind_rows(X2b) %>%
  select(Date, X2) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  left_join(yrs)

FallX2 = filter(X2all, Month %in% c(6:10)) %>%
  group_by(Year, `Yr-type`) %>%
  summarize(FallX2 = mean(X2)) %>%
  mutate(YT = factor(`Yr-type`, levels = c("C", "D", "BN","AN", "W")))

ggplot(FallX2, aes(x = Year, y = FallX2, fill = YT))+ geom_col()+
  geom_hline(yintercept = 80)+
  geom_hline(yintercept = 74, linetype = 2)+
  theme_bw()+
  ylab("Mean Jun-Oct X2, km")+
  scale_fill_manual(values = c("darkred", "orange", "yellow", "lightgreen", "darkblue"))
