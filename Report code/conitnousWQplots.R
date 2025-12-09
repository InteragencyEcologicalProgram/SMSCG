#Water quality time series plots for 2025.
#this is the in-progress data, not QAQC'd

#Rosemary Hartman last updated 12/9/2025

library(tidyverse)
library(lubridate)
library(dataRetrieval)

library(wql)
library(RColorBrewer)
library(readxl)
library(cder)

#water year assignments
yrs = read_csv("data/wtryrtype.csv")

#Plot of X2, based on CDEC ###################################
#x2 plot
X2 = cdec_query("CX2", sensors = 145,
                start.date = as.Date("2016-06-01"), end.date =  as.Date("2025-11-01"))

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
  

ggplot(filter(X2, Year ==2025, X2km >65), aes(x = DOY, y = X2km)) +
  geom_point(aes(color = DataFlag))+ geom_line()+
  
  coord_cartesian(xlim = c(150, 277), ylim = c(65, 85))+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  ylab("X2")+
  xlab("Date")+
  scale_color_manual(values = c("black", "red"), labels = c("Value", ">81km"))+
  theme_bw()
#huh, that's odd

#what about Dleta outflow and inflow and stuff?
Outflow2025 = cdec_query("DTO",23, durations = "D", start.date = as.Date("2025-6-01"), as.Date("2025-10-31")) %>%
  rename(Date = DateTime, OUT = Value) %>%
  select(Date, OUT) 

ggplot(Outflow2025, aes(x = Date, y = OUT))+ geom_line() + geom_point()+ 
  ylab("Outflow (CFS) from CDEC station DTO")
#some glitches, Ian says they were errors

#########monthly update stuff ####################################################################################
#plot the most recent months of data real quick

WQ = cdec_query(c("GZB", "GZM", "GZL", "BDL", "NSL", "RVB",  "HUN", "CSE"), 
                sensors = c(100, 25, 27, 28),
                start.date = as.Date("2025-06-01"), end.date =  as.Date("2025-11-01"))
str(WQ)

ggplot(WQ, aes(x = DateTime, y = Value, color = StationID)) + facet_wrap(~SensorType, scales = "free_y")+
  geom_line()+
  geom_vline(xintercept = ymd_hm("2025-06-23 09:50"))+
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


cuttoffs = data.frame(Analyte = c("Salinity", "Chlorophyll", "Temperature", "Turbidity", "Temperature"),
                      cutoff = c(6, 10, 22, 12, 25))

#15-minute data
ggplot(WQx, aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
   theme_bw()   #+
  #coord_cartesian(xlim = c(ymd_hms("2024-06-01 00:00:00"), now()))


#Do daily means instead
WQmean = WQx %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))



###############################################################################################
#BDL salinity to match the modeling graph

ggplot(filter(WQmean, StationID == "BDL", Analyte== "Salinity"), aes(x = Date, y = Value2))+
  geom_line()+
  ylab("psu")+
  theme_bw()

ggplot(filter(WQx, StationID == "BDL", Analyte== "Salinity"), aes(x = ObsDate, y = Value2))+
  geom_line()+
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

########################################################################################


gatedates24 = data.frame(StartDate = c(ymd("2024-07-01"), ymd("2024-09-06"), ymd("2024-09-01"), ymd("2024-10-28")),
                       EndDate = c(ymd("2024-08-29"), ymd("2024-09-30"), ymd("2024-09-30"), ymd("2024-11-04")),
                       Type = c("SMSCG", "SMSCG", "X2@80km", "SMSCG"),
                       xval = c(ymd("2024-07-15"), ymd("2024-09-06"), ymd("2024-09-01"), ymd("2024-10-20")),
                       ynudge = c(0,-1, 1, 1))

gatedates25 = data.frame(StartDate = c(ymd("2025-06-23"), ymd("2025-09-05")),
                         EndDate = c(ymd("2025-08-26"), ymd("2025-09-12")),
                         Type = c("SMSCG", "SMSCG"),
                         xval = c(ymd("2025-08-25"), ymd("2025-08-25")),
                         ynudge =c(0, 0))

yvals = data.frame(Analyte = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                   yval = c(11,8,25,95), yoff = c(0.08, 0.08, 0.015, 0.08))

gatedates2 = cross_join(gatedates25, yvals)

#plot for monthly update #######################################
ggplot(filter(WQmean, Date != ymd("2025-09-24")),
       aes(x = Date, y = Value2, color = StationID)) + 
   geom_rect(data = gatedates25, aes(ymin = -Inf, ymax = Inf,xmin = StartDate, xmax = EndDate,
                                   fill = Type), inherit.aes = FALSE, alpha = 0.4)+
scale_fill_manual(values = c("grey60", "tan1"), name = "Action\nPeriod")+
 geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line( linewidth =1)   + theme_bw() +ylab(NULL)# +
 #geom_text(data = gatedates2, aes(y = yval+yoff*yval*ynudge,x = xval, label = Type), 
        #    inherit.aes = FALSE, hjust =0)

filter(WQmean, Date == ymd("2025-09-16"))

ggsave("plots/ContWQ_2025.tiff", width = 8, height =6, device = "tiff")

#version with specific conductance for landowners ################################
#6ppt is similar about 10600 uS/mm
cuttoffs2 =  data.frame(Analyte = c("Salinity", "Temperature"),
                        cutoff = c(10600, 71.6))

ggplot(filter(WQmean,  !StationID %in% c("GZB", "GZM"), Date != ymd("2025-09-16"), 
              Analyte != "Chlorophyll", Analyte != "Turbidity"),
       aes(x = Date, y = Value, color = StationID)) + 
    geom_rect(data = gatedates25, aes(ymin = -Inf, ymax = Inf,xmin = StartDate, xmax = EndDate), 
            inherit.aes = FALSE, alpha = 0.4, fill = "grey")+
  scale_color_brewer(palette = c("Dark2"), labels = c("Beldens Landing", "Collinsville", "Grizzly Bay", "Hunter Cut", 
                                                      "National Steel", "Rio Vista"))+
  geom_hline(data = cuttoffs2, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  geom_vline(xintercept = ymd("2023-08-15"), linewidth =1)+
  facet_wrap(~Analyte, scales = "free_y", nrow  =2)+
  geom_line( linewidth =1)   + theme_bw() +ylab(NULL)



#just salinity

ggplot(droplevels(filter(WQmean,Analyte == "Salinity", Date != ymd("2025-09-21"))), aes(x = Date, y = Value2, color = StationID)) + 
  geom_hline(aes(yintercept = 6), color = "red", 
             linetype =2, linewidth =1)+
  geom_line( linewidth =1)   + theme_bw() 


###########################################################################################
#plot of 2025 data for SF report ##############################################
#add a few more stations - to be updated with QAQC'd stuff later

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
  coord_cartesian(xlim = c(ymd("2025-06-01"), ymd("2025-10-31")))+
  scale_fill_manual(values = c("lightblue", "grey"))+
  theme_bw()+
  ylab(NULL)

                  ggsave("plots/AVGwq2025.png", device = "png", width =8, height =8)
                  
save(WQmeanally, gatedates, file = "data/WQfor2025report_notQCd.Rdata")



#########################################################
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
#make some plots for the 100 TAF memo #######################################
 

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

#add wate ryear type
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

#plot of water quality versus DOY by water year type
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
