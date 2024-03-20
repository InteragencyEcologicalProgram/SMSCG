#britt asks if we have any habitat data from before 2008 on wet years (no X2 action)

library(lubridate)
library(tidyverse)
library(cder)
library(wql)

WQold = cdec_query(c("HUN", "MSL", "BDL", "NSL", "RVB", "CSE"), sensors = c(100, 25, 27, 28),
                start.date = as.Date("2007-06-01"), end.date =  as.Date("2008-11-01"))


WQold2 = cdec_query(c("HUN", "MSL", "BDL", "NSL", "RVB", "CSE"), sensors = c(100, 25, 27, 28),
                   start.date = as.Date("2000-06-01"), end.date =  as.Date("2006-11-01"))


WQold3 = bind_rows(WQold, WQold2)

WQxold = mutate(WQold3, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                                    SensorNumber == 25 ~ (Value - 32)*5/9,
                                    SensorNumber == 25 & Value >30 ~ NA,
                                    TRUE~ Value),
             Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                              labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0, !(SensorNumber ==25 & Value2>26), !(SensorNumber ==27 & Value2>200), 
         !(SensorNumber ==28 & Value2>20))


cuttoffs = data.frame(Analyte = c("Salinity",  "Temperature", "Turbidity"),
                      cutoff = c(6,22, 12))

#15-minute data
ggplot(WQxold, aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
  theme_bw()   

#just BDL and RVB for smelt cages
ggplot(filter(WQx, StationID %in% c("RVB", "BDL")), aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
  theme_bw()   +
  coord_cartesian(xlim = c(ymd_hms("2023-08-15 00:00:00"), ymd_hms("2023-10-12 00:00:00")))


ggplot(filter(WQx, StationID %in% c("RVB", "BDL"), Analyte == "Temperature"), aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(yintercept = 25, color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
  theme_bw()   + ylab("Tempearature C")+
  coord_cartesian(xlim = c(ymd_hms("2023-08-15 00:00:00"), ymd_hms("2023-08-31 00:00:00")))


#Do daily means instead
WQmean = WQx %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))

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

