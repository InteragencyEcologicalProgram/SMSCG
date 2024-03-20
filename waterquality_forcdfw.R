#Water quality time series plots for 2020.

#Rosemary Hartman last updated 7/25/2023

library(tidyverse)
library(lubridate)
library(dataRetrieval)

library(wql)
library(RColorBrewer)

library(cder)

#plot the most recent months of data real quick

WQ = cdec_query(c("GZB", "GZM", "GZL", "BDL", "NSL", "RVB", "CSE"), sensors = c(100, 25, 27, 28),
                start.date = as.Date("2023-06-01"), end.date = today())
str(WQ)

#calculate salinity and convert temperature to celcisu
WQx = mutate(WQ, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                                    SensorNumber == 25 ~ (Value - 32)*5/9,
                                    SensorNumber == 25 & Value >30 ~ NA,
                                    TRUE~ Value),
             Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                              labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0, !(SensorNumber ==25 & Value2>26))

#smelt habitat cuttoffs
cuttoffs = data.frame(Analyte = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"),
                      cutoff = c(6, 10, 22, 12))

#15-minute data
ggplot(WQx, aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
  theme_bw()   +
  coord_cartesian(xlim = c(ymd_hms("2023-08-15 00:00:00"), now()))


#Do daily means instead
WQmean = WQx %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))


ggplot(WQmean, aes(x = Date, y = Value2, color = StationID)) + 
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  geom_vline(xintercept = ymd("2023-08-15"))+
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line( linewidth =1)   + theme_bw() +ylab(NULL)

#just rio vista and bdl for smelt cages
ggplot(filter(WQmean, StationID %in% c("RVB", "BDL")), aes(x = Date, y = Value2, color = StationID)) + 
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line( linewidth =1)   + theme_bw()


