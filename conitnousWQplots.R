#Water quality time series plots for 2020.

#Rosemary Hartman last updated 7/25/2023

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(wql)
library(RColorBrewer)

library(cder)

#plot the most recent months of data real quick

WQ = cdec_query(c("GZB", "GZM", "GZL", "BDL", "NSL", "RVB", "HUN", "MAL"), sensors = c(100, 25, 27, 28),
                start.date = as.Date("2023-06-01"), end.date = as.Date("2023-07-30"))
str(WQ)

ggplot(WQ, aes(x = DateTime, y = Value, color = StationID)) + facet_wrap(~SensorType, scales = "free_y")+
  geom_line()

WQ = mutate(WQ, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                               SensorNumber == 25 ~ (Value - 32)*5/9,
            TRUE~ Value),
            Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                             labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0)


cuttoffs = data.frame(Analyte = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"),
                      cutoff = c(6, 10, 22, 12))

#this is the plot for the monthly update
ggplot(WQ, aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
   theme_bw()       


#Do daily means instead
WQmean = WQ %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))


ggplot(WQmean, aes(x = Date, y = Value2, color = StationID)) + 
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line( linewidth =1)   + theme_bw() 

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


