#Grizzly Bay sonde comparison


library(tidyverse)
library(lubridate)
library(wql)
library(RColorBrewer)

library(cder)

#I got some of the QAQC'd data from Jamel. I forgot to ask for water temperature though.
#You should go back and ask him for data from 2020 and 2021 too.

WQ2022 = read_csv("Data/SMSG Data 2022.csv") 



# CEMP should have the QAQC'd data for GZL, and Jamel may have it for HUN, I"m not sure.
#but I just grabed it from CDEC to start.

WQ = cdec_query(c( "GZL",   "HUN"), sensors = c(100, 25, 27, 28),
                start.date = as.Date("2021-10-01"), end.date = as.Date("2022-10-31"))

# Make the cdec data match Jamel's data by renaming things.

WQp = mutate(WQ, time = DateTime) %>%
  rename(analyte_name = SensorType, cdec_code = StationID, value = Value, qaqc_flag_id = DataFlag) %>%
  mutate(analyte_name = case_when(analyte_name == "EL COND" ~ "Specific Conductance",
                                  analyte_name == "CHLORPH" ~ "Chlorophyll",
                                  analyte_name == "TURB W" ~ "Turbidity",
                                  analyte_name == "TEMP W" ~ "Water Temperature",
                                  TRUE ~ analyte_name)) %>%
  
  #get rid of columns I don't need
  select(time, cdec_code, analyte_name, value, qaqc_flag_id) %>%
  
  #convert temperature to celcius
  mutate(value = case_when(analyte_name == "Water Temperature" ~ (value-31)*5/9,
                           TRUE ~ value))

#bind the two datasets together and get rid of bad data
WQall = bind_rows(WQ2022, WQp) %>%
  filter(qaqc_flag_id != "X")

#calculate daily means
WQmean = WQall %>%
  mutate(Date = date(time)) %>%
  group_by(Date, cdec_code, analyte_name) %>%
  summarize(Value = mean(value, na.rm = T)) 

#make a quick plot
ggplot(WQmean, aes(x = Date, y = Value, color = cdec_code)) + 
  facet_wrap(~analyte_name, scales = "free_y")+
  geom_line(size = 1)   + theme_bw() 

#what's going on with GZL chlorophyll?
test = filter(WQmean, cdec_code == "GZL", analyte_name == "Chlorophyll")
ggplot(test, aes(x = Date, y = Value)) + 
  geom_point(aes(color = cdec_code), size = 1)   + theme_bw() + 
  geom_line(aes(color = cdec_code))+
  scale_color_discrete(guide = NULL)+
  ylab("Chlorophyll (ug/L)") + xlab("Date")+
  coord_cartesian(xlim = c(ymd("2022-06-01", "2022-10-31")))


#subset just hte Grizzly Bay stations we want to compare
Grizz = filter(WQmean, cdec_code %in% c("GZB", "HUN", "GZL", "GZM"), 
               analyte_name %in% c("Chlorophyll", "Water Temperature", "Specific Conductance", "Turbidity"))

#quick plot
ggplot(Grizz, aes(x = Date, y = Value, color = cdec_code)) +
  geom_point()+geom_line()+
  facet_wrap(~analyte_name, scales = "free_y")


