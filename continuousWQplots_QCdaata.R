
library(tidyverse)
library(lubridate)
library(wql)
library(RColorBrewer)
library(readxl)
library(janitor)
library(cder)


#upload all the data
BDL17 = cdec_query("BDL", sensors = 100,
                 start.date = as.Date("2017-06-01"), end.date = as.Date("2017-10-31")) %>%
  filter(Value >1, Value < 30000) %>%
  mutate(Date = date(DateTime), Year = year(DateTime), Month = month(DateTime), DOY = yday(Date)) %>%
  group_by(Date, Year, Month, DOY) %>%
  summarize(SC = mean(Value, na.rm = T), Salinity = ec2pss(SC/1000, 25))  %>%
  mutate(station_id = 49, station_name = "Beldens Landing")

MSL = read_csv("Data/MSL all.csv")%>%
  mutate(DATETIME = mdy_hm(DATETIME))
BDL = read_csv("Data/BDL all.csv") %>%
  mutate(DATETIME = mdy_hm(DATETIME))
NSL = read_csv("Data/NSL all.csv")%>%
  mutate(DATETIME = mdy_hm(DATETIME))
GOD = read_csv("Data/GOD all.csv")%>%
  mutate(DATETIME = mdy_hm(DATETIME))
GZM = read_csv("Data/GZM all.csv")%>%
  mutate(DATETIME = mdy_hm(DATETIME))
CSE = read_csv("Data/CSE all.csv")%>%
  mutate(DATETIME = mdy_hm(DATETIME))
GZB = read_csv("Data/GZB all.csv", skip =1)
names(GZB) = names(GZM)
GZB = mutate(GZB, DATETIME = mdy_hm(DATETIME))

GZL = read_csv("Data/GZL_2023_data.csv")
HON = read_csv("Data/HON_2023_data.csv")
MAL = read_csv("Data/MAL_2023_data.csv")
RVB = read_csv("Data/RVB_2023_data.csv")
SSI = read_csv("Data/SSI_2023_data.csv")
TRB = read_csv("Data/TRB_data_ESA.csv") %>%
  rename(datetime = date_time, spc = `Specific Conductivity`, watertemperature = `Temperature (C)`,
         fluorescence = `Chlorophyll-a`, turbidity = `Turbidity (NTU)`) %>%
  mutate(datetime = mdy_hm(datetime))


wytype = read_csv("data/wateryeartypes.csv") %>%
  mutate(YT = case_when(Year == 2023 ~ "2023",
                        TRUE ~ YT))
################################################
#suisun data
alldata = bind_rows(BDL, NSL, GOD, GZM, CSE, GZB, MSL)   %>%
  clean_names()

alldata = select(alldata, -temp_slope_prior, -temp_slope_after, -turb_slope_prior, -turb_slope_after,
                     -chl_slope_prior, -chl_slope_after)

cond = select(alldata, station_id, station_name, datetime, sp_cond_m_s_cm, sp_cond_qaqc_flag)  %>%
  filter(sp_cond_qaqc_flag != "X") %>%
  mutate(Value = ec2pss(sp_cond_m_s_cm/1000, 25), Analyte = "Salinity") %>%
  select(station_name, datetime, Analyte, Value)


turb = select(alldata, station_id, station_name, datetime, turbidity_ntu, turbidity_qaqc_flag)  %>%
  filter(turbidity_qaqc_flag != "X") %>%
  mutate(Analyte = "turbidity", Value = turbidity_ntu) %>%
  select(station_name, datetime, Analyte, Value)


temp = select(alldata, station_id, station_name, datetime, temp_c_2,temp_c, temp_wqp_qaqc_flag)  %>%
  mutate(temp = case_when(!is.na(temp_c_2) ~ temp_c_2,
                         !is.na(temp_c) ~ temp_c)) %>%
  mutate(Analyte = "watertemperature", Value = temp) %>%
  select( station_name, datetime, Analyte, Value)



chl = select(alldata,  station_id, station_name, datetime, chl_a_ug_l, chl_sop_process_qc_flag, chl_qaqc_flag, chl_a_mg_l) %>%
  mutate(CHL = case_when(!is.na(chl_a_ug_l) ~ chl_a_ug_l,
                         !is.na(chl_a_mg_l) ~ chl_a_mg_l)) %>%
  filter(chl_qaqc_flag != "X" & chl_sop_process_qc_flag != "X") %>%
  mutate(Analyte = "fluorescence", Value = CHL) %>%
  select( station_name, datetime, Analyte, Value)



alldata2 = bind_rows(turb, temp, chl, cond) %>%
  rename(station = station_name)


###########################################
#other data
alldata3 = bind_rows(GZL, HON, MAL, RVB, SSI) %>%
  mutate(datetime = ymd_hms(paste(as.character(date), as.character(time)))) %>%
  bind_rows(TRB) %>%
  mutate(Salinity = ec2pss(spc/1000, 25))

alldatalong = select(alldata3, station, datetime, fluorescence, Salinity, turbidity, watertemperature) %>%
  pivot_longer(cols = c(fluorescence, Salinity, turbidity, watertemperature), names_to = "Analyte", values_to = "Value")


reallyallthedata = bind_rows(alldata2, alldatalong) %>%
  filter(datetime >= as.Date("2023-06-01"))


cuttoffs = data.frame(Analyte = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"),
                      cutoff = c(6, 10, 22, 12))

######################################
#bdl salinity (deal with later)
alldataSC = filter(alldata, sp_cond_qaqc_flag == "G" ) %>%
  mutate(Date = date(datetime)) %>%
  group_by(station_id, station_name, Date) %>%
  summarize(SC = mean(sp_cond_m_s_cm), N = n()) %>%
  mutate(Year = year(Date), Month = month(Date), DOY = yday(Date)) %>%
  filter(Month %in% c(6:10)) %>%
  bind_rows(BDL17)%>%
  left_join(wytype)%>%
  mutate(YT = factor(YT, levels = c("C", "D", "BN", "W", "2023"), 
                     labels = c("Critical", "Dry", "Below Normal", "Wet", "2023"))) %>%
  mutate(Salinity = ec2pss(SC/1000, 25))


ggplot(filter(alldataSC, station_id == 49), aes(x = DOY, y = Salinity, color = YT, group = Year, linewidth = as.factor(Year)))+
  geom_line()+
  scale_linewidth_manual(values = c(rep(.7, 6), 1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3", "blue", "black"), name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme_bw()+
  geom_hline(yintercept = 6, color = "grey", linetype =2)+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))+
   ylab("Salinity, PSU") + xlab("Month")


ggsave("plots/BDLsalinity.tiff", device = "tiff", width =6.5, height =4.5)


####################################################################
#plot for report

WQmean2 = reallyallthedata %>%
  mutate(Date = date(datetime)) %>%
  group_by(Date, station, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T)) %>%
  mutate(station = case_when(station == "Beldons Landing" ~ "BDL",
                             station == "National Steel" ~ "NSL",
                             station == "Godfather II" ~ "GOD",
                             station == "Collinsville B" ~ "CSE",
                             station == "Grizzly Bay East" ~ "GZB",
                             station ==  "Montezuma Slough Mouth" ~ "GZM",
                             station ==  "Montezuma Slough" ~ "MSL",
                             TRUE ~ station))


stations = read_csv("Data/station_data.csv")

WQmeanallx = left_join(WQmean2, stations)


WQmeanally = mutate(WQmeanallx, region = factor(region, levels = c("Bay", "Marsh", "River"), 
                                                labels = c("Suisun Bay", "Suisun Marsh", "Sacramento River")),
                    Analyte2 = factor(Analyte, levels = c("fluorescence" , "Salinity", "watertemperature", "turbidity"),
                                      labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU")))
cuttoffs$Analyte2 = factor(cuttoffs$Analyte, levels = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                           labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU"))

ggplot(WQmeanally, aes(x = Date, y = Value))+
  geom_line(aes(color = station))+
  facet_grid(Analyte2~region, scales = "free_y")+
  geom_hline(data = filter(cuttoffs, Analyte2 != "Chlorophyll ug/L"), aes(yintercept = cutoff), color = "black",
             linetype =2, linewidth =1)+
  geom_hline(data = filter(cuttoffs, Analyte2 == "Chlorophyll ug/L"), aes(yintercept = cutoff), color = "grey",
             linetype =3, linewidth =1)+
  geom_vline(xintercept = as.Date("2023-08-15"), color = "red")+
  geom_vline(xintercept = as.Date("2023-10-17"), color = "red")+
  coord_cartesian(xlim = c(ymd("2023-06-01"), ymd("2023-10-31")))+
  theme_bw()+
  ylab(NULL)

ggsave("plots/AVGwq2023.png", device = "png", width =8, height =8)


########################################
#plot for smelt cage report


BDLRVBph = cdec_query(c("BDL", "RVB"), sensors = c(62),
                start.date = as.Date("2023-08-20"), end.date =  as.Date("2023-10-20")) %>%
  filter(Duration == "E") %>%
  select(station = StationID, datetime = DateTime, SensorType, Value) %>%
  mutate(station = case_when(station == "RVB" ~ "Rio Vista",
                             station == "BDL" ~ "Belden's Landing"),
         Analyte2 = "pH")


BDLRVB = filter(reallyallthedata, station %in% c("Beldons Landing", "RVB"),
                datetime > ymd_hm("2023-08-25 00:00"), datetime < ymd_hm("2023-10-20 00:00")) %>%
  mutate(Analyte2 = factor(Analyte, levels = c("fluorescence" , "Salinity", "watertemperature", "turbidity", "pH"),
                           labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU", "pH")),
         station = case_when(station == "RVB" ~ "Rio Vista",
                             station == "Beldons Landing" ~ "Belden's Landing")) %>%
  bind_rows(BDLRVBph)


ggplot(BDLRVB, aes(x = datetime, y = Value))+
  geom_line(aes(color = station))+
  facet_wrap(~Analyte2, scales = "free", nrow =3)+
  scale_color_manual(values = c( "#1B9E77" , "#7570B3"))+
  ylab(NULL)+xlab("Date")+
  theme(legend.position = "bottom")

###################################

#is it GZB or TRB?

trb = filter(alldata, station_name == "Tule Red at Breach")

gzb = filter(WQx, StationID == "GZB", analyte == "Salinity")

gzb = cdec_query(stations = "GZB", sensors = 100, start.date = as.Date("2021-01-01"), end.date = ("2023-11-01"))

ggplot() +
  geom_line(data = gzb, aes(x = DateTime, y = Value), color = "red")+
  geom_line(data = trb, aes(x = datetime, y = sp_cond_m_s_cm), color = "blue")
  

#is it GZB or TRB?

gzbchl = cdec_query(stations = "GZB", sensors = 28, start.date = as.Date("2021-01-01"), end.date = ("2023-11-01"))

GZB$DATETIME = mdy_hm(GZB$DATETIME)

ggplot() +
  geom_line(data = gzbchl, aes(x = DateTime, y = Value), color = "red")+
  geom_line(data = GZB, aes(x = DATETIME, y = `Chl a (µg/L)`), color = "blue")


ggplot() +
  geom_line(data = gzb, aes(x = DateTime, y = Value), color = "red")+
  geom_line(data = GZB, aes(x = DATETIME, y = `SpCond (µS/cm)`), color = "blue")+
  ylab("Specific Conductance")



