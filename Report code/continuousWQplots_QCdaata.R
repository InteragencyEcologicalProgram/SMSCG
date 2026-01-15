
library(tidyverse)
library(lubridate)
library(wql)
library(RColorBrewer)
library(readxl)
library(janitor)
library(cder)

#upload teh data Morgan nicely organized for me
#once Jamel does the 2025 data, replace it here. 

WQdata = read_csv("Data/SMSCG_wq_data_2017-2025_final.csv")
WQdaily = WQdata %>%
  mutate(Date = date(date_time_pst)) %>%
  mutate(Salinity = ec2pss(SpC, 25)) %>%
  select(-SpC) %>%
  pivot_longer(cols = c(Turbidity, WaterTemperature, Salinity,  Fluorescence), names_to = "Analyte", values_to = "Value") %>%
  group_by(Date, station, year, region, group, water_year_type, Analyte) %>%
  summarize(Value = mean(Value, na.rm =T)) %>%
  mutate(DOY = yday(Date),YT = case_when(year == 2025 ~ "2025",
                        TRUE ~ water_year_type), YT = factor(YT, levels = c("critically dry", "dry", "below normal", 
                                                                            "above normal", "wet", "2025"),
                                       labels = c("critical", "dry", "below normal", "above normal", "wet", "2025")),
         )


ggplot(filter(WQdaily, Analyte == "WaterTemperature", region == "Marsh"), aes(x = Date, y = Value, color = station))+
  geom_line()+ geom_hline(yintercept = 25)+ geom_hline(yintercept = 22, linetype =2)

# 
# #upload all the data - this was the orgional version where things weren't organized.
# BDL17 = cdec_query("BDL", sensors = 100,
#                  start.date = as.Date("2017-06-01"), end.date = as.Date("2017-10-31")) %>%
#   filter(Value >1, Value < 30000) %>%
#   mutate(Date = date(DateTime), Year = year(DateTime), Month = month(DateTime), DOY = yday(Date)) %>%
#   group_by(Date, Year, Month, DOY) %>%
#   summarize(SC = mean(Value, na.rm = T), Salinity = ec2pss(SC/1000, 25))  %>%
#   mutate(station_id = 49, station_name = "Beldens Landing")
# 
# MSL = read_csv("Data/MSL all.csv")%>%
#   mutate(DATETIME = mdy_hm(DATETIME))
# BDL = read_csv("Data/BDL all.csv") %>%
#   mutate(DATETIME = mdy_hm(DATETIME))
# NSL = read_csv("Data/NSL all.csv")%>%
#   mutate(DATETIME = mdy_hm(DATETIME))
# GOD = read_csv("Data/GOD all.csv")%>%
#   mutate(DATETIME = mdy_hm(DATETIME))
# GZM = read_csv("Data/GZM all.csv")%>%
#   mutate(DATETIME = mdy_hm(DATETIME))
# CSE = read_csv("Data/CSE all.csv")%>%
#   mutate(DATETIME = mdy_hm(DATETIME))
# GZB = read_csv("Data/GZB all.csv", skip =1)
# names(GZB) = names(GZM)
# GZB = mutate(GZB, DATETIME = mdy_hm(DATETIME))
# 
# GZL = read_csv("Data/GZL_2023_data.csv")
# HON = read_csv("Data/HON_2023_data.csv")
# MAL = read_csv("Data/MAL_2023_data.csv")
# RVB = read_csv("Data/RVB_2023_data.csv")
# SSI = read_csv("Data/SSI_2023_data.csv")
# TRB = read_csv("Data/TRB_data_ESA.csv") %>%
#   rename(datetime = date_time, spc = `Specific Conductivity`, watertemperature = `Temperature (C)`,
#          fluorescence = `Chlorophyll-a`, turbidity = `Turbidity (NTU)`) %>%
#   mutate(datetime = mdy_hm(datetime))
# 
# 
# wytype = read_csv("data/wateryeartypes.csv") %>%
#   mutate(YT = case_when(Year == 2023 ~ "2023",
#                         TRUE ~ YT))
# ################################################
# #suisun data
# alldata = bind_rows(BDL, NSL, GOD, GZM, CSE, GZB, MSL)   %>%
#   clean_names()
# 
# alldata = select(alldata, -temp_slope_prior, -temp_slope_after, -turb_slope_prior, -turb_slope_after,
#                      -chl_slope_prior, -chl_slope_after)
# 
# cond = select(alldata, station_id, station_name, datetime, sp_cond_m_s_cm, sp_cond_qaqc_flag)  %>%
#   filter(sp_cond_qaqc_flag != "X") %>%
#   mutate(Value = ec2pss(sp_cond_m_s_cm/1000, 25), Analyte = "Salinity") %>%
#   select(station_name, datetime, Analyte, Value)
# 
# 
# turb = select(alldata, station_id, station_name, datetime, turbidity_ntu, turbidity_qaqc_flag)  %>%
#   filter(turbidity_qaqc_flag != "X") %>%
#   mutate(Analyte = "turbidity", Value = turbidity_ntu) %>%
#   select(station_name, datetime, Analyte, Value)
# 
# 
# temp = select(alldata, station_id, station_name, datetime, temp_c_2,temp_c, temp_wqp_qaqc_flag)  %>%
#   mutate(temp = case_when(!is.na(temp_c_2) ~ temp_c_2,
#                          !is.na(temp_c) ~ temp_c)) %>%
#   mutate(Analyte = "watertemperature", Value = temp) %>%
#   select( station_name, datetime, Analyte, Value)
# 
# 
# 
# chl = select(alldata,  station_id, station_name, datetime, chl_a_ug_l, chl_sop_process_qc_flag, chl_qaqc_flag, chl_a_mg_l) %>%
#   mutate(CHL = case_when(!is.na(chl_a_ug_l) ~ chl_a_ug_l,
#                          !is.na(chl_a_mg_l) ~ chl_a_mg_l)) %>%
#   filter(chl_qaqc_flag != "X" & chl_sop_process_qc_flag != "X") %>%
#   mutate(Analyte = "fluorescence", Value = CHL) %>%
#   select( station_name, datetime, Analyte, Value)
# 
# 
# 
# alldata2 = bind_rows(turb, temp, chl, cond) %>%
#   rename(station = station_name)
# 
# ###########################################
# #other data
# alldata3 = bind_rows(GZL, HON, MAL, RVB, SSI) %>%
#   mutate(datetime = ymd_hms(paste(as.character(date), as.character(time)))) %>%
#   bind_rows(TRB) %>%
#   mutate(Salinity = ec2pss(spc/1000, 25))
# 
# alldatalong = select(alldata3, station, datetime, fluorescence, Salinity, turbidity, watertemperature) %>%
#   pivot_longer(cols = c(fluorescence, Salinity, turbidity, watertemperature), names_to = "Analyte", values_to = "Value")
# 
# 
# reallyallthedata = bind_rows(alldata2, alldatalong) %>%
#   filter(datetime >= as.Date("2023-06-01"))
# 
# 

# 
# 
# save(alldata2, alldata3, alldatalong, reallyallthedata, file = "data/WQdata2023.Rdata")
# 
# ######################################
# #bdl salinity (deal with later)
# alldataSC = filter(alldata, sp_cond_qaqc_flag == "G" ) %>%
#   mutate(Date = date(datetime)) %>%
#   group_by(station_id, station_name, Date) %>%
#   summarize(SC = mean(sp_cond_m_s_cm), N = n()) %>%
#   mutate(Year = year(Date), Month = month(Date), DOY = yday(Date)) %>%
#   filter(Month %in% c(6:10)) %>%
#   bind_rows(BDL17)%>%
#   left_join(wytype)%>%
#   mutate(YT = factor(YT, levels = c("C", "D", "BN", "W", "2023"), 
#                      labels = c("Critical", "Dry", "Below Normal", "Wet", "2023"))) %>%
#   mutate(Salinity = ec2pss(SC/1000, 25))

bdlwq = filter(WQdaily, station == "BDL", Analyte == "Salinity")

ggplot(bdlwq, aes(x = DOY, y = Value, color = YT, group = year, linewidth = as.factor(year)))+
  geom_line()+
  scale_linewidth_manual(values = c(rep(.7, 8), 1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3","green4", "blue", "black"), name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme_bw()+
  geom_hline(yintercept = 6, color = "grey", linetype =2)+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))+
  coord_cartesian(xlim = c(135, 320))+
   ylab("Salinity, PSU") + xlab("Month")


ggsave("plots/BDLsalinity.tiff", device = "tiff", width =6.5, height =4.5)


####################################################################
#plot for report

#start and stop dates for the SMSCGs - modify for year of interest

gatedates = data.frame(StartDate = c(ymd("2025-06-23"), ymd("2025-09-05")),
                         EndDate = c(ymd("2025-08-26"), ymd("2025-09-12")),
                         Type = c("SMSCG", "SMSCG"),
                         xval = c(ymd("2025-08-25"), ymd("2025-08-25")),
                         ynudge =c(0, 0))


yvals = data.frame(Analyte = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                   yval = c(11,8,25,95), yoff = c(0.08, 0.08, 0.015, 0.08))

gatedates2 = cross_join(gatedates, yvals)

#red line at 6 PSU salinity, 10 mg/L chlorophyll, 22 C temperature, and 12 NTU turbidity
 cuttoffs = data.frame(Analyte = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"),
                       cutoff = c(6, 10, 22, 12))


WQdaily2 = mutate(WQdaily, region = factor(region, levels = c("Bay", "Marsh", "River"), 
                                                labels = c("Suisun Bay", "Suisun Marsh", "Sacramento River")),
                    Analyte2 = factor(Analyte, levels = c("Fluorescence" , "Salinity", "WaterTemperature", "Turbidity"),
                                      labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU"))) %>%
  filter(year == 2025, DOY > 135, DOY <320)

cuttoffs$Analyte2 = factor(cuttoffs$Analyte, levels = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                           labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU"))

ggplot(filter(WQdaily2, DOY >135, DOY < 320),  aes(x = Date, y = Value))+
  geom_rect(data = gatedates, aes(ymin = -Inf, ymax = Inf,xmin = StartDate, xmax = EndDate,
                                  fill = Type), inherit.aes = FALSE, alpha = 0.4)+
  
  scale_fill_manual(values = c("grey60", "skyblue"), name = "Action\nPeriod")+
  geom_line(aes(color = station))+
  facet_grid(Analyte2~region, scales = "free_y")+
  geom_hline(data = filter(cuttoffs, Analyte2 != "Chlorophyll ug/L"), aes(yintercept = cutoff), color = "black",
             linetype =2, linewidth =1)+
  geom_hline(data = filter(cuttoffs, Analyte2 == "Chlorophyll ug/L"), aes(yintercept = cutoff), color = "grey",
             linetype =3, linewidth =1)+

  coord_cartesian(xlim = c(ymd("2025-06-01"), ymd("2025-10-31")))+
  theme_bw()+
  ylab(NULL)

ggsave("plots/AVGwq2025.png", device = "png", width =8, height =8)

#salinityonly
mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set3"))
ggplot(filter(WQmeanally, Analyte2 == "Salinity PSU"), aes(x = Date, y = Value))+
  geom_line(aes(color = station))+
  scale_color_manual(values = mypal)+
  facet_grid(Analyte2~region, scales = "free_y")+
  coord_cartesian(xlim = c(ymd("2025-06-01"), ymd("2025-10-31")))+
  theme_bw()+
  ylab(NULL)+
  theme(legend.position = "bottom")


