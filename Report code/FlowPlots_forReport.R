#Plots of delta outflow, X2, exports, for SFHA report

library(tidyverse)
library(cder)

#Plot Delta Outflow
load("data/Dayflow_allw2024.RData")
wytype = read_csv("data/wateryeartypes.csv") 


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


################################

# OUtflow for 2025 is not on CDEC yet
Outflow2025 = cdec_query("DTO",23, durations = "D", start.date = as.Date("2024-10-01"), as.Date("2025-10-31")) %>%
  rename(Date = DateTime, OUT = Value) %>%
  select(Date, OUT) %>%
  mutate(Year = year(Date), YT = case_when(Year ==2024 ~ "AN",
                                           Year == 2023 ~ "W",
                                           Year ==2025 ~ "2025",)) %>%
  filter(OUT >0)

DFw2025 = bind_rows(DF, Outflow2025) %>%
  mutate(Month = month(Date), DOY = yday(Date)) %>%
  filter(Month %in% c(6:10), Year %in% c(2017:2025)) %>%
  select(OUT, X2, CVP, SWP, Date, Month, DOY, Year, YT) %>%
  mutate(YT = case_when(Year == 2025 ~ "2025",
                        TRUE ~ YT)) %>%
  mutate(YT = factor(YT, levels = c("C", "D", "BN","AN", "W", "2025"), 
                     labels = c("Critical", "Dry", "Below Normal","Above Normal", "Wet", "2025")))

#plot for report
ggplot(DFw2025, aes(x = DOY, y = OUT, group = as.factor(Year), color = YT, 
                    linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("Delta Outflow Index (cfs)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 8),  1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3","seagreen", "blue", "black", "pink"), 
                     name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), 
                     labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))

ggsave("plots/NDOI2025.tiff", device = "tiff", width =6.5, height =4.5)

#now get export data ######################################
# 
Exports2025 = cdec_query(stations = c("TRP", "HRO"), sensors = 70, start.date = ymd("2024-10-01"),
                         end.date = today()) %>%
  pivot_wider(id_cols = c(DateTime), names_from = StationID, values_from = Value) %>%
  mutate(DOY = yday(DateTime), CVP = HRO, SWP = TRP, Year = year(DateTime), Date = date(DateTime))

Ex2025 = group_by(Exports2025, Date, DOY, Year) %>%
  summarise(SWP = mean(SWP, na.rm = TRUE), CVP = mean(CVP, na.rm =T)) %>%
  mutate(YT = case_when(Year == 2024 ~ "Above Normal",
                        Year == 2025 ~ "2025"),
         Month = month(Date)) %>%
  filter(Month %in% c(6:10))

#add to dayflow exports
DFw2025 = bind_rows(DFw2025, Ex2025) %>%
  filter(!is.na(SWP), !is.na(YT)) %>%
  mutate(YT = factor(YT, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2025")))

#plot for report
ggplot(DFw2025, aes(x = DOY, y = CVP+SWP, group = as.factor(Year), 
                    color = YT, linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("CVP + SWP Exports (cfs)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 8), 1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3","seagreen", "blue", "black", "pink"), 
                     name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))

ggsave("plots/exports2025.tiff", device = "tiff", width =6.5, height =4.5)

#now the plot of X2 ##################################

#data not on dayflow yet
X2 = cdec_query("CX2", sensors = 145,
                start.date = as.Date("2024-09-30"), end.date =  as.Date("2025-11-01"))

X2 = mutate(X2, X2 = case_when(DataFlag == "v" & DateTime > ymd_hm("2025-07-01 11:11")~ 81,
                               TRUE~ Value), Date = as.Date(DateTime), Month = month(Date),
            Year = year(Date), YT = case_when(Year == 2024 ~"Above Normal",
                                              Year == 2025 ~ "2025"),
            DOY = yday(Date))

#combine with dayflow
X2w2025 = bind_rows(DFw2025, X2)  %>%
  filter(!is.na(X2), Month %in% c(6:10), Date != "2025-09-25") %>%
  mutate(YT = factor(YT, levels =  c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2025")))
#what was the monthly average X2 in year year and month?

monthlyx2 = mutate(X2w2025, Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarize(X2 = mean(X2, na.rm = T))

write.csv(monthlyx2, "outputs/monthlyx2.csv")

ggplot(X2w2025, aes(x = DOY, y = X2, group = as.factor(Year), 
                    color = YT, linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("X2 (km)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 8), 1.4), guide = NULL)+
  scale_color_manual(values = c( "orangered", "orange", "gold3", "seagreen", "blue","black", "pink"), 
                     name = "Year Type")+
  geom_point(data = filter(X2w2025, DataFlag == "v"), 
             aes(x = DOY, y = X2, shape = "X2 >81"), color = "green3")+
  scale_shape_manual(values = 16, name = NULL)+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), 
                     labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom", legend.margin = margin(t=0, r = 0, b = 0, l = 0))


ggsave("plots/X22025.tiff", device = "tiff", width =6.5, height =4.5)


######################################################################
#BDL salinity versus previous years

BDL = cdec_query("BDL", sensors = 100,
                 start.date = as.Date("2011-06-01"), end.date = as.Date("2025-10-31"))
BDLdaily = filter(BDL, Value >1, Value < 30000, year(DateTime) >2016) %>%
  mutate(Date = date(DateTime), Year = year(DateTime), Month = month(DateTime)) %>%
  group_by(Date, Year, Month) %>%
  summarize(EC = mean(Value, na.rm = T), Salinity = ec2pss(EC/1000, 25)) %>%
  left_join(wytype) %>%
  mutate(DOY = yday(Date), YT = case_when(Year == 2025 ~ "2025",
                                          TRUE ~ YT),
         YT = factor(YT, levels = c("C", "D", "BN", "AN", "W", "2025"), 
                     labels = c("Critical", "Dry", "Below Normal","Above Normal", "Wet", "2025"))) %>%
  filter(Month %in% c(6:10))

#plot for report
ggplot(BDLdaily, aes(x = DOY, y = Salinity, group = as.factor(Year), color = YT, linewidth = as.factor(Year))) + 
  geom_line()+
  theme_bw()+
  ylab("Salinity at Belden's Landing (PSU)")+
  xlab("Day of Year")+
  scale_linewidth_manual(values = c(rep(.7, 8), 1.4), guide = NULL)+
  scale_color_manual(values = c("orangered", "orange", "gold3","seagreen", "blue", "black"), name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  geom_hline(yintercept = 6, linetype =2, color = "green")+
  theme(legend.position = "bottom")

ggsave("plots/BDLsalinity2025.tiff", device = "tiff", width =6.5, height =4.5)


#now longer dataset with all the year types
BDLdaily2 = filter(BDL, Value >1, Value < 30000) %>%
  mutate(Date = date(DateTime), Year = year(DateTime), Month = month(DateTime)) %>%
  group_by(Date, Year, Month) %>%
  summarize(EC = mean(Value, na.rm = T), Salinity = ec2pss(EC/1000, 25)) %>%
  left_join(wytype) %>%
  mutate(DOY = yday(Date), YT = case_when(Year == 2025 ~ "AN",
                                          TRUE ~ YT),
         YT = factor(YT, levels = c("C", "D", "BN", "AN","W"), 
                     labels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))) %>%
  filter(Month %in% c(6:10))


ggplot(BDLdaily2, aes(x = DOY, y = Salinity, group = as.factor(Year), color = YT)) + 
  geom_line()+
  theme_bw()+
  ylab("Salinity at Belden's Landing (PSU)")+
  xlab("Day of Year")+
  scale_color_manual(values = c("orangered", "orange", "gold3", "cyan", "blue", "black"), name = "Year Type")+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))+
  geom_hline(yintercept = 4, linetype =2, color = "black")+
  theme(legend.position = "bottom")

################################################
DFw20232 = bind_rows(DF, Outflow2025) %>%
  mutate(Month = month(Date), DOY = yday(Date)) %>%
  # filter(Month %in% c(6:10), Year %in% c(2017:2023)) %>%
  select(OUT, X2, CVP, SWP, Date, Month, DOY, Year, YT) %>%
  mutate(YT = case_when(Year == 2025 ~ "2025",
                        TRUE ~ YT)) %>%
  mutate(YT = factor(YT, levels = c("C", "D", "BN", "AN", "W", "2025"), 
                     labels = c("Critical", "Dry", "Below Normal", "Above Normal" , "Wet", "2025")))


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
  mutate(Year = year(Date), Month = month(Date),
         WY = case_when(Month  %in% c(10:12)~Year+1,
                        TRUE ~Year)) %>%
  left_join(yrs)

FallX2 = filter(X2all, Month %in% c(6:10)) %>%
  group_by(WY, `Yr-type`) %>%
  summarize(FallX2 = mean(X2)) %>%
  mutate(YT = factor(`Yr-type`, levels = c("C", "D", "BN","AN", "W")))

ggplot(FallX2, aes(x = WY, y = FallX2, fill = YT))+ geom_col()+
  geom_hline(yintercept = 80)+
  geom_hline(yintercept = 74, linetype = 2)+
  theme_bw()+
  ylab("Mean Jun-Oct X2, km")+
  scale_fill_manual(values = c("darkred", "orange", "yellow", "lightgreen", "darkblue"))
