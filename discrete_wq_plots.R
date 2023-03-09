#discrete water quality

library(readxl)
library(tidyverse)
library(lubridate)

ICFwq = read_excel("Data/ICF_WQ_2022-11-08.xlsx", na = "NA")

ICFwq2022 = mutate(ICFwq, Year = year(date), Month = month(date)) %>%
  filter(Year == 2022, Month %in% c(6, 7,8,9, 10)) %>%
  mutate(Month = factor(Month, levels = c(6, 7,8,9,10), labels = c("Jun", "Jul", "Aug", "Sep", "Oct")),
         region = factor(region, levels = c("Suisun Marsh", "Suisun Bay","Lower Sacramento", "Lower San Joaquin",
                                            "Upper Sacramento River", "Cache Slough and Liberty Island",
                                             "Sacramento Deep Water Ship Channel",
                                             "Western"),
                         labels = c("Suisun Marsh", "Suisun Bay", "Lower Sac", "Lower SJ", 
                                    "Upper Sac", "Cache/Liberty", "SDWSC", "Western")),
         Source = "DOP") %>%
  rename(Chlorophyll = chla_fluor, Station = site_id)

ggplot(ICFwq2022, aes(x = region, y = no3)) + geom_boxplot()

ggplot(ICFwq2022, aes(x = region, y = nh4)) + geom_boxplot()

ggplot(ICFwq2022, aes(x = region, y = po4)) + geom_boxplot()

#read in EMP data
EMP = read_excel("Data/Jun-Aug 2022 Discrete EMP WQ Data.xlsx", sheet = "fielddata") %>%
  rename(Station = `Station Name`, date = `Sample Date`, turbidity = `Turbidity F.N.U. EPA 180.1 (Field) - [1]*`,
         secchi_depth = `Secchi Depth Centimeters Secchi Depth - [1]*`, 
         temperature = `Water Temperature °C EPA 170.1 (Field) - [1]*`,
         specific_conductivity = `Specific Conductance uS/cm@25 °C EPA 120.1 (Field) - [1]*`) %>%
  mutate(turbidity = as.numeric(turbidity), temperature = as.numeric(temperature), 
         specific_conductivity = as.numeric(specific_conductivity), secchi_depth = as.numeric(secchi_depth)) %>%
select(Station, date, turbidity, secchi_depth, temperature, specific_conductivity)

#EMP station locations
EMPstas = read_excel("Data/EMPstations.xlsx")

#lab data (we just want chlorophyll)
EMPlab = read_excel("Data/Jun-Aug 2022 Discrete EMP WQ Data.xlsx", sheet = "labdata") %>%
  rename(Station = `Station Name`, date = `Sample Date`, 
         Chlorophyll = `Chlorophyll a ug/L Std Method 10200 H 274 [1]*`) %>%
  mutate(Chlorophyll = as.numeric(Chlorophyll)) %>%
  select(Station, date, Chlorophyll)

EMP2 = left_join(EMP, EMPlab) %>%
  left_join(EMPstas) %>%
  mutate(date = mdy_hm(date), Year = year(date), Month = month(date),
         Month = factor(Month, levels = c(6, 7,8,9,10), 
                        labels = c("Jun", "Jul", "Aug", "Sep", "Oct")),
         Source = "EMP")

#Summer Townet
STN = read_csv("Data/MCvisual_CatchPerStation_STN.csv")
STN2 = mutate(STN, date = mdy(SampDate), Month = month(date), Source = "STN",
              Latitude = StartLatDegrees + StartLatMinutes/60+StartLatSeconds/3600,
              Longitude = -1*StartLongDegrees + StartLongMinutes/60+StartLongSeconds/3600) %>%
  rename(secchi_depth = Secchi, temperature = TemperatureTop, turbidity = TurbidityTop,
         specific_conductivity = ConductivityTop) %>%
  filter(!is.na(Latitude), Latitude !=0) %>%
  select(Latitude, Longitude, secchi_depth, temperature, turbidity, 
         StationCode, Source, Month, date, Year, specific_conductivity)

#Group by region
library(deltamapr)
library(sf)
regions = R_EDSM_Strata_19P3 %>%
  mutate(region = factor(Stratum, levels = c("Suisun Marsh", "Suisun Bay","Lower Sacramento River", "Lower Joaquin River",
                                                      "Upper Sacramento River", "Cache Slough/Liberty Island",
                                                      "Sac Deep Water Shipping Channel",
                                                      "Western Delta"),
                         labels = c("Suisun Marsh", "Suisun Bay", "Lower Sac", "Lower SJ", 
                                    "Upper Sac", "Cache/Liberty", "SDWSC", "Western"))) %>%
  st_transform(crs = 4326)

STN3 = st_as_sf(STN2, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(regions) %>%
  st_drop_geometry() %>%
  select(Source, temperature, secchi_depth, specific_conductivity, turbidity, StationCode,
         date, Year, Month, region) %>%
  filter(!is.na(region)) %>%
  mutate(StationCode = as.character(StationCode), 
         Month = factor(Month, levels = c(6, 7,8,9,10), labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))

#combine EMP and ICF and STN
Discall = bind_rows(EMP2, ICFwq2022, STN3) %>%
  filter(!is.na(region), region != "Lower SJ") %>%
  mutate(region = factor(region, levels =c("Suisun Marsh", "Suisun Bay", "Lower Sac", "Lower SJ", 
                                           "Upper Sac", "Cache/Liberty", "SDWSC", "Western")))

#plot chlotophyll
ggplot(Discall, aes(x = region, y = Chlorophyll, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Chlorophyll-a (ug/L)")+ xlab(NULL)

  Discall %>%
    group_by(region) %>%
    summarize(MaxCHL = max(Chlorophyll, na.rm = t), MeanCHL = mean(Chlorophyll, na.rm = T))

ggplot(Discall, aes(x = region, y = turbidity, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Turbidity (NTU)")+ xlab(NULL)


Discall%>%
  group_by(region) %>%
  summarize(Maxturb = max(turbidity, na.rm = t), MeanCHL = mean(turbidity, na.rm = T))


ggplot(Discall,aes(y = secchi_depth, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Secchi cm")+ xlab(NULL)




ggplot(Discall, aes(x = region, y = temperature, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  geom_hline(yintercept = 23.9, color = "red", linetype = 2)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Temperature")+ xlab(NULL)


ggplot(Discall, aes(x = region, y = salinity, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  geom_hline(yintercept = 6, color = "red", linetype = 2)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Salinity")+ xlab(NULL)


ggplot(ICFwq2022, aes(x = region, y = specific_conductivity, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  geom_hline(yintercept = 6, color = "red", linetype = 2)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Specific Conductivity")+ xlab(NULL)


ggplot(ICFwq2022, aes(x = region, y = ph, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  geom_hline(yintercept = 6, color = "red", linetype = 2)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Salinity")+ xlab(NULL)

#calculate days above 29.3 C

Temps = Discall %>%
  mutate(Date2 = date(date)) %>%
  group_by(region, Date2) %>%
  summarize(Temp = max(temperature)) %>%
  group_by(region) %>%
  summarize(n = n(), stress = length(Temp[which(Temp >23.9)]))

###############################################################
#compare grizzly bay bouys


