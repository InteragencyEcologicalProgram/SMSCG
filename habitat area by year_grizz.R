#plot different habitat scenarios

library(tidyverse)
library(lubridate)
##############################################################################
#new data July2023 ###########

ANbase = read_csv("Data/habitat_csv/2010_base/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Above Normal", Year = "2010", scenario = "base")

AN77 = read_csv("Data/habitat_csv/2010_7_7/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Above Normal", Year = "2010", scenario = "7_7")

AN60d = read_csv("Data/habitat_csv/2010_60d/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Above Normal", Year = "2010", scenario = "60d")

AN60d_100TAF = read_csv("Data/habitat_csv/2010_60d_100TAF/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Above Normal", Year = "2010", scenario = "60d_100TAF")

BNbase = read_csv("Data/habitat_csv/2016_base/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Below Normal", Year = "2016", scenario = "base")

BN77 = read_csv("Data/habitat_csv/2016_7_7/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Below Normal", Year = "2016", scenario = "7_7")

BN60d = read_csv("Data/habitat_csv/2016_60d/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Below Normal", Year = "2016", scenario = "60d")

BN60d_100TAF = read_csv("Data/habitat_csv/2016_60d_100TAF/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Below Normal", Year = "2016", scenario = "60d_100TAF")


Wbase = read_csv("Data/habitat_csv/2017_base/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Wet", Year = "2017", scenario = "base")

Wjul1 = read_csv("Data/habitat_csv/2017_100taf_jul1/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Wet", Year = "2017", scenario = "100TAFjul1")

Waug15 = read_csv("Data/habitat_csv/2017_100taf_aug15/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Wet", Year = "2017", scenario = "100TAFaug15")


Dbase = read_csv("Data/habitat_csv/2020_base/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Dry", Year = "2020", scenario = "base")

D77 = read_csv("Data/habitat_csv/2020_7_7/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Dry", Year = "2020", scenario = "7_7")

D60d = read_csv("Data/habitat_csv/2020_60d/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Dry", Year = "2020", scenario = "60d")

D60d_100TAF = read_csv("Data/habitat_csv/2020_60d_100TAF/lsz_area_less_than_6.0.csv") %>%
  mutate(YT = "Dry", Year = "2020", scenario = "60d_100TAF")

#summarize and plot

habitatarea = bind_rows(D60d, D60d_100TAF, D77, Dbase, Waug15, Wjul1, Wbase,
                        BNbase, BN77, BN60d, BN60d_100TAF, ANbase, AN77, AN60d, AN60d_100TAF)
ggplot(habitatarea, aes(x = time, y = lsz_area, color = scenario))+
  geom_line()+
  facet_wrap(YT~region, scales = "free_x")

#we're only really interested in Suisun 

habarea = filter(habitatarea, region %in% c("Suisun Marsh", "NE Suisun", "NW Suisun",
                                            "SE Suisun", "SW Suisun", "Confluence"))

ggplot(habarea, aes(x = time, y = lsz_area, color = scenario))+
  geom_line()+
  facet_wrap(YT~region, scales = "free_x")

#Cut off on November 1st. Some of the scenarios started in July instead of June (arg)
#so also cut of june so its standardized
habarea = mutate(habarea, DOY = yday(time), Month = month(time)) %>%
  filter(Month %in% c(7:10))

#calculate average habitat area by day

habsum = group_by(habarea, YT, Year, scenario, region) %>%
  summarize(LSZ = mean(lsz_area)) 

#average percent increase - suisun marsh
marshsum = filter(habsum, region == "Suisun Marsh") %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate(SevenSevenPercent = (`7_7`-base)/(base)*100, SixtyDayPercent = (`60d`-base)/base*100,
         SixtyTAF = (`60d_100TAF`-base)/base*100, TAFaug = (`100TAFaug15`-base)/base*100,
TAFjul = (`100TAFjul1`-base)/base*100)

marshsumlong = pivot_longer(select(marshsum, YT, Year, region, SevenSevenPercent,
                                   SixtyDayPercent, SixtyTAF, TAFaug, TAFjul), 
                            cols = c(SevenSevenPercent,
                                     SixtyDayPercent, SixtyTAF, TAFaug, TAFjul),
                            names_to = "scenario", values_to = "Percent") %>%
  filter(!is.na(Percent)) %>%
  mutate(scenario = case_when(scenario == "SixtyDayPercent" ~ "60d",
    scenario == "SixtyTAF" ~ "60d_100TAF",
                              scenario == "SevenSevenPercent" ~ "7_7",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              scenario == "TAFjul" ~ "100TAFjul1",
                              TRUE ~ scenario))

marshsumlong2 = pivot_longer(select(marshsum, YT, Year, region, base, `60d`, `60d_100TAF`, `7_7`,
                                     `100TAFaug15`,  `100TAFjul1`), 
                            cols = c(base,`60d`, `60d_100TAF`, `7_7`,
                                     `100TAFaug15`,  `100TAFjul1`),
                            names_to = "scenario", values_to = "MetersSquared") %>%
  filter(!is.na(MetersSquared))%>%
  mutate(Acres = MetersSquared*0.000247105)


marsh = left_join(marshsumlong2, marshsumlong) %>%
  mutate(Region = "Suisun Marsh",
         Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))

#now for suisun bay

bysum = filter(habsum, region %in% c("NE Suisun",  "SE Suisun", "SW Suisun")) %>%
  group_by(scenario, YT, Year) %>%
  summarize(LSZ = sum(LSZ)) %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate(SevenSevenPercent = (`7_7`-base)/(base)*100, SixtyDayPercent = (`60d`-base)/base*100,
         SixtyTAF = (`60d_100TAF`-base)/base*100, TAFaug = (`100TAFaug15`-base)/base*100,
         TAFjul = (`100TAFjul1`-base)/base*100)

baysumlong = pivot_longer(select(bysum, YT, Year, SevenSevenPercent,
                                   SixtyDayPercent, SixtyTAF, TAFaug, TAFjul), 
                            cols = c(SevenSevenPercent,
                                     SixtyDayPercent, SixtyTAF, TAFaug, TAFjul),
                            names_to = "scenario", values_to = "Percent") %>%
  mutate(Percent = case_when(is.nan(Percent)~0,
                             is.infinite(Percent)~0,
                             TRUE ~ Percent)) %>%
  filter(!is.na(Percent))%>%
  mutate(scenario = case_when(scenario == "SixtyTAF" ~ "60d_100TAF",
                              scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "SevenSevenPercent" ~ "7_7",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              scenario == "TAFjul" ~ "100TAFjul1",
                              TRUE ~ scenario))


baysumlong2 = pivot_longer(select(bysum, YT, Year, base, `60d_100TAF`, `7_7`, `60d`,
                                    `100TAFaug15`,  `100TAFjul1`), 
                             cols = c(base, `60d`, `60d_100TAF`, `7_7`,
                                      `100TAFaug15`,  `100TAFjul1`),
                             names_to = "scenario", values_to = "MetersSquared") %>%
  filter(!is.na(MetersSquared)) %>%
  mutate(Acres = MetersSquared*0.000247105)

bay = left_join(baysumlong2, baysumlong) %>%
  mutate(Region = "Suisun Bay",
         Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))

##########Grizzly bay 2023##############################################################################

grizzly = filter(habsum, region %in% c("NW Suisun")) %>%
  group_by(scenario, YT, Year) %>%
  summarize(LSZ = sum(LSZ)) %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate(SevenSevenPercent = (`7_7`-base)/(base)*100, SixtyDayPercent = (`60d`-base)/base*100,
         SixtyTAF = (`60d_100TAF`-base)/base*100, TAFaug = (`100TAFaug15`-base)/base*100,
         TAFjul = (`100TAFjul1`-base)/base*100)

grizzlong = pivot_longer(select(grizzly, YT, Year, SevenSevenPercent,
                                 SixtyDayPercent, SixtyTAF, TAFaug, TAFjul), 
                          cols = c(SevenSevenPercent,
                                   SixtyDayPercent, SixtyTAF, TAFaug, TAFjul),
                          names_to = "scenario", values_to = "Percent") %>%
  mutate(Percent = case_when(is.nan(Percent)~0,
                             is.infinite(Percent)~0,
                             TRUE ~ Percent)) %>%
  filter(!is.na(Percent))%>%
  mutate(scenario = case_when(scenario == "SixtyTAF" ~ "60d_100TAF",
                              scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "SevenSevenPercent" ~ "7_7",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              scenario == "TAFjul" ~ "100TAFjul1",
                              TRUE ~ scenario))


grizzlong2 = pivot_longer(select(grizzly, YT, Year, base, `60d_100TAF`, `7_7`, `60d`,
                                  `100TAFaug15`,  `100TAFjul1`), 
                           cols = c(base, `60d`, `60d_100TAF`, `7_7`,
                                    `100TAFaug15`,  `100TAFjul1`),
                           names_to = "scenario", values_to = "MetersSquared") %>%
  filter(!is.na(MetersSquared)) %>%
  mutate(Acres = MetersSquared*0.000247105)

grizz = left_join(grizzlong2, grizzlong) %>%
  mutate(Region = "Grizzly Bay",
         Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))
##################################################################################

#maximu area by region

foo = group_by(AN60d, region) %>% summarize(area = max(subarea)) %>%
  mutate(Acres = area*0.000247105)


#################################################################################

HabitatPercent = bind_rows(marsh, bay, grizz) %>%
  mutate(scenario = factor(scenario, levels = c("base", "7_7", "60d", "60d_100TAF", "100TAFjul1", "100TAFaug15"),
                           labels = c("base", "60 days, 7 days-on/ 7 days-off",
                                      "60 days continuous", "60 days continuous, followed by 100 TAF",
                                      "100 TAF continuous starting July1",
                                      "100 TAF continuous starting Aug 15")),
         YT = factor(YT, levels = c("Dry", "Below Normal", "Above Normal", "Wet"),
                     labels = c("Dry", "Below\nNormal", "Above\nNormal", "Wet")))

ggplot(filter(HabitatPercent, scenario != "100 TAF continuous starting July1"),
       aes(fill = scenario, y = Percent, x = YT))+ 
  geom_col(position =  position_dodge2(width = 0.8, preserve = "single"), color = "black")+
  facet_wrap(~Region) + ylab("Percent increase in LSZ") + xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+ theme(legend.position = c(.5, .8), legend.direction = "vertical", 
                    legend.background= element_rect(fill = "white", color = "black"),
                    axis.text = element_text(size =12),
                    axis.title = element_text(size =12),
                    strip.text = element_text(size =12))

#ggsave("Plots/Habatatpercent100TAF.png", device = "png", width =8, height =6)

ggplot(filter(HabitatPercent,scenario != "100 TAF continuous starting July1"),
       aes(fill = scenario, y = Acres, x = YT))+ 
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"), color = "black")+
  facet_wrap(~Region, scales = "free_y") + ylab("LSZ area (acres)")+
  xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw() + theme(legend.position ="bottom", legend.direction = "vertical", 
                      legend.background= element_rect(fill = "white", color = "black"),
                      axis.text = element_text(size =12),
                      axis.title = element_text(size =12),
                      strip.text = element_text(size =12))


#ggsave("Plots/Habatatacres100TAF.png", device = "png", width =9, height =8)


#write.csv(HabitatPercent, "outputs/Habitatpercent100TAF_grizz.csv", row.names = FALSE)

habitatwide = pivot_wider(HabitatPercent, id_cols = c(Region, Year, YT), names_from = scenario, values_from = Acres)

#write.csv(habitatwide, "Data/habitatwide.csv", row.names = FALSE)

#make a nice version of the area by date plot ###############################

habarea2 = mutate(habarea, Zone = case_when(region %in% c("NE Suisun", 
                                                         "SE Suisun", "SW Suisun") ~ "Suisun Bay",
                                            region ==  "NW Suisun" ~ "Grizzly Bay",
                                           region == "Suisun Marsh" ~ "Suisun Marsh")) %>%
  filter(!is.na(Zone)) %>%
  group_by(Zone, scenario, time, YT, Year, DOY) %>%
  summarize(LSZ = sum(lsz_area)) %>%
  mutate(scenario = factor(scenario, levels = c("base", "7_7", "60d", "60d_100TAF", "100TAFjul1", "100TAFaug15"),
                           labels = c("base", "60 days, 7 days-on/ 7 days-off",
                                      "60 days continuous", "60 days continuous, followed by 100 TAF",
                                      "100 TAF continuous starting July1",
                                      "100 TAF continuous starting Aug 15")),
         YT = factor(YT, levels = c("Dry", "Below Normal", "Above Normal", "Wet"),
                     labels = c("Dry", "Below\nNormal", "Above\nNormal", "Wet")))


ggplot(habarea2, aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)+
  theme(legend.position = "bottom", legend.direction = "vertical")

#ggsave("HabatatTimeSeries100TAF.png", device = "png", width =8, height =6)

########################################################
#plot for synthesis report (no 100TAF) ###########################################
ggplot(filter(habarea2, scenario != "60 days continuous, followed by 100 TAF"), aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)+
  theme(legend.position = "bottom", legend.direction = "vertical")


Jultest = filter(habarea2, YT =="Above\nNormal", Zone == "Suisun Marsh") %>%
  pivot_wider(names_from = scenario, values_from = LSZ)

######post hoc 2023########################################################################
#check out this year's data

noaction = read_csv("Data/lsz_area_less_than_6.0_no_operation.csv")

action = read_csv("Data/lsz_area_less_than_6.0_operation.csv")

action = mutate(action, scenario = "2023operation", acres = lsz_area*0.000247105)
noaction = mutate(noaction, scenario = "base", acres = lsz_area*0.000247105)

action2023 = bind_rows(action, noaction) %>%
  filter(region %in% c("Suisun Bay", "Suisun Marsh"))

ggplot(action2023, aes(x = time, y = acres, color = scenario)) + geom_line()+
  facet_wrap(~region, scales = "free_y")

#calculate difference between action and no action

action2023_2 = pivot_wider(action2023, id_cols = c(region, time), names_from = scenario, values_from = acres) %>%
  mutate(Diff = `2023operation`- `base`, percent = Diff/`base`*100)

ggplot(action2023_2, aes(x = time, y = percent)) + geom_line() +
  facet_wrap(~region)

ggplot(action2023_2, aes(x = time, y = Diff)) + geom_line() +
  facet_wrap(~region)+ ylab("Gain in habitat with the action (acres)")

#compare to modeled results

action2023 = mutate(action2023, Month = month(time), DOY = yday(time), Year = "2023", YT = "Wet")

alldat = bind_rows(action2023, habarea) %>%
  mutate(region = case_when(region %in% c("NW Suisun", "SE Suisun", "SW Suisun", "NE Suisun", "Suisun Bay")~ "Suisun Bay",
                            TRUE ~ region)) %>%
  filter(region %in% c("Suisun Bay", "Suisun Marsh")) %>%
  group_by(region, Month, Year, YT, time, scenario) %>%
  summarize(lsz_area = sum(lsz_area), acres = lsz_area*0.000247105)

############################################
#2024 data ######################
library(readxl)
area2024a = read_excel("Data/combined_lsz_area_less_than_6.0.xlsx", sheet = "Sheet1")
area2024b = read_excel("Data/combined_lsz_area_less_than_6.0.xlsx", sheet = "Sheet2")
area2024c = read_excel("Data/combined_lsz_area_less_than_6.0.xlsx", sheet = "Sheet3")
area2024d = read_excel("Data/combined_lsz_area_less_than_6.0.xlsx", sheet = "Sheet4")

area2024 = bind_rows(area2024a, area2024b, area2024c, area2024d) %>%
  pivot_longer(cols = c(-Date, -Scenario), names_to = "region", values_to = "lsz_area") %>%
  rename(time = Date, scenario = Scenario) %>%
  mutate(acres = lsz_area, Year ="2024", Month = month(time)) %>%
  filter(region %in% c("Suisun Bay", "Suisun Marsh"))

ggplot(filter(area2024, scenario != "Historical (SMSCG & X2)"), aes(x = date(time), y = lsz_area, color = scenario))+ 
  geom_rect(data = gatedates, aes(xmin = StartDate, xmax = EndDate, ymin =0, ymax = 15000, fill = Type),
            inherit.aes = F, alpha = 0.5)+
  geom_line(linewidth =1)+
  facet_wrap(~region, nrow =2)+
  coord_cartesian(xlim = c(ymd("2024-07-01"), ymd("2024-11-1")),
                  ylim = c(0, 15000))+
  scale_color_brewer(palette = "Set1", name = "Scenario", labels = c("No Action", "No SMSCG, X2", "SMSCG, No X2"))+
  scale_fill_manual(values = c("grey", "lightblue"), labels = c("SMSCG", "X2"), name = "Action")+
  ylab("Low Salinity Zone \nArea (Acres)")+
  xlab(NULL)+
  theme_bw()
  



alldat2 = bind_rows(alldat, filter(area2024, scenario == "Historical (SMSCG & X2)")) 

#######################################
#salinity at BDL versus habitat area


noaction23 = read_csv("Data/lsz_area_less_than_6.0_no_operation.csv") 
  
bdl23 = filter(bdlwq, year == 2023) %>%
  left_join( noaction23, by = c("Date"="time")) %>%
  filter(Analyte == "salinity") %>%
  filter(region.y %in% c("Suisun Bay", "Suisun Marsh"))


ggplot(filter(bdl23, !is.na(region.y)), aes(x = Value,  y = lsz_area, color = Date)) + geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(~region.y)+ xlab("salinity at BDL")+ ylab("LSZ area")

ggplot(filter(bdl23, !is.na(region.y)), aes(x = Date, y = Value)) + geom_point()


ggplot(filter(bdl23, !is.na(region.y)), aes(x = Date, y = lsz_area, color = Value)) + geom_point()+
  facet_wrap(~region.y)+ geom_hline(aes(yintercept = subarea))

BDL2024 = filter(bdlwq, year == 2024) %>%
  left_join( filter(area2024, scenario == "Historical (SMSCG & X2)"), by = c("Date"="time")) %>%
  filter(Analyte == "salinity")

ggplot(filter(BDL2024, !is.na(region.y)), aes(x = Value,  y = acres)) + geom_point(aes(color = Date))+ geom_smooth(method = "lm")+
  facet_wrap(~region.y)+ xlab("salinity at BDL")+ ylab("LSZ area")

ggplot(filter(BDL2024, region.y == "Suisun Marsh"), aes(x = Value,  y = acres)) + geom_point(aes(color = Date))+ geom_smooth(method = "lm")+
  facet_wrap(~region.y)+ xlab("salinity at BDL")+ ylab("LSZ area")

#######year to year comparison #################
#the 2024 data had a different grid than teh previous data, meaning more habitat acraage,
#so i'm going to put it in terms of percent of total. 
Maxacres = mutate(alldat2, grid = case_when(Year == 2024 & region == "Suisun Marsh" ~ "New",
                                               TRUE ~ "Old")) %>%
  group_by(region, grid) %>%
  summarize(MaxLSZ = max(acres))


habarea23a = mutate(alldat2, DOY = yday(time)) %>%
  filter(DOY < 305, DOY > 181,
         scenario %in% c("base", "2023operation", "Historical (SMSCG & X2)"),
         !(Year =="2023" & scenario == "base")) %>%
  mutate(scenario = factor(scenario, levels = c("base", "2023operation", "Historical (SMSCG & X2)")),
         grid = case_when(Year == 2024 & region == "Suisun Marsh" ~ "New",
                          TRUE ~ "Old")) %>%
  left_join(Maxacres) %>%
  mutate(percentarea = acres/MaxLSZ)

ggplot(habarea23a, aes(x = DOY, y = acres, color = Year)) +
  geom_line(linewidth =1)+
  facet_wrap(~region, scales = "free") + ylab("LSZ area (acres)")+
  theme_bw()


ggplot(habarea23a, aes(x = DOY, y = percentarea, color = Year)) +
  geom_line(linewidth =1)+
  facet_wrap(~region, scales = "free") + ylab("Percent of total area < 6 PSU")+
  theme_bw()+ xlab("Day of Year")+
  scale_x_continuous(breaks = c(182, 210, 240, 270, 300), labels = c("Jul", "Aug", "Sep", "Oct", "Nov"))+
  theme(legend.position = "bottom")


ggsave("plots/habitat_by_year.tiff", device = "tiff", width = 8, height =6)
########X2 versus SMSCG#######################################################################

gatedates = data.frame(StartDate = c(ymd("2024-07-01"), ymd("2024-09-06"), ymd("2024-09-01"), ymd("2024-10-28")),
                       EndDate = c(ymd("2024-08-29"), ymd("2024-09-30"), ymd("2024-09-30"), ymd("2024-11-04")),
                       Type = c("SMSCG", "SMSCG", "X2@80km", "SMSCG"),
                       xval = c(ymd("2024-07-15"), ymd("2024-09-06"), ymd("2024-09-01"), ymd("2024-10-20")),
                       ynudge = c(0,-1, 1, 1))


ggplot(area2024, aes(x = date(time), y = lsz_area, color = scenario))+ 
  facet_wrap(~region)+
  coord_cartesian(xlim = c(ymd("2024-07-01"), ymd("2024-11-01")),
                  ylim = c(0,15000))+
geom_rect(data = gatedates, aes(ymin = -Inf, ymax = Inf,xmin = StartDate, xmax = EndDate,
                                fill = Type), inherit.aes = FALSE, alpha = 0.4)+
  scale_fill_manual(values = c("grey60", "tan1"), name = "Action\nPeriod")+
  theme_bw()+geom_line(linewidth =1)

#how much do we get from fall X2 versus SMSCG operations?
habgain = area2024 %>%
  group_by(time, region, Year, Month) %>%
  summarize(X2gain = lsz_area[which(scenario == "No SMSCG, X2")]- lsz_area[which(scenario == "No SMSCG, No X2")],
           Gatesgain = lsz_area[which(scenario == "SMSCG, No X2")]- lsz_area[which(scenario == "No SMSCG, No X2")])

habgain2 = pivot_longer(habgain, cols = c(X2gain, Gatesgain), names_to = "Scenario", values_to = "Acres")

ggplot(habgain2) + geom_line(aes(x = time, y = Acres, color = Scenario), size =1)+
  facet_wrap(~region)+
  ylab("Increase in habitat acres")+
  coord_cartesian(xlim = c(ymd_hm("2024-07-01 00:00"), ymd_hm("2024-11-1 00:00")),
                  ylim = c(0, 10000))+
  scale_color_discrete(labels = c("Increase from \nSMSCG", "Increase from X2"))+
  xlab("Date - 2024")+
  theme_bw()


TotalGain = habgain %>%
  filter(Month %in% c(6:10)) %>%
  group_by(region) %>%
  summarize(X2gain = sum(X2gain), Gatesgain = sum(Gatesgain)) %>%
  pivot_longer(cols = c(X2gain, Gatesgain), names_to = "Scenario", values_to = "Acres")

#we had a 30-day X2 action, it would be more with 60 days
tot = TotalGain %>%
  group_by(Scenario) %>%
  summarize(Total = sum(Acres))

filter(tot, Scenario == "X2gain")$Total*2
#860687.1

#90ish day gates 
filter(tot, Scenario == "Gatesgain")$Total
 
#what is 20% of the habitat from fall X2?
860687*0.2
#172137.4

#so it would take 90 days of gates to equal 20% of the habitat from fall X2, and that will depend on salinity conditions

#how much did we get just from the 60 days in the sumer?
SummerGain = habgain %>%
  filter(Month %in% c(6:8)) %>%
  group_by(region) %>%
  summarize( Gatesgain = sum(Gatesgain))
sum(SummerGain$Gatesgain)
#only 89714 acres

#monthly gain
monthlyGain = habgain %>%
  filter(Month %in% c(6:10)) %>%
  group_by(region, Month) %>%
  summarize(X2gain = mean(X2gain), Gatesgain = mean(Gatesgain)) %>%
  pivot_longer(cols = c(X2gain, Gatesgain), names_to = "Scenario", values_to = "Acres")

ggplot(monthlyGain, aes(x = Month, y = Acres, fill = Scenario)) + geom_col(position = "dodge")+
  facet_wrap(~region)+ ylab("Increase in Acres")+
  scale_fill_brewer(palette = "Set1", labels = c("SMSCG, No X2", "No SMSCG, X2"))+
  theme_bw()

monthly2 = group_by(monthlyGain, Month, Scenario) %>%
  summarize(Acres = sum(Acres))
###################Month##############################################################################


#compare 2023 to other years

compar23 = filter(habarea23a, scenario %in% c("base", "2023operation", "Historical (SMSCG & X2)"), Year %in% c(2017, 2020, 2023, 2024)) %>%
  filter(!(Year == 2023 & scenario == "base"))


ggplot(compar23, aes(x = DOY, y = acres, color = Year)) +
  geom_line(linewidth =1)+
  facet_wrap(~region, scales = "free") + ylab("LSZ area (acres)")+
  theme_bw()+
  scale_x_continuous(breaks = c(182, 210, 240, 270, 300), labels = c("Jul", "Aug", "Sep", "Oct", "Nov"))


#calculate average habitat area by year

habsum23 = group_by(habarea23a, YT, Year, scenario, region) %>%
  summarize(LSZ = mean(acres)) %>%
  mutate(grid = case_when(Year == 2024 & region == "Suisun Marsh" ~ "New",
                          TRUE ~ "Old")) %>%
  left_join(Maxacres) %>%
  mutate(percentarea = LSZ/MaxLSZ)


#average percent increase - suisun marsh
marshsum = mutate(bind_rows(alldat, area2024), DOY = yday(time)) %>%
  filter(DOY < 305, DOY > 181) %>%
  group_by(Month, Year, scenario, region)  %>%
  summarize(LSZ = mean(acres, na.rm =T)) %>%
  mutate(grid = case_when(Year == 2024 & region == "Suisun Marsh" ~ "New",
                          TRUE ~ "Old"),
         scenario = case_when(scenario == "No SMSCG, No X2" ~ "base",
                              scenario == "2023operation" ~ "Historical (SMSCG & X2)",
                              TRUE ~ scenario)) %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate( SixtyDayPercent = (`60d`-base)/base*100,
          percent23 = (`Historical (SMSCG & X2)`-base)/base*100,
          
          percentX2 = (`No SMSCG, X2`-base)/base*100,
          percentGates = (`SMSCG, No X2`-base)/base*100,
         TAFaug = (`100TAFaug15`-base)/base*100,
         SixtyDayX = (`60d`-base),
         X23 = (`Historical (SMSCG & X2)`-base),
         
         XX2 = (`No SMSCG, X2`-base),
         XGates = (`SMSCG, No X2`-base),
         XTAFaug = (`100TAFaug15`-base),
         XXGates = (`No SMSCG, X2`-`Historical (SMSCG & X2)`))

marshsumlong = pivot_longer(select(marshsum,  Year, region, percent23, Month,
                                   SixtyDayPercent, TAFaug, percentX2, percentGates), 
                            cols = c(percent23,
                                     SixtyDayPercent,  TAFaug, percentX2, percentGates),
                            names_to = "scenario", values_to = "Percent") %>%
  filter(!is.na(Percent)) %>%
  mutate(scenario = case_when(scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "percent23" ~ "Historical (SMSCG & X2)",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              scenario == "percentX2" ~ "No SMSCG, X2",
                              scenario == "percentGates" ~ "SMSCG, No X2",
                              TRUE ~ scenario))

marshsumlongacres = pivot_longer(select(marshsum,  Year, region, X23, Month,
                                   SixtyDayX, XTAFaug, XX2, XGates), 
                            cols = c(X23,
                                     SixtyDayX, XTAFaug, XX2, XGates),
                            names_to = "scenario", values_to = "Acres") %>%
  filter(!is.na(Acres)) %>%
  mutate(scenario = case_when(scenario == "SixtyDayX" ~ "60d",
                              scenario == "X23" ~ "Historical (SMSCG & X2)",
                              scenario == "XTAFaug" ~ "100TAFaug15",
                              scenario == "XX2" ~ "No SMSCG, X2",
                              scenario == "XGates" ~"SMSCG, No X2",
                              TRUE ~ scenario))

marshsumlong2 = pivot_longer(select(marshsum, Year, region, base, `60d`, `Historical (SMSCG & X2)`,
                                    `100TAFaug15`, `SMSCG, No X2`, `No SMSCG, X2`), 
                             cols = c(base,`60d`, `Historical (SMSCG & X2)`,
                                      `100TAFaug15`, `SMSCG, No X2`, `No SMSCG, X2`),
                             names_to = "scenario", values_to = "acres") %>%
  filter(!is.na(acres))


marsh = left_join(marshsumlong2, marshsumlong) %>%
  mutate(Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))

marshacres = left_join(marshsumlong2, marshsumlongacres) %>%
  mutate(Acres = case_when(is.na(Acres)~ 0,
                             TRUE ~Acres))


ggplot(marshacres, aes(x = as.factor(Month), y = Acres, fill = as.factor(Year))) + geom_col(position = "dodge")+
  facet_wrap(scenario~region, scales = "free")

# 
# #now for suisun bay
# 
# bysum = filter(habsum23, region == c("Suisun Bay")) %>%
#   pivot_wider(names_from = scenario, values_from = LSZ) %>%
#   mutate(SixtyDayPercent = (`60d`-base)/base*100,
#          percent23 = (`2023operation`-base)/base*100,
#          TAFaug = (`100TAFaug15`-base)/base*100)
# 
# baysumlong = pivot_longer(select(bysum, YT, Year,
#                                  SixtyDayPercent,TAFaug, `percent23`), 
#                           cols = c( SixtyDayPercent, TAFaug, `percent23`),
#                           names_to = "scenario", values_to = "Percent") %>%
#   mutate(Percent = case_when(is.nan(Percent)~0,
#                              is.infinite(Percent)~0,
#                              TRUE ~ Percent)) %>%
#   filter(!is.na(Percent))%>%
#   mutate(scenario = case_when(scenario == "SixtyDayPercent" ~ "60d",
#                               scenario == "percent23" ~ "2023operation",
#                               scenario == "TAFaug" ~ "100TAFaug15",
#                               TRUE ~ scenario))
# 
# 
# baysumlong2 = pivot_longer(select(bysum, YT, Year, base,  `60d`,
#                                   `100TAFaug15`, `2023operation`), 
#                            cols = c(base, `60d`, 
#                                     `100TAFaug15`, `2023operation`),
#                            names_to = "scenario", values_to = "acres") %>%
#   filter(!is.na(acres)) %>%
#   mutate(scenario = case_when(
#                               scenario == "SixtyDayPerecnt" ~ "60d",
#                               
#                               scenario == "percent23" ~ "2023operation",
#                               scenario == "TAFaug" ~ "100TAFaug15",
#                               TRUE ~ scenario))
# 
# bay = left_join(baysumlong2, baysumlong) %>%
#   mutate(Region = "Suisun Bay",
#          Percent = case_when(is.na(Percent)~ 0,
#                              TRUE ~ Percent))
# 
# HabitatPercent = bind_rows(marsh, bay) %>%
#   mutate(scenario = factor(scenario, levels = c("base","60d", "100TAFaug15", "2023operation"),
#                            labels = c("base",
#                                       "60 days continuous", 
#                                       "100 TAF continuous starting Aug 15", "2023 gate operation")),
#          YT = factor(YT, levels = c("Dry", "Below Normal", "Above Normal", "Wet"),
#                      labels = c("Dry", "Below\nNormal", "Above\nNormal", "Wet")))
# 

ggplot(HabitatPercent, aes(fill = scenario, y = Percent, x = Year))+ 
  geom_col(position = "dodge", color = "black")+
  facet_wrap(~Region) + ylab("Percent increase in LSZ") + xlab("Water Year")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+ theme(legend.position = c(.2, .8), legend.direction = "vertical", 
                    legend.background= element_rect(fill = "white", color = "black"),
                    axis.text = element_text(size =12),
                    axis.title = element_text(size =12),
                    strip.text = element_text(size =12))



#############################################################################
#absolute acrage increase

marshsumx = filter(habsum23, region == "Suisun Marsh") %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate( SixtyDaydiff = (`60d`-base),
          diff23 = (`2023operation`-base),
          TAFaug = (`100TAFaug15`-base))

marshsumlongx = pivot_longer(select(marshsumx, YT, Year, region, diff23,
                                   SixtyDaydiff, TAFaug), 
                            cols = c( diff23,
                                      SixtyDaydiff, TAFaug),
                            names_to = "scenario", values_to = "Difference") %>%
  filter(!is.na(Difference)) %>%
  mutate(scenario = case_when(scenario == "SixtyDaydiff" ~ "60d",
                              scenario == "diff23" ~ "2023operation",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              TRUE ~ scenario))

marshsumlong2x = pivot_longer(select(marshsumx, YT, Year, region, base, `60d`, `2023operation`,
                                    `100TAFaug15`), 
                             cols = c(base,`60d`, `2023operation`,
                                      `100TAFaug15`),
                             names_to = "scenario", values_to = "acres") %>%
  filter(!is.na(acres))


marshx = left_join(marshsumlong2x, marshsumlongx) %>%
  mutate(Region = "Suisun Marsh",
         Percent = case_when(is.na(Difference)~ 0,
                             TRUE ~ Difference))

#now for suisun bay

bysumx = filter(habsum23, region == c("Suisun Bay")) %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate(SixtyDayPercent = (`60d`-base),
         percent23 = (`2023operation`-base),
         TAFaug = (`100TAFaug15`-base))

baysumlongx = pivot_longer(select(bysumx, YT, Year,
                                 SixtyDayPercent,TAFaug, `percent23`), 
                          cols = c( SixtyDayPercent, TAFaug, `percent23`),
                          names_to = "scenario", values_to = "Difference") %>%
  mutate(aces = case_when(is.nan(Difference)~0,
                             is.infinite(Difference)~0,
                             TRUE ~ Difference)) %>%
  filter(!is.na(Difference))%>%
  mutate(scenario = case_when(scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "percent23" ~ "2023operation",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              TRUE ~ scenario))


baysumlong2x = pivot_longer(select(bysumx, YT, Year, base,  `60d`,
                                  `100TAFaug15`, `2023operation`), 
                           cols = c(base, `60d`, 
                                    `100TAFaug15`, `2023operation`),
                           names_to = "scenario", values_to = "acres") %>%
  filter(!is.na(acres)) %>%
  mutate(scenario = case_when(
    scenario == "SixtyDayPerecnt" ~ "60d",
    
    scenario == "percent23" ~ "2023operation",
    scenario == "TAFaug" ~ "100TAFaug15",
    TRUE ~ scenario))

bayx = left_join(baysumlong2x, baysumlongx) %>%
  mutate(Region = "Suisun Bay",
         Percent = case_when(is.na(Difference)~ 0,
                             TRUE ~ Difference))

HabitatPercentx = bind_rows(marshx, bayx) %>%
  mutate(scenario = factor(scenario, levels = c("base","60d", "100TAFaug15", "2023operation"),
                           labels = c("base",
                                      "60 days continuous", 
                                      "100 TAF continuous starting Aug 15", "2023 gate operation")),
         YT = factor(YT, levels = c("Dry", "Below Normal", "Above Normal", "Wet"),
                     labels = c("Dry", "Below\nNormal", "Above\nNormal", "Wet")))

ggplot(HabitatPercentx, aes(fill = scenario, y = Difference, x = Year))+ 
  geom_col(position = "dodge", color = "black")+
  facet_wrap(~Region) + ylab("Mean Change in LSZ (acres)") + xlab("Water Year")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+ theme(legend.position = c(.2, .8), legend.direction = "vertical", 
                    legend.background= element_rect(fill = "white", color = "black"),
                    axis.text = element_text(size =12),
                    axis.title = element_text(size =12),
                    strip.text = element_text(size =12))

#what is the maximum area of all the things?
areas2 = group_by(ANbase, region) %>%
  summarize(area = max(subarea), acres = area*0.000247105)


#############################################################
#plot one year at a time for powerpoint

#above normal, 2010
ggplot(filter(habarea2, Year == 2010), aes(x = time, y = LSZ*0.000247105, color = scenario, linetype = scenario))+ 
  geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)+
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("plots/AN_LSZ_wgrizzly.tiff", device = "tiff", width = 10, height =6)

#new version for new action plan
habarea2b = filter(habarea2, Year ==2010,  scenario %in% c("base", "60 days continuous", "60 days continuous, followed by 100 TAF"))


ggplot(habarea2b, aes(x = time, y = LSZ*0.000247105, color = scenario, linetype = scenario))+ 
  geom_line(linewidth = 0.75)+
  facet_grid(Zone~., scales = "free")+
  theme_bw()+
  scale_linetype_manual(values = c(1,2,3), labels = c("base", "60 days continuous", "60 days continuous,\nfollowed by Sep/Oct operation"))+
  scale_color_brewer(palette = "Dark2", labels = c("base", "60 days continuous", "60 days continuous,\nfollowed by Sep/Oct operation"))+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)+
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("plots/AN_LSZ_wgrizzly.tiff", device = "tiff", width = 10, height =6)



#below normal, 2016
ggplot(filter(habarea2, Year == 2016, scenario != "60 days continuous, followed by 100 TAF"), aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)+
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("plots/BN_LSZ_wgrizzly2024.tiff", device = "tiff", width = 10, height =6)



#Dry, 2020
ggplot(filter(habarea2, Year == 2020, scenario != "60 days continuous, followed by 100 TAF"), aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)

#Dry, 2020
ggplot(filter(habarea2, Year == 2020), aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)+
  
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("plots/dry_LSZ_wgrizzly.tiff", device = "tiff", width = 10, height =6)


#below normal, 2016
ggplot(filter(habarea2, Year == 2016), aes(x = time, y = LSZ*0.000247105, color = scenario, linetype = scenario))+ 
  geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)+
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("plots/BN_LSZ_wgrizzly.tiff", device = "tiff", width = 10, height =6)



#wet 2017
ggplot(filter(habarea2, Year == 2017, scenario != "100 TAF continusous starting July1"), 
       aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)+
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("plots/Wet_LSZ_wgrizzly.tiff", device = "tiff", width = 10, height =6)

####################################################################################
#Now britt wants them by month and i'm a little grumpy about it ############


#calculate average habitat area by day
names(habarea)
habsumm = habarea %>%
  mutate(Month = month(time)) %>%
  group_by(YT, Year, scenario, region, Month) %>%
  summarize(LSZ = mean(lsz_area))

#average percent increase - suisun marsh
marshsumm = filter(habsumm, region == "Suisun Marsh") %>%
  pivot_wider(id_cols = c(Month, Year, region, YT), names_from = scenario, values_from = LSZ) %>%
  mutate(SevenSevenPercent = (`7_7`-base)/(base)*100, SixtyDayPercent = (`60d`-base)/base*100,
         SixtyTAF = (`60d_100TAF`-base)/base*100, TAFaug = (`100TAFaug15`-base)/base*100,
         TAFjul = (`100TAFjul1`-base)/base*100)

marshsumlongm = pivot_longer(select(marshsumm, Month, YT, Year, region, SevenSevenPercent,
                                   SixtyDayPercent, SixtyTAF, TAFaug, TAFjul), 
                            cols = c(SevenSevenPercent,
                                     SixtyDayPercent, SixtyTAF, TAFaug, TAFjul),
                            names_to = "scenario", values_to = "Percent") %>%
  filter(!is.na(Percent)) %>%
  mutate(scenario = case_when(scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "SixtyTAF" ~ "60d_100TAF",
                              scenario == "SevenSevenPercent" ~ "7_7",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              scenario == "TAFjul" ~ "100TAFjul1",
                              TRUE ~ scenario))

marshsumlong2m = pivot_longer(select(marshsumm, Month, YT, Year, region, base, `60d`, `60d_100TAF`, `7_7`,
                                    `100TAFaug15`,  `100TAFjul1`), 
                             cols = c(base,`60d`, `60d_100TAF`, `7_7`,
                                      `100TAFaug15`,  `100TAFjul1`),
                             names_to = "scenario", values_to = "MetersSquared") %>%
  filter(!is.na(MetersSquared))%>%
  mutate(Acres = MetersSquared*0.000247105)


marshm = left_join(marshsumlong2m, marshsumlongm) %>%
  mutate(Region = "Suisun Marsh",
         Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))

#now for suisun bay

bysumm = filter(habsumm, region %in% c("NE Suisun",  "SE Suisun", "SW Suisun")) %>%
  group_by(scenario, YT, Year, Month) %>%
  summarize(LSZ = sum(LSZ)) %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate(SevenSevenPercent = (`7_7`-base)/(base)*100, SixtyDayPercent = (`60d`-base)/base*100,
         SixtyTAF = (`60d_100TAF`-base)/base*100, TAFaug = (`100TAFaug15`-base)/base*100,
         TAFjul = (`100TAFjul1`-base)/base*100)

baysumlongm = pivot_longer(select(bysumm, Month, YT, Year, SevenSevenPercent,
                                 SixtyDayPercent, SixtyTAF, TAFaug, TAFjul), 
                          cols = c(SevenSevenPercent,
                                   SixtyDayPercent, SixtyTAF, TAFaug, TAFjul),
                          names_to = "scenario", values_to = "Percent") %>%
  mutate(Percent = case_when(is.nan(Percent)~0,
                             is.infinite(Percent)~0,
                             TRUE ~ Percent)) %>%
  filter(!is.na(Percent))%>%
  mutate(scenario = case_when(scenario == "SixtyTAF" ~ "60d_100TAF",
                              scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "SevenSevenPercent" ~ "7_7",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              scenario == "TAFjul" ~ "100TAFjul1",
                              TRUE ~ scenario))


baysumlong2m = pivot_longer(select(bysumm, Month,  YT, Year, base, `60d_100TAF`, `7_7`, `60d`,
                                  `100TAFaug15`,  `100TAFjul1`), 
                           cols = c(base, `60d`, `60d_100TAF`, `7_7`,
                                    `100TAFaug15`,  `100TAFjul1`),
                           names_to = "scenario", values_to = "MetersSquared") %>%
  filter(!is.na(MetersSquared)) %>%
  mutate(Acres = MetersSquared*0.000247105)

baym = left_join(baysumlong2m, baysumlongm) %>%
  mutate(Region = "Suisun Bay",
         Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))

########################################################################################

grizzlym = filter(habsumm, region %in% c("NW Suisun")) %>%
  group_by(scenario, YT, Year, Month) %>%
  summarize(LSZ = sum(LSZ)) %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate(SevenSevenPercent = (`7_7`-base)/(base)*100, SixtyDayPercent = (`60d`-base)/base*100,
         SixtyTAF = (`60d_100TAF`-base)/base*100, TAFaug = (`100TAFaug15`-base)/base*100,
         TAFjul = (`100TAFjul1`-base)/base*100)

grizzlongm = pivot_longer(select(grizzlym, Month, YT, Year, SevenSevenPercent,
                                SixtyDayPercent, SixtyTAF, TAFaug, TAFjul), 
                         cols = c(SevenSevenPercent,
                                  SixtyDayPercent, SixtyTAF, TAFaug, TAFjul),
                         names_to = "scenario", values_to = "Percent") %>%
  mutate(Percent = case_when(is.nan(Percent)~0,
                             is.infinite(Percent)~0,
                             TRUE ~ Percent)) %>%
  filter(!is.na(Percent))%>%
  mutate(scenario = case_when(scenario == "SixtyTAF" ~ "60d_100TAF",
                              scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "SevenSevenPercent" ~ "7_7",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              scenario == "TAFjul" ~ "100TAFjul1",
                              TRUE ~ scenario))


grizzlong2m = pivot_longer(select(grizzlym, Month, YT, Year, base, `60d_100TAF`, `7_7`, `60d`,
                                 `100TAFaug15`,  `100TAFjul1`), 
                          cols = c(base, `60d`, `60d_100TAF`, `7_7`,
                                   `100TAFaug15`,  `100TAFjul1`),
                          names_to = "scenario", values_to = "MetersSquared") %>%
  filter(!is.na(MetersSquared)) %>%
  mutate(Acres = MetersSquared*0.000247105)

grizzm = left_join(grizzlong2m, grizzlongm) %>%
  mutate(Region = "Grizzly Bay",
         Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))
#################################################################################

HabitatPercentm = bind_rows(marshm, baym, grizzm) %>%
  mutate(scenario = factor(scenario, levels = c("base", "7_7", "60d", "60d_100TAF", "100TAFjul1", "100TAFaug15"),
                           labels = c("base", "60 days, 7 days-on/ 7 days-off",
                                      "60 days continuous", "60 days continuous, followed by 100 TAF",
                                      "100 TAF continuous starting July1",
                                      "100 TAF continuous starting Aug 15")),
         YT = factor(YT, levels = c("Dry", "Below Normal", "Above Normal", "Wet"),
                     labels = c("Dry", "Below\nNormal", "Above\nNormal", "Wet")))

ggplot(filter(HabitatPercentm, scenario != "100 TAF continuous starting July1"),
       aes(fill = scenario, y = Percent, x = YT))+ 
  geom_col(position =  position_dodge2(width = 0.8, preserve = "single"), color = "black")+
  facet_grid(Month~Region) + ylab("Percent increase in LSZ") + xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+ theme(legend.position = c(.5, .8), legend.direction = "vertical", 
                    legend.background= element_rect(fill = "white", color = "black"),
                    axis.text = element_text(size =12),
                    axis.title = element_text(size =12),
                    strip.text = element_text(size =12))

ggsave("Plots/Habatatpercent100TAF.png", device = "png", width =8, height =6)

ggplot(filter(HabitatPercentm,scenario != "100 TAF continuous starting July1", Month !=6),
       aes(fill = scenario, y = Acres, x = YT))+ 
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"), color = "black")+
  facet_grid(Month~Region, scales = "free_y") + ylab("LSZ area (acres)")+
  xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw() + theme(legend.position ="bottom", legend.direction = "vertical", 
                     legend.background= element_rect(fill = "white", color = "black"),
                     axis.text = element_text(size =12),
                     axis.title = element_text(size =12),
                     strip.text = element_text(size =12))


ggsave("Plots/Habatatacres100TAF_bymonth.png", device = "png", width =10, height =10)


AN = filter(HabitatPercentm,Month !=6, YT == "Above\nNormal")
ggplot(AN,
       aes(fill = scenario, y = Acres, x = YT))+ 
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"), color = "black")+
  facet_grid(Month~Region, scales = "free_y") + ylab("LSZ area (acres)")+
  xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw() + theme(legend.position ="right", legend.direction = "vertical", 
                     legend.background= element_rect(fill = "white", color = "black"),
                     axis.text = element_text(size =12),
                     axis.title = element_text(size =12),
                     strip.text = element_text(size =12))
ggsave("Plots/Habatatacres100TAF_AN_bymonth.png", device = "png", width =10, height =8)


#Version for the new action plan

#copy the 100TAF scanario but change october so it's no action
ANx = filter(AN, scenario == "60 days continuous, followed by 100 TAF") %>%
  mutate(scenario = "60 days continuous, followed by September operation",
         Acres = case_when(Month ==10 & Region == "Suisun Marsh" ~ 3167,
                           Month ==10 & Region == "Suisun Bay" ~ 6073,
                           Month ==10 & Region == "Grizzly Bay" ~ 1309,
                           TRUE ~ Acres))
AN2 = bind_rows(AN, ANx) %>%
  filter(scenario %in% c("base", "60 days continuous", "60 days continuous, followed by September operation")) %>%
  mutate(scenario = factor(scenario, levels =c("base", "60 days continuous", "60 days continuous, followed by September operation"),
                             labels = c("base", "60 days continuous", "60 days continuous,\n followed by September operation")))

ggplot(AN2,
       aes(fill = scenario, y = Acres, x = Region))+ 
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"), color = "black")+
  facet_grid(Month~., scales = "free_y") + ylab("LSZ area (acres)")+
  xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw() + theme(legend.position ="right", legend.direction = "vertical", 
                     legend.background= element_rect(fill = "white", color = "black"),
                     axis.text = element_text(size =12),
                     axis.title = element_text(size =12),
                     strip.text = element_text(size =12))


BN = filter(HabitatPercentm,Month !=6, YT == "Below\nNormal")
ggplot(BN,
       aes(fill = scenario, y = Acres, x = YT))+ 
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"), color = "black")+
  facet_grid(Month~Region, scales = "free_y") + ylab("LSZ area (acres)")+
  xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw() + theme(legend.position ="right", legend.direction = "vertical", 
                     legend.background= element_rect(fill = "white", color = "black"),
                     axis.text = element_text(size =12),
                     axis.title = element_text(size =12),
                     strip.text = element_text(size =12))


ggsave("Plots/Habatatacres100TAF_BN_bymonth.png", device = "png", width =10, height =8)



write.csv(HabitatPercentm, "outputs/Habitatpercent100TAF_grizz_month.csv", row.names = FALSE)




ggplot(filter(HabitatPercentm, scenario != "100 TAF continuous starting July1"),
       aes(fill = scenario, y = Percent, x = YT))+ 
  geom_col(position =  position_dodge2(width = 0.8, preserve = "single"), color = "black")+
  facet_grid(Month~Region) + ylab("Percent increase in LSZ") + xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+ theme(legend.position = "bottom", legend.direction = "vertical", 
                    legend.background= element_rect(fill = "white", color = "black"),
                    axis.text = element_text(size =12),
                    axis.title = element_text(size =12),
                    strip.text = element_text(size =12))+
  coord_cartesian(ylim = c(0,900))

ggsave("Plots/Habatatpercent100TAF_bymonth.png", device = "png", width =10, height =10)

habmwide = pivot_wider(HabitatPercentm, id_cols = c(Month, Region), names_from = c(YT, scenario), values_from = Percent) %>%
  mutate(Metric = "Percent")

habmwide2 = pivot_wider(HabitatPercentm, id_cols = c(Month, Region), names_from = c(YT, scenario), values_from = Acres) %>%
  mutate(Metric = "area")

habm = bind_rows(habmwide, habmwide2)

write.csv(habm, "outputs/habitat_bymonth_forsdm.csv", row.names = FALSE)

########################################################

#I think actions started at 4psu, but not sure

ANwide = bind_rows(AN60d, ANbase) %>%
  filter(region == "Suisun Marsh") %>%
  pivot_wider(names_from = scenario, values_from = lsz_area) %>%
  mutate(diff = `60d`-base)

ggplot(ANwide, aes(x = time, y = diff))+ geom_line()
#nope, that one was July 1

BNwide = bind_rows(BN60d, BNbase) %>%
  filter(region == "Suisun Marsh") %>%
  pivot_wider(names_from = scenario, values_from = lsz_area)%>%
  mutate(diff = `60d`-base)


ggplot(BNwide, aes(x = time, y = diff))+ geom_line()
