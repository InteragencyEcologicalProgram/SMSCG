#plot different habitat scenarios

library(tidyverse)
library(lubridate)

##############################################################################
#new data July2023

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
habarea = mutate(habarea, DOY = yday(time)) %>%
  filter(DOY < 305, DOY > 181)

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

bysum = filter(habsum, region %in% c("NE Suisun", "NW Suisun", "SE Suisun", "SW Suisun")) %>%
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
                              scenario == "SixtyDayPerecnt" ~ "60d",
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

HabitatPercent = bind_rows(marsh, bay) %>%
  mutate(scenario = factor(scenario, levels = c("base", "7_7", "60d", "60d_100TAF", "100TAFjul1", "100TAFaug15"),
                           labels = c("base", "60 days, 7 days-on/ 7 days-off",
                                      "60 days continuous", "60 days continuous, followed by 100 TAF",
                                      "100 TAF continuous starting July1",
                                      "100 TAF continuous starting Aug 15")),
         YT = factor(YT, levels = c("Dry", "Below Normal", "Above Normal", "Wet"),
                     labels = c("Dry", "Below\nNormal", "Above\nNormal", "Wet")))

ggplot(HabitatPercent, aes(fill = scenario, y = Percent, x = YT))+ 
  geom_col(position = "dodge", color = "black")+
  facet_wrap(~Region) + ylab("Percent increase in LSZ") + xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+ theme(legend.position = c(.2, .8), legend.direction = "vertical", 
                    legend.background= element_rect(fill = "white", color = "black"),
                    axis.text = element_text(size =12),
                    axis.title = element_text(size =12),
                    strip.text = element_text(size =12))

ggsave("plots/Habatatpercent100TAF_nogriz.png", device = "png", width =8, height =6)

ggplot(HabitatPercent, aes(fill = scenario, y = Acres, x = YT))+ 
  geom_col(position = "dodge", color = "black")+
  facet_wrap(~Region, scales = "free_y") + ylab("LSZ area (acres)")+
  xlab("Water Year Type")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw() + theme(legend.position = c(.18, .8), legend.direction = "vertical", 
                      legend.background= element_rect(fill = "white", color = "black"),
                      axis.text = element_text(size =12),
                      axis.title = element_text(size =12),
                      strip.text = element_text(size =12))


ggsave("plots/Habatatacres100TAF_nogriz.png", device = "png", width =9, height =6)


write.csv(HabitatPercent, "outputs/Habitatpercent100TAF_nogriz.csv", row.names = FALSE)

habitatwide = pivot_wider(HabitatPercent, id_cols = c(Region, Year, YT), names_from = scenario, values_from = Acres)

write.csv(habitatwide, "Data/habitatwide_nogriz.csv", row.names = FALSE)

#make a nice version of the area by date plot

habarea2 = mutate(habarea, Zone = case_when(region %in% c("NE Suisun", "NW Suisun",
                                                         "SE Suisun", "SW Suisun") ~ "Suisun Bay",
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

ggsave("plots/HabatatTimeSeries100TAF_nogriz.png", device = "png", width =8, height =6)


##############################################################################
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

habarea23a = mutate(alldat, DOY = yday(time)) %>%
  filter(DOY < 305, DOY > 181,
         scenario %in% c("base", "60d", "100TAFaug15", "2023operation")) %>%
  mutate(scenario = factor(scenario, levels = c("base", "60d", "100TAFaug15", "2023operation")))

ggplot(habarea23a, aes(x = DOY, y = acres, color = Year, linetype = scenario)) +
  geom_line(linewidth =1)+
  facet_wrap(~region, scales = "free") + ylab("LSZ area (acres)")+
  theme_bw()


#calculate average habitat area by year

habsum23 = group_by(habarea23a, YT, Year, scenario, region) %>%
  summarize(LSZ = mean(acres))


#average percent increase - suisun marsh
marshsum = filter(habsum23, region == "Suisun Marsh") %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate( SixtyDayPercent = (`60d`-base)/base*100,
          percent23 = (`2023operation`-base)/base*100,
         TAFaug = (`100TAFaug15`-base)/base*100)

marshsumlong = pivot_longer(select(marshsum, YT, Year, region, percent23,
                                   SixtyDayPercent, TAFaug), 
                            cols = c(percent23,
                                     SixtyDayPercent,  TAFaug),
                            names_to = "scenario", values_to = "Percent") %>%
  filter(!is.na(Percent)) %>%
  mutate(scenario = case_when(scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "percent23" ~ "2023operation",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              TRUE ~ scenario))

marshsumlong2 = pivot_longer(select(marshsum, YT, Year, region, base, `60d`, `2023operation`,
                                    `100TAFaug15`), 
                             cols = c(base,`60d`, `2023operation`,
                                      `100TAFaug15`),
                             names_to = "scenario", values_to = "acres") %>%
  filter(!is.na(acres))


marsh = left_join(marshsumlong2, marshsumlong) %>%
  mutate(Region = "Suisun Marsh",
         Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))

#now for suisun bay

bysum = filter(habsum23, region == c("Suisun Bay")) %>%
  pivot_wider(names_from = scenario, values_from = LSZ) %>%
  mutate(SixtyDayPercent = (`60d`-base)/base*100,
         percent23 = (`2023operation`-base)/base*100,
         TAFaug = (`100TAFaug15`-base)/base*100)

baysumlong = pivot_longer(select(bysum, YT, Year,
                                 SixtyDayPercent,TAFaug, `percent23`), 
                          cols = c( SixtyDayPercent, TAFaug, `percent23`),
                          names_to = "scenario", values_to = "Percent") %>%
  mutate(Percent = case_when(is.nan(Percent)~0,
                             is.infinite(Percent)~0,
                             TRUE ~ Percent)) %>%
  filter(!is.na(Percent))%>%
  mutate(scenario = case_when(scenario == "SixtyDayPercent" ~ "60d",
                              scenario == "percent23" ~ "2023operation",
                              scenario == "TAFaug" ~ "100TAFaug15",
                              TRUE ~ scenario))


baysumlong2 = pivot_longer(select(bysum, YT, Year, base,  `60d`,
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

bay = left_join(baysumlong2, baysumlong) %>%
  mutate(Region = "Suisun Bay",
         Percent = case_when(is.na(Percent)~ 0,
                             TRUE ~ Percent))

HabitatPercent = bind_rows(marsh, bay) %>%
  mutate(scenario = factor(scenario, levels = c("base","60d", "100TAFaug15", "2023operation"),
                           labels = c("base",
                                      "60 days continuous", 
                                      "100 TAF continuous starting Aug 15", "2023 gate operation")),
         YT = factor(YT, levels = c("Dry", "Below Normal", "Above Normal", "Wet"),
                     labels = c("Dry", "Below\nNormal", "Above\nNormal", "Wet")))


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
ggplot(filter(habarea2, Year == 2010), aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)

#below normal, 2016
ggplot(filter(habarea2, Year == 2016, scenario != "60 days continuous, followed by 100 TAF"), aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)

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
  xlab(NULL)

#below normal, 2016
ggplot(filter(habarea2, Year == 2016), aes(x = time, y = LSZ*0.000247105, color = scenario))+ geom_line(linewidth = 0.75)+
  facet_grid(Zone~YT, scales = "free")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  ylab("Low Salinity Zone Area (acres)")+
  xlab(NULL)
