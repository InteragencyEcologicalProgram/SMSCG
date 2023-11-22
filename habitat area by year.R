#plot different habitat scenarios

library(tidyverse)
library(lubridate)

noact2010 = read_csv("Data/habitat_output/HSI_2010/suisun_2010_base/hsib_area.csv") %>%
  mutate(scenario = "Base", YT = "Above Normal", Year = 2010)


#act2010 = read_csv("Data/habitat_output/HSI_2010/suisun_2010_juloct/hsib_area.csv")%>%
#  mutate(scenario = "SMSCG+100TAF", YT = "Above Normal", Year = 2010)

act2010c = read_csv("Data/habitat_output/HSI_2010/suisun_2010_julaug/hsib_area.csv")%>%
  mutate(scenario = "SMSCG", YT = "Above Normal", Year = 2010)


noact2017 = read_csv("Data/habitat_output/HSI_2017/base_gate_fix/hsib_area.csv") %>%
  mutate(scenario = "Base", YT = "wet", Year = 2017)


act2017 = read_csv("Data/habitat_output/HSI_2017/100taf0815/hsib_area.csv")%>%
  mutate(scenario = "SMSCG", YT = "wet", Year = 2017)


noact2021 = read_csv("Data/habitat_output/noact/hsib_group.csv") %>%
  mutate(scenario = "Base", YT = "below Normal", Year = 2021)
act2021 = read_csv("Data/habitat_output/smscg4/hsib_group.csv") %>%
  mutate(scenario = "SMSCG", YT = "below Normal", Year = 2021)

allscenarios = bind_rows(noact2010, act2010c, noact2017, act2017, noact2021, act2021) %>%
 # mutate(SuisunBay = (`SE Suisun`+ `NE Suisun`+`NW Suisun`+`SW Suisun`)/4) %>%
  select(time,  scenario, YT, Year, `Suisun Marsh`,`SE Suisun`, `NE Suisun`,`NW Suisun`,`SW Suisun`)

areas=data.frame(region = c("Suisun Marsh", "SW Suisun", "SE Suisun", "NE Suisun", "NW Suisun"),
                 area = c(5201.163518,2680.745477,6415.036890,4887.930174, 12478.843228))

allscenlong = pivot_longer(allscenarios, cols = c(`Suisun Marsh`:`SW Suisun`), names_to = "region", values_to = "HSI") %>%
  left_join(areas) %>%
  mutate(GoodHSI = area*HSI)

allscenp1 = pivot_wider(allscenlong, id_cols = c(time, YT, Year, scenario), names_from = region, values_from = GoodHSI) %>%
  mutate(SuisunBay = `SE Suisun`+ `NE Suisun`+`NW Suisun`+`SW Suisun`)

allscenpivot = pivot_wider(allscenp1, 
                           id_cols = c(time, YT, Year), 
                           names_from = scenario, values_from = c(`Suisun Marsh`, SuisunBay)) %>%
  mutate(percentchangemarsh = (`Suisun Marsh_SMSCG`-`Suisun Marsh_Base`)/(`Suisun Marsh_Base`)*100, 
         perchangeBay = (SuisunBay_SMSCG-SuisunBay_Base)/SuisunBay_Base*100,
         changemarsh = (`Suisun Marsh_SMSCG`-`Suisun Marsh_Base`), 
         changeBay = (SuisunBay_SMSCG-SuisunBay_Base),
         DOY = yday(time), Month = month(time)) 



#the summer fall action is jun 1 through Oct 31, which is day 152 through 305

ggplot(allscenpivot, aes(x = DOY, y = percentchangemarsh, color = YT))+ geom_line()+
  ylab("Percent Change in Delta Smelt \n Habitat area in Suisun Marsh with SMSCG action")+
  theme_bw()+ xlab("Day of Year")+
  coord_cartesian(xlim = c(172, 305))

ggplot(allscenpivot, aes(x = DOY, y = perchangeBay, color = YT))+ geom_line()+
  ylab("Percent Change in Delta Smelt \n Habitat area in Suisun Bay with SMSCG action")+
  theme_bw()+ xlab("Day of Year")+
  coord_cartesian(xlim = c(172, 305))


ggplot(allscenpivot, aes(x = DOY, y = changemarsh, color = YT))+ geom_line()+
  ylab("Change in Delta Smelt \n Habitat area (acres) in Suisun Marsh with SMSCG action")+
  theme_bw()+ xlab("Day of Year")+
  coord_cartesian(xlim = c(172, 305))

ggplot(allscenpivot, aes(x = DOY, y = changeBay, color = YT))+ geom_line()+
  ylab("Change in Delta Smelt \n Habitat area in Suisun Bay (acres) with SMSCG action")+
  theme_bw()+ xlab("Day of Year")+
  coord_cartesian(xlim = c(172, 305))
#what is the average boost in habitat by water year type?

averages = filter(allscenpivot, DOY >152, DOY < 305) %>%
  mutate(`Suisun Marsh_Base` = case_when(YT == "below Normal" & DOY > 270 ~0,
                               TRUE ~ `Suisun Marsh_Base`),
         SuisunBay_Base = case_when(YT == "below Normal" & DOY > 270 ~0,
                             TRUE ~ SuisunBay_Base),
         `Suisun Marsh_SMSCG` = case_when(YT == "below Normal" & DOY > 270 ~ 0,
                                TRUE ~ (`Suisun Marsh_SMSCG`)),
            SuisunBay_SMSCG = case_when(YT == "below Normal" & DOY > 270 ~0,
                              TRUE ~ SuisunBay_SMSCG)) %>%
  group_by(YT, Year) %>%
  summarize(MarshGates = mean(`Suisun Marsh_SMSCG`, na.rm =T), BayGates = mean(SuisunBay_SMSCG, na.rm =T),
            MarshBase = mean(`Suisun Marsh_Base`, na.rm =T), BayBase = mean(SuisunBay_Base, na.rm =T)) %>%
  mutate( perchangemarsh = (MarshGates-MarshBase)/(MarshBase)*100, perchangeBay = (BayGates-BayBase)/BayBase*100,
         changemarsh = (MarshGates-MarshBase), changeBay = (BayGates-BayBase))

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
                           labels = c("Base", "7 days-on/ 7 days-off",
                                      "60 days continuous", "60 days continuous, followed by 100 TAF",
                                      "100 TAF continusous starting July1",
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

ggsave("Habatatpercent100TAF.png", device = "png", width =8, height =6)

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


ggsave("Habatatacres100TAF.png", device = "png", width =9, height =6)


write.csv(HabitatPercent, "Habitatpercent100TAF.csv", row.names = FALSE)

#make a nice version of the area by date plot

habarea2 = mutate(habarea, Zone = case_when(region %in% c("NE Suisun", "NW Suisun",
                                                         "SE Suisun", "SW Suisun") ~ "Suisun Bay",
                                           region == "Suisun Marsh" ~ "Suisun Marsh")) %>%
  filter(!is.na(Zone)) %>%
  group_by(Zone, scenario, time, YT, Year, DOY) %>%
  summarize(LSZ = sum(lsz_area)) %>%
  mutate(scenario = factor(scenario, levels = c("base", "7_7", "60d", "60d_100TAF", "100TAFjul1", "100TAFaug15"),
                           labels = c("Base", "7 days-on/ 7 days-off",
                                      "60 days continuous", "60 days continuous, followed by 100 TAF",
                                      "100 TAF continusous starting July1",
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
