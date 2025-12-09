#
library(tidyverse)

yrtyp = read_csv("Data/wateryeartypes.csv")
zoops = read_csv(file = "Zoop_month_ave2.csv")
regs = read_csv("Rosies_regions.csv")
zoop = left_join(zoops, regs) %>%
  filter(month %in% c(8,9,10,11)) %>%
  rename(Year = water_year)
zoop2 = group_by(zoop, Region, Year) %>%
  summarize(BPUE = mean(month_BPUE)) %>%
  left_join(yrtyp)

ggplot(filter(zoop2, Year >2011, Year < 2020, !is.na(Region)), 
       aes(x = Region, y = BPUE, fill = YT))+
  geom_col() + facet_wrap(~Year)



zoop3 = group_by(zoop2, Region) %>%
  summarize(mBPUE = mean(BPUE, na.rm =T), 
            sd = sd(BPUE, na.rm = T), 
            se = sd/n())

ggplot(zoop3, aes(x = Region, y = mBPUE)) + geom_col()



zoop3rec = zoop2 %>%
  filter(Year >2000, Year <2020, !is.na(Region)) %>%
  group_by(Region) %>%
  summarize(mBPUE = mean(BPUE, na.rm =T), 
            sd = sd(BPUE, na.rm = T), 
            se = sd/n())

ggplot(zoop3rec, aes(x = Region, y = mBPUE, fill = Region)) + 
  geom_col()+
  geom_errorbar(aes(ymin = mBPUE-sd, ymax = mBPUE + sd))

#look at the North Delta by season
zoopx = left_join(zoops, regs) %>%
  filter(!is.na(Region), Region == "North") %>%
  mutate(Season = case_when(
    month %in% c(12,1,2) ~ "Winter",
    month %in% c(3,4,5) ~ "Spring",
    month %in% c(6,7,8) ~ "Summer",
    month %in% c(9,10,11) ~"Fall"
  ), Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>%
  rename(Year = water_year)
zoop2x = group_by(zoopx, Region, Year, Season) %>%
  summarize(BPUE = mean(month_BPUE)) %>%
  left_join(yrtyp) %>%
  mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>%
  filter(Year > 2010, Year < 2020)

ggplot(zoop2x, aes(x = Season, y = BPUE, fill = YT)) + geom_col()+
  facet_wrap(~Year)


#look at the Suisun by season
zoopS = left_join(zoops, regs) %>%
  filter(!is.na(Region), Region == "Suisun Marsh") %>%
  mutate(Season = case_when(
    month %in% c(12,1,2) ~ "Winter",
    month %in% c(3,4,5) ~ "Spring",
    month %in% c(6,7,8) ~ "Summer",
    month %in% c(9,10,11) ~"Fall"
  ), Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>%
  rename(Year = water_year)
zoop2S = group_by(zoopS, Region, Year, Season) %>%
  summarize(BPUE = mean(month_BPUE)) %>%
  left_join(yrtyp) %>%
  mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>%
  filter(Year > 2010, Year < 2020)

ggplot(zoop2S, aes(x = Season, y = BPUE, fill = YT)) + geom_col()+
  facet_wrap(~Year)
