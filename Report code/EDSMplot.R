#Plot of EDSM's annual summer fall population number
#Lara Mitchell (USFWS) gave me this.

library(tidyverse)
library(readxl)
library(deltafish)
library(sf)
library(deltamapr)

smelt = read_csv("Data/DSM_aggregated_index_EDSM_Jul-Sep_2025-12-11.csv")

smelt = mutate(smelt, ymin = Mean_of_nHats-SE_of_mean_of_nHats+1, ymax =  Mean_of_nHats+SE_of_mean_of_nHats)

smelt = mutate(smelt, ymin = case_when(ymin <1 ~1,
                                       TRUE ~ymin))

ggplot(smelt, aes(x = Year, y = Mean_of_nHats)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.6)+
  scale_y_log10()+
  ylab("Mean Jul-Sep Delta Smelt \nAbundance Estimate")+
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
                     minor_breaks = NULL)+
  theme_bw()

ggsave("Plots/AnnualSmeltPlot.tiff", device = "tiff", width =5, height =5)

#now plot catch of Delta Smelt in Suisun Marsh through the years
catch = read_excel("Data/Running Delta Smelt Catch.xlsx", sheet = "Delta Smelt Catch Data")

catch_sumfall = filter(catch,
                      month(SampleDate) %in% c(6:10)) %>%
  mutate(Month = month(SampleDate), Year = year(SampleDate)) %>%
  group_by(Month, Year, SubRegion) %>%
  summarize(Catch = n())


catch_suisun = filter(catch, SubRegion %in% c("Suisun Marsh", "Grizzly Bay", 
                                              "Mid Suisun Bay", "West Suisun Bay", "Honker Bay"),
                      month(SampleDate) %in% c(6:10)) %>%
  mutate(Month = month(SampleDate), Year = year(SampleDate)) %>%
  group_by(Month, Year, SubRegion) %>%
  summarize(Catch = n())


#how frequently do we have multiple recaptures in the same trawl?

catchtrawls =  catch %>%
  filter(MarkCode != "None") %>%
  group_by(SampleDate, StationCode, SubRegion, LatitudeStart) %>%
  summarize(N = n()) %>%
  mutate(Multiple = case_when(N >1 ~ "YES",
                              TRUE ~ "NO"))

catchtrawlsAnnual = catchtrawls %>%
  mutate(WY = case_when(month(SampleDate) %in% c(10:12) ~ year(SampleDate) +1,
                        TRUE ~ year(SampleDate))) %>%
  group_by(WY) %>%
  summarize(Multiple = length(Multiple[which(Multiple == "YES")]))

test = filter(catch, year(SampleDate) == 2024, MarkCode != "None")

#older data




con <- open_database()

# open our two data tables
surv <- open_survey(con)
fish <- open_fish(con)

# filter for sources and taxa of interest
# Also filter for dates of interest. Although dates and datetimes are stored as text in the dataset,
# you can still filter on them using text strings in the "YYYY-MM-DD" or "YYYY-MM-DD HH:MM:SS" format.

surv_FMWT <- surv %>% 
  filter(Date > "2016-01-01") 
fish_smelt <- fish %>% 
  filter(Taxa %in% c("Hypomesus transpacificus"))


# do a join and collect_data the resulting data frame
# collect_data executes the sql query, converts Date and Datetime columns to the correct format and timezone, and gives you a table
allsmelt <- left_join(surv_FMWT, fish_smelt) %>% 
  collect_data() 
# close connection to database
close_database(con)

#now make spatial, attach subregions, and summarise

smeltreg = filter(allsmelt, !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(R_EDSM_Subregions_18P23)) %>%
  st_join(R_EDSM_Subregions_18P23) %>%
  st_drop_geometry() %>%filter(SubRegion %in% c("Suisun Marsh", "Grizzly Bay", 
                                                       "Mid Suisun Bay", "West Suisun Bay", "Honker Bay"),
                               month(Date) %in% c(6:10))
smeltsum = smeltreg %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month, SubRegion) %>%
  summarize(Catch = sum(Count)) %>%
  filter(Year <2022) %>%
  bind_rows(catch_suisun)
  
  
ggplot(smeltsum, aes(x = Year, y = Catch, fill = SubRegion)) + geom_col()+
  ylab("Catch of Delta Smelt in Summer/Fall in Suisun Bay/Marsh")+
  scale_x_continuous(breaks = c(2017, 2019, 2021, 2023, 2025))


#ok, how about all recaptures in summer, not just marsh?


smeltregx = filter(allsmelt, !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(R_EDSM_Subregions_18P23)) %>%
  st_join(R_EDSM_Subregions_18P23) %>%
  st_drop_geometry() %>%filter(
                               month(Date) %in% c(6:10))
smeltsumx = smeltregx %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month, SubRegion) %>%
  summarize(Catch = sum(Count)) %>%
  filter(Year <2022) %>%
  bind_rows(catch_sumfall)

ggplot(smeltsumx, aes(x = Year, y = Catch, fill = SubRegion)) + geom_col()+
  ylab("Catch of Delta Smelt in Summer/Fall")+
  scale_x_continuous(breaks = c(2017, 2019, 2021, 2023, 2025))+
  scale_fill_manual(values = c("red", "blue", "grey", "green4", "red3", "yellow",
                               "orange", "white", "black", "cyan", "slateblue", "yellowgreen",
                               "wheat", "seagreen", 'gold', "grey30"))

