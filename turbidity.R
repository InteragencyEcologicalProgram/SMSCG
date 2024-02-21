#Continuous plots of turbidity for Vivian

library(tidyverse)
library(lubridate)
library(dataRetrieval)

library(wql)
library(RColorBrewer)

library(cder)

########################################################################
#look at just turbidity to see how often turbidity is higher in dry years

#query cdec for a selection of stations in different areas of the Delta
turb = cdec_query(c("BDL","RVB", "CSE", "MAL", "NSL", "OMR"), sensors = c(27),
                  start.date = as.Date("2000-01-01"), end.date = today())

#calculate the daily averages 
turbdaily = turb %>%
  mutate(Date = date(DateTime)) %>%
  filter(Value>0, Value<1000) %>%
  group_by(Date, StationID, SensorType) %>%
  summarize(Value = mean(Value, na.rm = T)) %>%
  mutate(DOY = yday(Date), Year = year(Date), Month = month(Date),
         WY = case_when(Month %in% c(11,12) ~ Year +1,
                        TRUE ~ Year))

#bring in the file of water year types
wyrs = read_csv("data/wtryrtype.csv") %>%
  select(WY, Index, `Yr-type`)

#join water year types to the data
turbdaily = left_join(turbdaily, wyrs)

#Set up variables for Day of water year
turbdaily2 = mutate(turbdaily, Yr_type = case_when(WY == 2023 ~ "2023",
                                                   TRUE ~ `Yr-type`),
                    DOWY = case_when(DOY >305 ~ DOY-305,
                                     DOY<=305 ~ DOY +60)) %>%
  filter(DOWY < 365)

#Calculate average turbidity by year type and day of water year
turbyears = group_by(turbdaily2, DOWY, Yr_type, StationID, SensorType) %>%
  summarize(Value = mean(Value, na.rm = T)) %>%
  mutate(Station = factor(StationID, levels = c("RVB", "BDL","CSE", "MAL", "NSL", "OMR"), 
                          labels = c("Rio Vista", "Suisun Marsh", "Collinsville", "Mallard Island",
                                     "National Steel", "OMR")),
         Yr_type = factor(Yr_type, levels = c("C", "D", "BN", "AN", "W" ,"2023"),
                          labels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2023")))

#plot it
ggplot(turbyears, aes(x = DOWY, y = Value, color = Yr_type)) + geom_line()+
  facet_wrap(~StationID, scales = "free_y")

#just summer-fall

turbyearssf = filter(turbyears, DOWY>200)

ggplot(filter(turbyears, Yr_type !=2023), aes(x = DOWY, y = Value, color = Yr_type)) + geom_smooth()+
  facet_wrap(~Station, scales = "free_y")+
  scale_color_brewer(palette = "Dark2", name = "Water Year\nType")+
  geom_line(data=filter(turbyears, Yr_type == "2023"), linewidth = 1, color = "black")+
  scale_x_continuous(breaks = c(60, 120, 180, 212, 250, 280, 310, 340), labels = c("Jan", "Mar", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  ylab(NULL)+xlab("Day of Year")+ theme_bw()


ggplot(filter(turbyears, Yr_type !=2023), aes(x = DOWY, y = Value, color = Yr_type)) + geom_line()+
  facet_wrap(~Station, scales = "free_y")+
  scale_color_brewer(palette = "Dark2", name = "Water Year\nType")+
  geom_line(data=filter(turbyears, Yr_type == "2023"), linewidth = 1, color = "black")+
  scale_x_continuous(breaks = c(60, 120, 180, 212, 250, 280, 310, 340), labels = c("Jan", "Mar", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  ylab(NULL)+xlab("Day of Year")+ theme_bw()
