#make a map of smelt catch

library(tidyverse)
library(lubridate)
library(sf)
library(ggmap)
library(readxl)
library(deltamapr)
library(ggspatial)

smelt = read_excel("Data/smeltcatch_2023.xlsx")

smeltsf = st_as_sf(smelt, coords = c("LongitudeStart", "LatitudeStart"), crs = 4326) %>%
  mutate(Date = str_sub(as.character(SampleDate), 6, 10))

ggplot()+
  geom_sf(data = WW_Delta, fill = "grey90")+
  geom_sf(data = smeltsf, aes(shape = Survey, fill = LifeStage, color = LifeStage), size =5)+
  scale_shape_manual(values = c(23, 24, 25))+
  geom_sf_label(data = smeltsf, aes(label = Date), hjust =0, nudge_x = 0.01)+
  coord_sf(ylim = c(38.0, 38.45), xlim = c(-122.1, -121.55))+
  theme_bw()+
  ylab(NULL)+
  xlab(NULL)+
  annotation_scale()+
  annotation_north_arrow(aes(location = "tl"))
