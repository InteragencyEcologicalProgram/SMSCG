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

ggsave("Plots/smeltmap2023.tiff", device = "tiff", width =8, height =8)
ggsave("Plots/smeltmap2023.png", device = "png", width =8, height =8)

smelt2 = read_excel("Data/Running Delta Smelt Catch_2023-09-05.xlsx", sheet = "Delta Smelt Catch Data")

smeltsf = st_as_sf(smelt2, coords = c("LongitudeStart", "LatitudeStart"), crs = 4326) %>%
  mutate(Date = str_sub(as.character(SampleDate), 6, 10)) %>%
  filter(SampleDate > ymd("2023-09-30"))

ggplot()+
  geom_sf(data = WW_Delta, fill = "grey90")+
  geom_sf(data = smeltsf, aes(shape = ReleaseMethod,  fill = ReleaseMethod), size =3)+
  scale_fill_brewer(palette = "Set2", labels = c("Hard (carboy)", "Hard (large scale)", "Hard (trailer)",
                                                  "NA - unmarked fish", "Soft (carboy)"))+
  scale_shape_manual(values = c(21,22,23,24,25), labels = c("Hard (carboy)", "Hard (large scale)", "Hard (trailer)",
                                                  "NA - unmarked fish", "Soft (carboy)"))+
  #geom_sf_label(data = smeltsf, aes(label = Date), hjust =0, nudge_x = 0.01)+
  coord_sf(ylim = c(37.8, 38.35), xlim = c(-122.2, -121.45))+
  theme_bw()+
  ylab(NULL)+
  xlab(NULL)+
  annotation_scale()+
  annotation_north_arrow(aes(location = "tl"))

