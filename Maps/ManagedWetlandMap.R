#map for managed wetland proposal

library(sf)
library(tidyverse)
library(deltamapr)
library(ggspatial)

wetlands =H_CARI_wetlands %>%
  st_transform(crs = st_crs(R_Suisun)) %>%
  st_intersection(R_Suisun) %>%
  select(name, globalid, LOCATION)



st_write(wetlands, "maps/wetlands.shp")

welands2 = read_csv("maps/wetlands.csv") %>%
  select(globald,clickcd,   clcklbl,   legcode, lglbll1, lglbll2, lgnd_hd, )

wetlands = left_join(wetlands, welands2, by = c("globalid" = "globald")) %>%
  filter(!lglbll1 %in% c("Playa", "Slope and Seep Wetlands", "Vernal Pool")) %>%
  mutate(Classes = factor(lglbll1, levels = c("Tidal Marsh" , "Tidal Flat and Marsh Panne", 
                                              "Pond and associated vegetation", "Pond",
                                              "Fluvial Channel", "Tidal Channel", "Subtidal Water"),
                          labels = c("Tidal Wetland", "Tidal Flat",
                                     "Managed Wetland",  "Managed Wetland", "Channel", "Tidal Channel", "Open Water")))


ggplot(WW_Delta)+
  geom_sf(fill = "lightblue")+
  geom_sf(data = wetlands,
                        aes(fill = Classes, color = Classes))+
  coord_sf(xlim = c(-122.15, -121.85), ylim = c(38.05, 38.25))+
  scale_fill_manual(values = c("darkgreen", "tan", "yellowgreen",  "slateblue4",  "slateblue3","lightblue"))+
  scale_color_manual(values = c("darkgreen", "tan", "yellowgreen",  "slateblue4", "slateblue3","lightblue"))+
  theme_bw()+ annotation_north_arrow(location = "tl")+
  annotation_scale()

ggsave("maps/SuisunWetlands.png", width =8, height =5, device = "png")
