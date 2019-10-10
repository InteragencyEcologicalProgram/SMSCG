#let's take a look at the zooplankton data.


library(tidyverse)
library(readxl)

#import the biomass data
zoopB <- read_excel("FMWT_TNSZooplanktonBPUEMarch2019.xlsx", 
                    sheet = "FMWT&TNS ZP BPUE")

#Now filter it so it's just the stations and time period we are interested in
stas = data.frame(Station = as.character(c(513, 520, 801, 802, 606, 609, 610, "Mont", "NZ032")), 
                  Region = c(rep("River", 4), rep("Suisun Marsh", 5)))

zoopB = filter(merge(zoopB, stas, by = "Station"), Year == 2018, Month == 7 | Month ==8 | Month==9| Month==10) 

#Pick out just the columns we are interested in
zoopB2= mutate(zoopB, Acartiella = ACARTELA + ASINEJUV, 
              Tortanus = TORTANUS + TORTJUV,
                Limnoithona = LIMNOSPP + LIMNOSINE + LIMNOTET + LIMNOJUV,
              Pseudodiaptomus = PDIAPFOR + PDIAPMAR + PDIAPJUV + PDIAPNAUP,
              `Other Calaoids` = ACARTIA + DIAPTOM + EURYTEM + OTHCALAD + 
                SINOCAL + EURYJUV + OTHCALJUV + SINOCALJUV + ACARJUV + DIAPTJUV + SINONAUP + EURYNAUP,
              `Other Cyclopoids` = AVERNAL + OITHDAV + OITHSIM + OTHCYCAD + OITHJUV + OTHCYCJUV,
              Other = HARPACT + OTHCOPNAUP + ALLCLADOCERA)
zoopB2 = zoopB2[,c(1,3,5, 67:74)]
zoopB2$sample = 1:nrow(zoopB2)
zoopB2$Month = factor(zoopB2$Month, labels = c("Jul", "Aug", "Sep", "Oct"))

#now from wide to long
zooplong = gather(zoopB2, key = "Taxa", value = "BPUE", -Station, -Month, - Year, -Region.y, -sample)

#Now the summary version
zoopsum = group_by(zooplong, Region.y, Month, Taxa) %>% summarize(meanB = mean(BPUE), sdB = sd(BPUE), n = length(BPUE))

#reorder the factor levels so they look nicer
zoopsum$Taxa = factor(zoopsum$Taxa, levels = c("Other","Limnoithona", "Other Cyclopoids",
                                               "Other Calaoids", 
                                               "Tortanus", "Pseudodiaptomus","Acartiella"))
zoopsum$Month = factor(zoopsum$Month, labels = c("Jul", "Aug", "Sep", "Oct"))

b1 = ggplot(zoopsum, aes(x = Month, y = meanB))
b2 = b1 + geom_bar(stat = "identity", aes(fill = Taxa)) + facet_wrap(~Region.y) +
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("Mean BPUE (µgC/m3)") +
  geom_label(aes(x = Month, y = 100, label = paste("n=", n))) + 
  theme_few() + theme(text = element_text(family = "sans", size = 12),
    legend.text = element_text(face = "italic"))

b2

ggsave(plot = b2, filename = "zoopsplot.tiff", device = "tiff", width = 6, height =4, units = "in", dpi = 300)


#####################################################################################################
#now a glm of total BPUE
zooptots = group_by(zooplong, Region.y, Month, Station, sample) %>% summarize(BPUE = sum(BPUE), logBPUE = log(BPUE))
zooptots$Month = factor(zooptots$Month, labels = c("Jul", "Aug", "Sep", "Oct"))
hist(zooptots$BPUE)
hist(zooptots$logBPUE)

#quick boxplot to see whether we would expect any difference in total BPUE
tot = ggplot(data = zooptots, aes(x= Month, y = BPUE))
tot + geom_boxplot() + facet_wrap(~Region.y)

shapiro.test(zooptots$BPUE)
shapiro.test(zooptots$logBPUE)
#with october added in with need the log-transformed data. 

zlm1 = glm(BPUE~Month*Region, data = zooptots)
summary(zlm1)
plot(zlm1)

zlm2 = glm(BPUE~Month + Region, data = zooptots)
summary(zlm2)
plot(zlm2)

zlm3 = lm(logBPUE~Month*Region.y, data = zooptots)
summary(zlm3)
plot(zlm3)
visreg(zlm3, xvar = "Month", by = "Region.y")

zlm4 = lm(logBPUE~Month+Region.y, data = zooptots)
summary(zlm4)
plot(zlm4)
visreg(zlm4)

#################################################################################################
#What does an NMDS look like?

library(vegan)
zNMDS = metaMDS(zoopB2[,5:11], trymax = 50)
zNMDS


results.nmds = zoopB2 %>%
  #  na.omit() %>%
  # mutate_if(is.numeric, ~as.vector(scale(.x))) %>%
  nest() %>%
  mutate(
    nmds = map(data,
               ~ .x %>%
                 select(-Station, -Month, - Year, -Region.y, -sample) %>%
                 metaMDS(distance = "bray", autotransform = FALSE,
                         try = 50, trymax = 100)),
    scores = map(nmds, scores),
    envfit = map2(nmds, data, ~ envfit(ord = .x,
                                       env = select(.y, -Station, -Month, - Year, -Region.y, -sample))),
    envload = map(envfit, ~.x$vectors$arrows %>% 
                    as_tibble(rownames = "Species")),
    envfit2 = map2(nmds, data, ~ envfit(ord = .x,
                                        env = select(.y, Month, Region.y))),
    envload2 = map(envfit2, ~.x$factors$centroids %>% 
                     as_tibble(rownames = "groups")),
    newdata = map2(data, scores,
                   ~ bind_cols(select(.x, Month, Region.y, sample),
                               as_tibble(.y)) %>% group_by(Region.y, Month) %>%
                     mutate(num = 1:n()) %>% ungroup())
  )


results.hulls = results.nmds %>%
  select(newdata) %>%
  unnest() %>%
  nest(-Month, -Region.y) %>%
  mutate(
    hull = map(data, ~ chull(select(.x, NMDS1, NMDS2))),
    poly = map2(data, hull, ~ .x[c(.y, .y[1]),])
  ) %>%
  select(Region.y, Month, poly) %>%
  unnest()


vec.scale = .5
w = 8
h = 6


base = ggplot(results.nmds$newdata[[1]]) +
  ggthemes::theme_few(base_size = 18) +
  coord_fixed() +
  #scale_color_manual(NULL, values = c("Miner" = "#377eb8",
   #                                   "Lindsey" = "#4daf4a",
    #                                  "Liberty" = "#984ea3"),
    #                 breaks = c("Miner", "Lindsey", "Liberty"),
     #                labels = c("Miner", "Lindsey", "Liberty")) +
  #scale_fill_manual(NULL, values = c("leafpack" = "red",
   #                                  "sweepnet" = "yellow"), 
    #                breaks = c("leafpack", "sweepnet"),
     #               labels = c("leaf pack", "sweep net")) +
#  scale_shape_manual(NULL, values = c("SAV" = 10,
 #                                     "EAV" = 21, "FAV" = 19, "channel" = 2), drop = FALSE) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    Shape = guide_legend(order = 0)
  ) +
  
  geom_point(
    data = results.nmds$newdata[[1]],
    aes(x = NMDS1, y = NMDS2, color = Month,
        shape = Region.y), size = 4) +
  
  geom_label(x = 1, y = 1, label = paste("Stress =",
                                         round(results.nmds$nmds[[1]]$stress, 2))) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )+
  geom_polygon(
    data = results.hulls,
    aes(x = NMDS1, y = NMDS2,
        fill = Region.y, group = Region.y:Month),
    alpha = 0.25) +
  geom_segment(data = results.nmds$envload[[1]],
               aes(xend = vec.scale * NMDS1, yend = vec.scale * NMDS2, group = Species),
               x = 0, y = 0, arrow = arrow(length = unit(0.1, "inches"),
                                           type = "closed"), size = 1, lineend = "round") +
  geom_text(data =results.nmds$envload[[1]], aes(x= NMDS1*.55, y = NMDS2*.55, 
                             label = Species))
base

#Not add teh "envfit" centroids

#pull out results from each grouping
envfits = results.nmds$envload2[[1]]
targs = envfits[1:4,]
regs = envfits[7:9,]
samps = envfits[5:6,]

species = rownames(results.nmds[[2]][[1]]$species)
specs = as_tibble(results.nmds[[2]][[1]]$species)
specs = cbind(species, specs)

base + 
  #geom_text(data = targs, 
  #         aes(x= NMDS1, y = NMDS2, 
  #            label = c("channel", "EAV", "FAV", "SAV"))) +
  geom_text(data =specs, aes(x= MDS1, y = MDS2, 
                             label = species))
#That is hideous. Let's try again.

