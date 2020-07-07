library(vegan)
library(tidyverse)
library(lubridate)
library(pairwiseAdonis)

WQmatrix = action.daily %>%
  filter(!str_detect(Station, "Bel")) %>%
  mutate(
    OfInterest = case_when(
      Analyte == "Fluorescence" ~ log(Mean),
      Analyte == "Turbidity" ~ log(Mean),
      Analyte == "Salinity" ~ Mean,
      Analyte == "Temperature" ~ Mean,
    ),
    Analyte = if_else(Analyte == "Fluorescence",
                      "Chlorophyll-a", Analyte),
    OfInterest = if_else(is.finite(OfInterest), OfInterest, NA_real_),
    Operation = factor(case_when(
      Datetime < as_datetime("2018-08-03") ~ "Before",
      Datetime < as_datetime("2018-09-08") ~ "During",
      TRUE ~ "After"
    ), c("Before", "During", "After")),
    Station = factor(Station, c("(S-54)  Hunter Cut",
                                "(S-64)  National Steel", "(C-2B)  Collinsville B"),
                     c("Hunter Cut", "National Steel", "Collinsville")),
  ) %>%
  arrange(Datetime)%>%
  select(Datetime, Analyte, Station, OfInterest, Operation) %>%
  spread(Analyte, OfInterest) %>%
  na.omit()

#Permanova

lagWQmatrix = group_by(WQmatrix, Station, Operation) %>%
  transmute(Chla = lag(`Chlorophyll-a`), Salinity = lag(Salinity), Temperature = lag(Temperature), Turbidity = lag(Turbidity))

pm1 = adonis(WQmatrix[,5:7]~ Station + Operation, data = WQmatrix)
pm1
pm2 = pairwise.adonis(WQmatrix[,5:7], c(WQmatrix$Station))
pm2 = pairwise.adonis(WQmatrix[,5:7], WQmatrix$Operation)

#try the NMDS again

NMDS1 = WQmatrix %>% mutate_if(is.numeric, ~as.vector(scale(.x))) %>%
  select(-Datetime, - Station, -Operation) %>%
  metaMDS(distance = "manhattan", autotransform = FALSE,
          try = 50, trymax = 100)


#significance testing of the environmental variables. 
foo=envfit(NMDS1, WQmatrix[,2:3])

plot(NMDS1)
plot(foo)
