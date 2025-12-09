#Look at different ways to summarize data

library(tidyverse)
library(stars)
library(raster)
library(deltamapr)
library(rgdal)
library(exactextractr)

#load rasters from RMA models

SMSCG = raster("Data/smscg/smscg_1940-07_hsit-avgmon_rma.tif")
plot(SMSCG, xlim = c(-13600000, -13500000), ylim = c(4550000, 4700000))


#Well, that's pretty 
poly = R_EDSM_Strata_1718P1
poly = st_transform(poly, crs(SMSCG))

test = crop(SMSCG, extent(poly))
test2 = mask(SMSCG, poly)
system.time(
 
  fastermaybe2 <- exact_extract(test2, poly, fun='mean',  
                                force_df = TRUE, progress = TRUE)
)
#Trim to regions of interest and calculate averages within regions


ff <- list.files("Data/smscg/", pattern = "\\.tif$", full=TRUE)
ffn <- list.files("Data/no+action/", pattern = "\\.tif$", full=TRUE)
ffx = c(ff, ffn)
ff2 = ffx[seq(2,60, 2)]
s <- stack(ff2)

ex <- exact_extract(s, poly, fun='mean', force_df = TRUE, progress = TRUE)

meanSHIt = bind_cols(Stratum = poly$Stratum, ex)

meanLong = pivot_longer(meanSHIt, cols = 2:31, names_to = "RunName", values_to = "HSI_mean") %>%
  mutate(RunName = str_remove(RunName, "mean."),
    Year = case_when(
    str_detect(RunName, "1940") ~ "1940",
    str_detect(RunName, "1979") ~ "1979",
    str_detect(RunName, "1986") ~ "1986"
  ),
  Month = case_when(
    str_detect(RunName, ".08") ~ "August",
    str_detect(RunName, ".07") ~ "July",
    str_detect(RunName, ".06") ~ "June",
    str_detect(RunName, ".09") ~ "Sep",
    str_detect(RunName, ".10") ~ "Oct"
  ),
  Action = case_when(
    str_detect(RunName, "no.action") ~ "noAction",
    str_detect(RunName, "smscg") ~"GateAction"
  ))


runname = names(ex) %>%
  str_remove("mean.")


#Ugh, i know there's a beetter way to do this, but I can't figure it out
areas = data.frame(Regions = poly$Stratum)
for (i in 1:30){
exArea <- exact_extract(s[[i]], poly, function(values, coverage_fraction) length(which(values*coverage_fraction >0.5)), 
  progress = TRUE,  force_df = TRUE)
areas = bind_cols(areas, exArea)
}
names(areas) = c("Region", runname)

areaLong = pivot_longer(areas, cols = 2:31, names_to = "RunName", values_to = "HSI_0.5") %>%
  mutate(Year = case_when(
    str_detect(RunName, "1940") ~ "1940",
    str_detect(RunName, "1979") ~ "1979",
    str_detect(RunName, "1986") ~ "1986"
  ),
  Month = case_when(
    str_detect(RunName, ".08") ~ "August",
    str_detect(RunName, ".07") ~ "July",
    str_detect(RunName, ".06") ~ "June",
    str_detect(RunName, ".09") ~ "Sep",
    str_detect(RunName, ".10") ~ "Oct"
  ),
  Action = case_when(
    str_detect(RunName, "no.action") ~ "noAction",
    str_detect(RunName, "smscg") ~"GateAction"
  ))


ggplot(areaLong, aes(x = Month, y = HSI_0.5, fill = Action)) + geom_col(position = "dodge")+
  facet_grid(Region~Year, scales = "free_y")


ggplot(meanLong, aes(x = Month, y = HSI_mean, fill = Action)) + geom_col(position = "dodge")+
  facet_grid(Stratum~Year)

smeltHSI = full_join(areaLong, meanLong, by = c("Region" = "Stratum", "RunName", "Year", "Month", "Action"))

###########################################################
#Create a polygon of all the areas where HSI is over 0.5

starstest = st_as_stars(SMSCG)
starstest[[1]][starstest[[1]] < 0.5] = NA
starstest[[1]][starstest[[1]] >= 0.5] = 1

polytest3 = st_as_sf(starstest)


SMSCG = read_stars("Data/smscg/smscg_1940-07_hsit-avgmon_rma.tif")
ggplot()+geom_stars(data = SMSCG)

################################################################
#now let's create a dataset where we have all the IEP stations
#and see which ones are in "good" habitat
AllIEP_wRegions <- read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/DroughtSynthesis/AllIEP_wRegions.csv")
IEP_sf = st_as_sf(AllIEP_wRegions, coords = c("Longitude", "Latitude"), crs = st_crs(4326))# %>%
 # st_transform(crs = 3857)

#I might actually want to do it habitat area within a km from the point.
IEP_sfbuff = st_buffer(IEP_sf, dist = 1000)
ggplot()+geom_sf(data = IEP_sfbuff)


exstas <- exact_extract(s, IEP_sfbuff, fun='mean', force_df = TRUE, progress = TRUE)
IEP_sf_HSI = cbind(IEP_sf, exstas)
ggplot()+geom_sf(data = IEP_sf_HSI, aes(color = mean.no.action_1940.08_hsit.avgmon_rma))
save(IEP_sf_HSI, file = "HabitatbyStations.RData")
