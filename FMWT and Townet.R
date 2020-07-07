#combine delta smelt catch for Suisun marsh from FMWT and Summer Townet

library(readxl)
library(tidyverse)

FMWT <- read_excel("FMWT 1967-2018 Catch Matrix_updated.xlsx", sheet = "FlatFile", 
                   col_types = c("numeric","date", "numeric", "text", "date",  
                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text", "text", "text", rep("numeric", times =112)))

#put it in long format instead of wide
FMWTl = gather(FMWT, key = "Species", value = "catch", `Aequorea spp.`:`Yellowfin Goby`)

#rename all the stupid names
names(FMWTl)
names(FMWTl) = c("Year" ,"Date","Survey","Station", "StartTime","Index", 
                 "TopTemp", "TopEC","BottomEC","Turb",
                 "Secchi" , "Depthft","TowVolume","Tide","TowDirection",
                 "Weather","Microcystis","Wave", "Species" , "catch" )

#Ideally, we'd do this analysis on CPUE instead of raw catch
#They didn't caculate volumes until later, so I'll use the average
#volume per station for the older tows

#first replace any zero volumes with NAs, because zero volumes don't make sense
FMWTl$TowVolume[which(FMWTl$TowVolume==0)] = NA

#we may want to analyze year as a factor instead of continuous
FMWTl$Year2 = as.factor(FMWTl$Year)

meanvol = group_by(FMWTl, Station) %>% summarize(mvol = mean(TowVolume, na.rm = T))
FMWTl2 = merge(FMWTl, meanvol)
FMWTl2$TowVolume[which(is.na(FMWTl2$TowVolume))] = FMWTl2$mvol[which(is.na(FMWTl2$TowVolume))]

#Calculate CPUE 
FMWTl2 = mutate(FMWTl2, CPUE = catch*TowVolume,
                program = "FMWT", TowNumber = 1)

#Filter out just the Delta Smelt catch in Suisun Marsh
FMWT_DS = filter(FMWTl2, Species == "Delta Smelt", Station == 605 |Station == 606| Station == 608 )


#Now townet
TownetData <- read_excel("TownetData_1959-2019.xlsx",  
                         col_types = c("numeric","numeric", "date","text",
                                       rep("numeric", 96)),
                         sheet = "CatchPerTow")

#pivot from wide to long
Townet = pivot_longer(TownetData, cols = `Age-0 Striped Bass`:`Yellowfin Goby`, 
                      names_to = "Species", values_to = "Count")

#change the names to match FMWT
names(Townet) = c("Year", "Survey", "Date",
                      "Station", "TowNumber", "Index",
                      "TowVolume", "Secchi", "TopTemp", "TopEC",
                      "Depthft", "Species", "catch")


meanvol = group_by(Townet, Station) %>% summarize(mvol = mean(TowVolume, na.rm = T))
Townet2 = merge(Townet, meanvol)
Townet2$TowVolume[which(is.na(Townet2$TowVolume))] = Townet2$mvol[which(is.na(Townet2$TowVolume))]


Townet2ds = mutate(Townet2, CPUE = catch/TowVolume, program = "Townet") %>%
  filter(Station %in% c(606,609,610), Species == "Delta Smelt")

SuisunSmelt = bind_rows(FMWT_DS, Townet2ds)

#get rid of some of the crappy columns
SuisunSmelt2 = dplyr::select(SuisunSmelt,  -Survey, -Index, -TowDirection, -Weather, -Wave)


#quick plot of catch over time
p1 = ggplot(SuisunSmelt2, aes(x=Date, y = catch)) +
  geom_point() 
p1

recentyears = as.POSIXct(c("2000-01-01", "2020-01-01"))
p1 + coord_cartesian(xlim = recentyears, ylim = c(0,50))

write.csv(SuisunSmelt2, "SuisunSmelt.csv")

SuisunSmelt2$Month = month(SuisunSmelt2$Date)
smeltmonth = group_by(SuisunSmelt2, Month) %>%
  summarize(catch2 = mean(catch), sdcatch = sd(catch)) %>%
  filter(Month %in% c(7,8,9,10))
p2 = ggplot(smeltmonth, aes(x = Month, y = catch2))
p2 + geom_bar(stat = "identity")+ geom_errorbar(aes(ymin = catch2 - secatch, ymax = catch2+secatch))

SuisunSmelt3 = filter(SuisunSmelt2, Month %in% c(7,8,9,10))
p2 = ggplot(SuisunSmelt3, aes(x = as.factor(Month), y = log(catch+1)))
p2 + geom_boxplot()

p2 = ggplot(filter(SuisunSmelt3, Year > 2000), aes(x = as.factor(Month), y = log(catch+1)))
p2 + geom_boxplot()
