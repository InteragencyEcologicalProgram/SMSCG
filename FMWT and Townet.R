#combine delta smelt catch for Suisun marsh from FMWT and Summer Townet

library(readxl)
library(tidyverse)
library(lubridate)
library(wql)

FMWT <- read_excel("FMWT 1967-2018 Catch Matrix_updated.xlsx", sheet = "FlatFile", 
                   col_types = c("numeric","date", "numeric", "text", "date",  
                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text", "text", "text", rep("numeric", times =112)))

wtryrs <- read_excel("wtryrtype.xlsx")
wtryrs = rename(wtryrs, Year = WY)

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

#write.csv(SuisunSmelt2, "SuisunSmelt.csv")

SuisunSmelt2$Month = month(SuisunSmelt2$Date)


SuisunSmelt2.2 = merge(SuisunSmelt2, wtryrs, by = "Year")
  SuisunSmelt2.2 =  mutate(SuisunSmelt2.2, `Yrtype` = factor(`Yr-type`, levels = c("C", "D", "BN", "AN", "W")))

SuisunSmelt3 = filter(SuisunSmelt2.2, Month %in% c(7,8,9,10))
p2 = ggplot(SuisunSmelt3, aes(x = as.factor(Month), y = log(catch+1)))
p2 + geom_boxplot() + facet_wrap(~`Yrtype`)

###############################################################################################################
#monthly averages

smeltmonth = group_by(SuisunSmelt2.2, Month, `Yrtype`) %>%
  summarize(catch2 = mean(catch), sdcatch = sd(catch), secatch = sd(catch)/60,
            catch2log = mean(log(catch+1)), sdcatchlog = sd(log(catch+1)), secatchlog = sd(log(catch+1))/60) %>%
  filter(Month %in% c(7,8,9,10))



p2 = ggplot(smeltmonth, aes(x = Month, y = catch2))
p2 + geom_bar(stat = "identity")+ geom_errorbar(aes(ymin = catch2 - secatch, ymax = catch2+secatch)) + 
  facet_wrap(~`Yrtype`)


p3 = ggplot(smeltmonth, aes(x = Month, y = catch2log))
p3 + geom_bar(stat = "identity")+ geom_errorbar(aes(ymin = catch2log - secatchlog, ymax = catch2log+secatchlog)) + 
  facet_wrap(~`Yrtype`)

#check and make sure we have good august representation
augsmelt = filter(SuisunSmelt2.2, Month == 8)
ausmelt = table(augsmelt$Year, augsmelt$program)

################################################################################################################
#monthly presence/absence
PAfun = function(x) {
  if (sum(x) == 0)  "absent" else "present"
}

SuisunSmeltPA =  group_by(SuisunSmelt2.2, Month, `Yrtype`, Year) %>%
  summarise(PA = PAfun(catch), count = length(catch)) %>%
  filter(Month %in% c(7,8,9,10))

p4 = ggplot(SuisunSmeltPA, aes(x = Month,fill = PA))
p4 + geom_bar(position = "fill") + facet_wrap(~Yrtype)

p4.1 = ggplot(filter(SuisunSmeltPA, Month == 8), aes(x = Yrtype, fill = PA))
p4.1 + geom_bar(position = "fill") 

#############################################################################################################
dayflowall = read.csv("dayflow_all.csv")
dayflowall = mutate(dayflowall, Month = month(Date), Year = year(Date))

dayflowaug = filter(dayflowall, Month == 8)
dayflowaug2 =   group_by(dayflowaug, Year) %>%
  summarize(out = mean(OUT))

augsmelt = merge(augsmelt, dayflowaug2)
augsmeltsum = group_by(augsmelt, Year, out) %>%
  summarize(catch = mean(catch))
p6 = ggplot(augsmeltsum, aes(x = out, y = log(catch+1)))
p6 + geom_point()

simyears = filter(augsmeltsum, out > 4700 & out <6700)

###############################################################################################################
#august catch versus salinity

p7 = ggplot(augsmelt, aes(x = TopEC, y = catch))
p7 + geom_point()

augsmelt$PA = NA
augsmelt$PA[which(augsmelt$catch == 0)] = 0
augsmelt$PA[which(augsmelt$catch > 0)] = 1

p8 = ggplot(augsmelt, aes(x = TopEC, y = PA))
p8 +geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))

b1 = glm(PA ~ TopEC, family = "binomial", data = augsmelt)
summary(b1)
visreg(b1)


p8.1 = ggplot(filter(augsmelt, Year < 2000), aes(x = TopEC, y = PA))
p8.1 +geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))

b2 = glm(PA ~ TopEC, family = "binomial", data = filter(augsmelt, Year < 2000))
summary(b2)
visreg(b2)


p8.1 = ggplot(filter(augsmelt, Year < 2018), aes(x = TopEC, y = PA))
p8.1 +geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))

b2 = glm(PA ~ TopEC, family = "binomial", data = filter(augsmelt, Year < 2018))
summary(b2)
visreg(b2)


p8.3 = ggplot(filter(augsmelt, Year > 2000), aes(x = TopEC, y = PA))
p8.3 +geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))

b3 = glm(PA ~ TopEC, family = "binomial", data = filter(augsmelt, Year > 2000))
summary(b3)
visreg(b3)

recyear = filter(augsmelt, Year > 2000, Year < 2018)
table(recyear$PA)
table(augsmelt$PA)

#########################################################################################
#Convert conductivity to salinity

#used the "ec2pss" function from "wql" with temperature set at 25
augsmelt = mutate(augsmelt, sal = ec2pss(TopEC/1000, t=25))

#do it for the other months too, just to see
SuisunSmelt2.2 = mutate(SuisunSmelt2.2, sal = ec2pss(TopEC/1000, t=25), PA = catch)
SuisunSmelt2.2$PA[SuisunSmelt2.2$catch != 0] = 1
highsal = filter(SuisunSmelt2.2, sal >5, catch != 0)

#graph of all months
p8.x = ggplot(filter(SuisunSmelt2.2, Year < 2018), aes(x = sal, y = PA))
p8.x +geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  xlab("Salinity")+ ylab("Delta Smelt Presence in Suisun Marsh in July-December")

#graph of allmonths, recent years only
p8.x = ggplot(filter(SuisunSmelt2.2, Year < 2018, Year > 2000, Month %in% c(7:12)), aes(x = sal, y = PA))
p8.x +geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  xlab("Salinity")+ ylab("Delta Smelt Presence in Suisun Marsh in July-December") + 
  facet_wrap(~Month)


##########################################################################################
#look at the response to salinity during just august of all years
p8.1 = ggplot(filter(augsmelt, Year < 2018), aes(x = sal, y = PA))
p8.1 +geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  xlab("Salinity")+ ylab("Delta Smelt Presence in Suisun Marsh in August")

b2 = glm(PA ~ sal, family = "binomial", data = filter(augsmelt, Year < 2018))
summary(b2)

test = filter(augsmelt, sal >5 & PA == 1)

#predict the probability of catching smelt with the action (4PSU) and without the action (7PSU)
predict(b2, newdata = data.frame(sal = c(4,7)), type = "response")

##########################################################################################
#repeat with just the post-POD years. THIS IS THE ONE TED LIKES
p8.3 = ggplot(filter(augsmelt, Year > 2000 & Year < 2018), aes(x = sal, y = PA))
p8.3 = p8.3 +geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  xlab("Salinity where smelt were collected (PSU)")+ 
  ylab("Delta Smelt Presence") + theme_bw() +
  theme(text = element_text(size = 16)) + coord_cartesian(xlim = c(0,12.5))+
  scale_y_continuous(breaks = c(0,1)) + scale_x_continuous(breaks = c(0,2,4,6,8,10,12))


ggsave(plot = p8.3, filename = "SmetlSTN.tiff", device = "tiff", width = 7, height =7, units = "in", dpi = 300)


b3 = glm(PA ~ sal, family = "binomial", data = filter(augsmelt, Year > 2000 & Year < 2018))
summary(b3)
predict(b3, newdata = data.frame(sal = c(4,7)), type = "response")

#write.csv(recyear, "TNSsmelt_2001_2018.csv")

##############################################################################################
#upload data to include a histogram
modeldata = read.csv("beldenmodel.csv")
BeldenSalAction <- read.csv("~/salinity control gates/SMSCG/salinity_Belden_Landing_2018_SMSCG_Action.csv")
BeldenSalAction = mutate(BeldenSalAction, Datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second, sep = ","), 
                                                      format = "%Y,%m,%d,%H,%M,%S"),
                         Action = "Action")
          
BeldenSalno <- read.csv("~/salinity control gates/SMSCG/salinity_Belden_Landing_No_Action.csv")
BeldenSalno = mutate(BeldenSalno, Datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second, sep = ","), 
                                                                format = "%Y,%m,%d,%H,%M,%S"),
                     Action = "No Action") %>%
  rename(salinity = salinity_.PSU.)

BeldenSal = rbind(BeldenSalAction, BeldenSalno)

BeldenSalAug = filter(BeldenSal, Month == 8)

ggplot(BeldenSalAug, aes(x = salinity, fill = Action)) + geom_histogram(position = "dodge")
p9 = ggplot(BeldenSalAug, aes(x = salinity, fill = Action)) + geom_density(alpha = 0.5)+
  theme_bw() + theme(legend.position=c(0.2, 0.8), text = element_text(size = 16)) + 
  xlab("Salinity at Beldon's (PSU)") + 
  scale_fill_discrete(name = NULL) + coord_cartesian(xlim = c(0,12.5))+
   scale_x_continuous(breaks = c(0,2,4,6,8,10,12))

#Daily averages
Beldondaily = group_by(BeldenSalAug, Day, Action) %>%
  summarize(sal = mean(salinity))


ggplot(Beldondaily, aes(x = sal, fill = Action)) + geom_histogram(position = "dodge")
ggplot(Beldondaily, aes(x = sal, fill = Action)) + geom_density(alpha = 0.5) 

augfiltered = filter(augsmelt, Year > 2000 & Year < 2018)

# Put them both in one plot
ggplot()+
  geom_point(data = augfiltered, mapping = aes(x = sal, y = PA)) +
 geom_smooth(data = augfiltered, mapping = aes(x = sal, y = PA),
             method = "glm", method.args = list(family = "binomial"))+
  geom_density(data = Beldondaily, mapping = aes(x = sal, fill = Action, ..density../2.2), alpha = 0.5)+
  xlab("Salinity at Beldon's (PSU)")+ ylab("Delta Smelt Presence") + theme_bw() +
  theme(text = element_text(size = 18)) +
  scale_y_continuous(sec.axis = sec_axis(~.*2.2, name = "Density of Modeled Salinity at Beldons Landing"))

#do them in two seperate plots

library(gridExtra)
p10 = grid.arrange(p8.3, p9)



ggsave(plot = p10, filename = "smeltsal.tiff", device = "tiff", width = 7, height =7, units = "in", dpi = 300)


