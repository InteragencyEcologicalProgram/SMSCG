#Look at some different ways of plotting the data for environmental variables during the 2018 SMSCG action

#Rosemary Hartman
#23 Sep. 2019

library(tidyverse)
library(DataCombine)
library(lubridate)
library(visreg)

#load the data from the sondes
load("~/salinity control gates/SMSCG/actiondata.RData")
load("~/salinity control gates/SMSCG/waterquality.RData")
yrtyp = read.csv("wateryeartypes.csv")
names(yrtyp) = c("Year","Index","YT","YT2" )
str(action.daily)


#first just some exploritory plots
p1 = ggplot(data = action.daily, aes(x = Datetime, y = Mean, color = Station))
p1 + geom_line() + geom_point() + facet_wrap(~Analyte, scales = "free_y")

p1 = ggplot(data = action.timeseries, aes(x = Datetime, y = Value, color = Station))
p1 + geom_line() + facet_wrap(~Analyte, scales = "free_y")

#Now let's seperate it by motnh
action.daily$Month = month(action.daily$Datetime)
action.timeseries$Month = factor(month(action.timeseries$Datetime), labels = c("Jul", "Aug", "Sep"))

#calculate the mean and standard deviation per month. 
action.monthly = group_by(action.timeseries, Station, Analyte, Month) %>% summarize(Mean = mean(Value), sd = sd(Value))
action.monthly$YT2 = "action"

#Quick bar plot of the action
p2 = ggplot(data = action.monthly, aes(x=Month, y = Mean))
p2 + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd))+
  facet_grid(Station~Analyte, scales = "free_y")

#Now look at it a different way
p3 = ggplot(data = action.monthly, aes(x=Month, y = Mean, fill = Station))
p3 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "Station", position = "dodge")+
  facet_wrap(Analyte~., scales = "free_y")

#organize the historical data
historical.timeseries$Month = month(historical.timeseries$Datetime)
historical.timeseries$Year = year(historical.timeseries$Datetime)
historical.monthly =  group_by(filter(historical.timeseries, !is.na(Value), Year != 2018 & Year !=2019), Station, Analyte, Month, Year) %>% 
  summarize(Mean = mean(Value, na.rm = T), sd = sd(Value, na.rm=T))

#add water year type
hist.monthly = merge(historical.monthly, yrtyp)

#average monthly water quality
hist.monthsum = group_by(hist.monthly, Station, Analyte, Month, YT2) %>% 
  summarize(Mean2 = mean(Mean), sd = sd(Mean))
hist.monthsum = rename(hist.monthsum, Mean = Mean2)

#just for the months we are interested in
hist.monthsum = filter(hist.monthsum, Month ==7 | Month ==8 |Month == 9)

#put the data together
alldata = rbind(hist.monthsum, action.monthly)

# filter out just the stations we are interested in
alldata = filter(alldata, Station == "(C-2B)  Collinsville B" | Station == "(S-64)  National Steel" |
                   Station =="(S-54)  Hunter Cut")

alldata$Station = factor(alldata$Station, levels = c("(C-2B)  Collinsville B", "(S-64)  National Steel" ,"(S-54)  Hunter Cut"),
                         labels = c("River", "East Marsh", "West Marsh"))

alldata$Month = factor(alldata$Month, levels = c(7, 8, 9),
                         labels = c("July", "August", "September"))

alldata$Analyte = factor(alldata$Analyte, levels = c("Salinity","Temperature","Fluorescence","Turbidity"))

#Graph it
alldata$sd[which(alldata$YT2 == "action")] = NA
p4 = ggplot(data = alldata, aes(x=Station, y = Mean, fill = YT2))
p4 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "YT2", position = "dodge")+
  facet_grid(Analyte~Month, scales = "free_y", space = "free_x")+
  scale_fill_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL)+
  theme(legend.position="bottom")

#Graph it a different way ########################## THIS IS THE ONE FOR THE PAPER
p5 = ggplot(data = alldata, aes(x=Month, y = Mean, fill = YT2))
p5.1 = p5 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "YT2", position = "dodge")+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_fill_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL)+
  ylab("Turbidity (NTU)      Chlorophyll (ug/L)    Temperature (C)     Salinity (PSU)")+
  theme(legend.position="bottom")

p5.1

ggsave(plot = p5.1, filename = "WQplot.tiff", device = "tiff", width = 6.5, height =7, units = "in", dpi = 300)



#lines instead of bars
alldata$month2 = as.numeric(alldata$Month)
p5.1 = ggplot(data = alldata, aes(x=month2, y = Mean,color = YT2))
p5.1 + geom_line() + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "YT2", width = 0.25)+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_color_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL)

#And another way to look at it.
qp6 = ggplot(data = alldata, aes(x=YT2, y = Mean, fill = Month))
p6 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "Month", position = "dodge")+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_x_discrete(labels = c("2018", "Below Normal",
                                 "Above normal"), name = NULL)

p7 = ggplot(data = alldata, aes(x=Month, y = Mean, fill = Station))
p7 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "Month", position = "dodge")+
  facet_grid(Analyte~YT2, scales = "free_y", space = "free_x")

#one more try...
historical.daily$month = month(historical.daily$Datetime)
historical.daily$Year = year(historical.daily$Datetime)
hist.daily = merge(historical.daily, yrtyp, by = "Year")[c(1:5, 8, 10, 16, 19)]
action.daily$month = month(action.daily$Datetime)
action.daily$Year = year(action.daily$Datetime)
act.daily = action.daily[c(1:4, 7, 8, 15, 16)]
act.daily$YT2 = rep("action", nrow(act.daily))
daily = rbind(hist.daily, act.daily)
daily$Day = day(daily$Datetime)
daily$jDay = yday(daily$Datetime)



#just the stations we are interested in
daily = filter(daily, Station == "(C-2B)  Collinsville B" | Station == "(S-64)  National Steel" |
                   Station =="(S-54)  Hunter Cut")

daily = filter(daily, month == 7 | month ==8 | month ==9)
daily = filter(daily, Count != 0)


daily$Station = factor(daily$Station, levels = c("(C-2B)  Collinsville B", "(S-64)  National Steel" ,"(S-54)  Hunter Cut"),
                         labels = c("River", "East Marsh", "West Marsh"))




p8 = ggplot(data = daily, aes(x=Day, y = Mean, color = YT2))
p8 + #geom_point() +
 geom_smooth(method = "lm")+
  facet_grid(Analyte~Station + month, scales = "free_y", space = "free_x")+
  scale_fill_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL)

p8.1 = ggplot(data = daily, aes(x=jDay, y = Mean, color = YT2))
p8.1 + geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_color_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL) +
  xlab("day of year")

##########################################################################################################
