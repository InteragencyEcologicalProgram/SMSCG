#Look at some different ways of plotting the data for environmental variables during the 2018 action

library(tidyverse)
library(lubridate)

#load the data from the sondes
load("~/salinity control gates/SMSCG/actiondata.RData")
load("~/salinity control gates/SMSCG/waterquality.RData")
yrtyp = read.csv("wateryeartypes.csv")
names(yrtyp) = c("Year","Index","YT","YT2" )
str(action.daily)

p1 = ggplot(data = action.daily, aes(x = Datetime, y = Mean, color = Station))
p1 + geom_line() + facet_wrap(~Analyte, scales = "free_y")

action.daily$month = month(action.daily$Datetime)
action.timeseries$Month = month(action.timeseries$Datetime)

action.monthly = group_by(action.timeseries, Station, Analyte, Month) %>% summarize(Mean = mean(Value), sd = sd(Value))
action.monthly$YT2 = "action"

p2 = ggplot(data = action.monthly, aes(x=Month, y = Mean))
p2 + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd))+
  facet_grid(Station~Analyte, scales = "free_y")
  
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

#just the stations we are interested in
alldata = filter(alldata, Station == "(C-2B)  Collinsville B" | Station == "(S-64)  National Steel" |
                   Station =="(S-54)  Hunter Cut")

alldata$Station = factor(alldata$Station, levels = c("(C-2B)  Collinsville B", "(S-64)  National Steel" ,"(S-54)  Hunter Cut"),
                         labels = c("River", "East Marsh", "West Marsh"))

alldata$Month = factor(alldata$Month, levels = c(7, 8, 9),
                         labels = c("July", "August", "September"))

alldata$Analyte = factor(alldata$Analyte, levels = c("Salinity","Temperature","Fluorescence","Turbidity"))

#Graph it
p4 = ggplot(data = alldata, aes(x=Station, y = Mean, fill = YT2))
p4 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "YT2", position = "dodge")+
  facet_grid(Analyte~Month, scales = "free_y", space = "free_x")+
  scale_fill_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL)

#Graph it a different way
p5 = ggplot(data = alldata, aes(x=Month, y = Mean, fill = YT2))
p5 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "YT2", position = "dodge")+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_fill_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL)

#lines instead of bars
alldata$month2 = as.numeric(alldata$Month)
p5.1 = ggplot(data = alldata, aes(x=month2, y = Mean,color = YT2))
p5.1 + geom_line() + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "YT2", width = 0.25)+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_color_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL)

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
#now for some linear models
lm1 = lm(Value ~ Datetime + Station, data = filter(action.timeseries, Analyte == "Fluorescence"))
summary(lm1)
library(visreg)
visreg(lm1)

#ted just wants month versus station
action.timeseries$Month = factor(action.timeseries$Month, labels = c("July", "Aug", "Sep"))
lm2 = lm(Value ~ Month + Station, data = filter(action.timeseries, Analyte == "Fluorescence"))
summary(lm2)
visreg(lm2)

#in that case, we could just do an ANOVA
am1 = aov(Value ~ Month*Station, data = filter(action.timeseries, Analyte == "Fluorescence"))
summary(am1)
TukeyHSD(am1)

#But I dont' know if we want to use the whole time series, or the daily averages, or the weekly averages!
#look at the daily averages
action.daily$Month = factor(action.daily$month, labels = c("Jul", "Aug", "sep"))
am2 = aov(Mean ~ Month + Station, data = filter(action.daily, Analyte == "Fluorescence"))
summary(am2)
TukeyHSD(am2)
visreg(am2)

#check for autocorrelation
plot(am2)
acf(am2$residuals)

########################################################################################################
#the points are going to have a lot of temporal autocorrelation, which I haven't really dealt with before
library(nlme)
#first do it with the daily data
mdl.ac <- gls(Mean ~ Month*Station, data=filter(action.daily, Analyte == "Fluorescence"), 
              correlation = corAR1(form=~Datetime|Station),
              na.action=na.omit)
summary(mdl.ac)

mdl <- gls(Mean ~ Month*Station, data=filter(action.daily, Analyte == "Fluorescence"), 
              na.action=na.omit)
summary(mdl)

#see if adding autocorrelation improves the model
anova(mdl, mdl.ac)


#now look at the whole time series - or not, that broke R.
#mdl.ac2 <- gls(Value ~ Month*Station, data=filter(action.timeseries, Analyte == "Fluorescence"), 
 #             correlation = corAR1(value = 0.5, form=~Datetime|Station),
  #            na.action=na.omit)
#summary(mdl.ac2)

##########################################################################################################
#another option is putting a lag in the dependent variable
library(DataCombine)
dailyF =filter(action.daily, Analyte == "Fluorescence")
dailyF = slide(dailyF, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagF", slideBy = -1)


mdll <- lm(Mean ~ Month*Station + lagF, data= dailyF,
              na.action=na.omit)
summary(mdll)
visreg(mdll, xvar = "Month", by = "Station")
plot(mdll)
acf(mdll$residuals)
#Much better!
# I like the simiplicity of this version.

#turbidity
dailyTurb =filter(action.daily, Analyte == "Turbidity")
dailyTurb = slide(dailyTurb, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT", slideBy = -1)


mdllT <- lm(Mean ~ Month*Station + lagT, data= dailyTurb,
            na.action=na.omit)
summary(mdllT)
visreg(mdllT, xvar = "Month", by = "Station")

#temperature
dailyTemp =filter(action.daily, Analyte == "Temperature")
dailyTemp = slide(dailyTemp, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT", slideBy = -1)


mdllC <- lm(Mean ~ Month*Station + lagT, data= dailyTemp,
             na.action=na.omit)
summary(mdllC)
visreg(mdllC, xvar = "Month", by = "Station")

#salinity
dailysal =filter(action.daily, Analyte == "Salinity")
dailysal = slide(dailysal, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT", slideBy = -1)


mdlls <- lm(Mean ~ Month*Station + lagT, data= dailysal,
             na.action=na.omit)
summary(mdlls)
visreg(mdlls, xvar = "Month", by = "Station")
#################################################################################################
