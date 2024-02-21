#Look at some different ways of plotting the data for environmental 
#variables during the 2018 SMSCG action

#Rosemary Hartman
#23 Sep. 2019

library(tidyverse)
library(DataCombine)
library(lubridate)
library(visreg)
library(ggthemes)
library(nlme)

#load the data from the sondes
load("~/salinity control gates/SMSCG/actiondata.RData")
load("~/salinity control gates/SMSCG/waterquality.RData")
load("~/salinity control gates/SMSCG/actiondata-october.RData")

yrtyps = data.frame(an= c(2005, 2006, 2017), bn= c(2002, 2009, 2016)) %>% 
  gather(key = "yrtyp", value = "Year")

str(action.daily)
action.daily2 = rbind(action.daily, action.daily.october)
action.timeseries2 = rbind(action.timeseries, action.timeseries.october)
action.timeseries2$Month = month(action.timeseries2$Datetime)


#first just some exploritory plots
p1 = ggplot(data = action.daily2, aes(x = Datetime, y = Mean, color = Station))
p1 + geom_line() + facet_wrap(~Analyte, scales = "free_y")

p1 = ggplot(data = filter(action.daily2, Analyte == "Turbidity"),
            aes(x = Datetime, y = Mean, color = Station))
p1 + 
  annotate("rect", xmin = ymd("2018-08-02"), xmax = ymd("2018-09-06"),
           ymax = 50, ymin = 0,
           fill = "grey", alpha = 0.4) +
  geom_line() + theme_bw() +
  annotate("text", x = ymd("2018-08-15"), y = 45, label = "Action \nperiod") +
  xlab("Date") + ylab("Daily mean turbidity (NTU)")

p1s = ggplot(data = filter(action.daily2, Analyte == "Salinity"), 
       aes(x = Datetime, y = Mean, color = Station))

p1s + 
  annotate("rect", xmin = ymd("2018-08-02"), xmax = ymd("2018-09-06"),
           ymax = 10, ymin = 0,
           fill = "grey", alpha = 0.4) +
  geom_line() + theme_bw() +
  annotate("text", x = ymd("2018-08-15"), y = 8, label = "Action \nperiod") +
  scale_color_discrete(labels = c("River", "West Marsh", "East Marsh"))+
  xlab("Date") + ylab("Daily mean salinity (PSU)")

p1 = ggplot(data = action.timeseries, aes(x = Datetime, y = Value, color = Station))
p1 + geom_line() + facet_wrap(~Analyte, scales = "free_y")

#Now let's seperate it by motnh
action.daily2$Month = month(action.daily2$Datetime)
#action.timeseries$Month = factor(month(action.timeseries$Datetime), labels = c("Jul", "Aug", "Sep", "Oct"))

#calculate the mean and standard deviation per month. 
action.monthly = group_by(action.timeseries2, Station, Analyte, Month) %>% 
  summarize(Mean = mean(Value), sd = sd(Value), se = sd)
action.monthly$yrtyp = "action"
action.monthly = filter(action.monthly, Month != 11)

#Quick bar plot of the action
p2 = ggplot(data = action.monthly, aes(x=Month, y = Mean))
p2 + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd),
                position = position_dodge(1), width = 0.2)+
  facet_grid(Station~Analyte, scales = "free_y")

#Now look at it a different way
p3 = ggplot(data = action.monthly, aes(x=Month, y = Mean, fill = Station))
p3 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "Station", position = "dodge")+
  facet_wrap(Analyte~., scales = "free_y")

#organize the historical data
historical.daily = mutate(historical.daily, Year = year(Datetime),
                          Month = month(Datetime))

#add the year types
historical.daily2 = merge(historical.daily, yrtyps)

#Calculate the monthly means and standard deviations
historical.monthly =  group_by(filter(historical.daily2, !is.na(Mean), Year != 2018 & Year !=2019), 
                               Station, Analyte, Month, yrtyp) %>% 
  summarize(Mean = mean(Mean, na.rm = T), sds = sd(Mean, na.rm=T)) %>%
  filter(Month %in% c(7,8,9,10))

#add water year type
#hist.monthly = merge(historical.monthly, yrtyps)

#average monthly water quality
#hist.monthsum = group_by(hist.monthly, Station, Analyte, Month, yrtyp) %>% 
#  summarize(Mean2 = mean(Mean, na.rm= T), sd = sd(Mean, na.rm = T), se = mean(sds, na.rm = T))


#that didn't work right. 
#histsum =  group_by(filter(historical.timeseries2, !is.na(Value), Year != 2018 & Year !=2019), Station, Analyte, Month, yrtyp) %>% 
#  summarize(Mean = mean(Value, na.rm = T), sd = sd(Value, na.rm=T), se = mean(sds))


#just for the months we are interested in
#histsum = filter(histsum, Month ==7 | Month ==8 |Month == 9 | Month ==10)

#put the data together
alldata = rbind(hist.monthsum, action.monthly)

# filter out just the stations we are interested in
alldata = filter(alldata, Station == "(C-2B)  Collinsville B" | Station == "(S-64)  National Steel" |
                   Station =="(S-54)  Hunter Cut") %>%
  mutate(Station = factor(Station, levels = c("(S-54)  Hunter Cut", "(S-64)  National Steel" ,"(C-2B)  Collinsville B"),
                         labels = c("West Marsh", "East Marsh", "River")),
         Month = factor(Month, levels = c(7, 8, 9, 10),
                         labels = c("Jul", "Aug", "Sep", "Oct")),
         Analyte = factor(Analyte, levels = c("Salinity","Temperature","Fluorescence","Turbidity")),
         yrtyp = factor(yrtyp, levels = c("bn","an","action")))


#Graph it
alldata$sd[which(alldata$yrtyp == "action")] = NA
p4 = ggplot(data = alldata, aes(x=Station, y = Mean, fill = yrtyp))
p4 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "yrtyp", position = "dodge")+
  facet_grid(Analyte~Month, scales = "free_y", space = "free_x")+
  scale_fill_discrete(labels = c("2018 (Below Normal)", "2002, 2009, 2016 (Below Normal)",
                                 "2005, 2006, 2017 (Above Normal)"), name = NULL)+
  theme(legend.position="bottom")

#Graph it a different way ########################## THIS IS THE ONE FOR THE PAPER
p5 = ggplot(data = alldata, aes(x=Month, y = Meanx, fill = yrtyp))
p5.1 = p5 + geom_bar(stat = "identity", position = "dodge", color = "black") + 
  geom_errorbar(aes(ymin = Meanx + sd, ymax = Meanx - sd), group = "yrtyp", 
                position = position_dodge(.9), width = .5)+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_fill_manual(values = c(
    "bn" = "#dfc27d",
    "an" = "#92c5de",
    "action" = "#e31a1c"),
    labels = c("dry summers",
                                 "wet summers","2018"), name = NULL)+
  ylab("Turbidity (NTU)      Chlorophyll (ug/L)    Temperature (C)     Salinity (PSU)")+
  theme_bw()+ theme(legend.position="bottom", strip.background.y = element_blank(),
                     strip.text.y = element_blank(), axis.title.x = element_blank())

p5.1

ggsave(plot = p5.1, filename = "WQplot.tiff", device = "tiff", width = 7, height =7, units = "in", dpi = 300)

#Just the salinity (for es conference presentation)
p5x = ggplot(data = filter(alldata, Analyte == "Salinity"), aes(x=Month, y = Mean, fill = yrtyp))
p5.1x = p5x + geom_bar(stat = "identity", position = "dodge", color = "black") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "yrtyp", 
                position = position_dodge(.9), width = 0.2)+
  facet_grid(.~Station, scales = "free_y", space = "free_x")+
  scale_fill_manual(values = c(
    "bn" = "#dfc27d",
    "an" = "#92c5de",
    "action" = "#e31a1c"),
    labels = c("dry summers",
               "wet summers","2018"), name = NULL)+
  ylab("Salinity (PSU)")+
  theme_few()+ theme(text = element_text(size = 18), legend.position="bottom", strip.background.y = element_blank(),
                     strip.text.y = element_blank(), axis.title.x = element_blank())

p5.1x

ggsave(plot = p5.1x, filename = "salinity.tiff", device = "tiff", width = 11, height =5, units = "in", dpi = 300)



#lines instead of bars
alldata$month2 = as.numeric(alldata$Month)
p5.1 = ggplot(data = alldata, aes(x=month2, y = Mean,color = yrtyp))
p5.1 + geom_line() + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "yrtyp", width = 0.25)+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_color_discrete(labels = c("2018 (Below Normal)", "(1998-2017) Below Normal Years",
                                 "(1998-2017) Above Normal Years"), name = NULL)

#And another way to look at it.
p6 = ggplot(data = alldata, aes(x=yrtyp, y = Mean, fill = Month))
p6 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "Month", position = "dodge")+
  facet_grid(Analyte~Station, scales = "free_y", space = "free_x")+
  scale_x_discrete(labels = c("2018", "Below Normal",
                                 "Above normal"), name = NULL)

p7 = ggplot(data = alldata, aes(x=Month, y = Mean, fill = Station))
p7 + geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Mean + sd, ymax = Mean - sd), group = "Month", position = "dodge")+
  facet_grid(Analyte~yrtyp, scales = "free_y", space = "free_x")
