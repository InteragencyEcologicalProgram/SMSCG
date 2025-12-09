#grizzly WQ   
#demo on how to compare data from continuous sondes

library(tidyverse) # data manipulation package
library(cder) #grabbing data from CDEC (not needed if you're pulling data from your database)
library(lme4) #linear mixed models
library(lmerTest) #p-values for mixed models
library(DHARMa) # diagnostic plots
library(effects) #visualize model rsults

#pull data from the four stations of interest. 
#I just grapped 2020-today, but you can obviously subset it differntly
grizz = cdec_query(c("GZM", "GZB"), 100, "E", start.date = c("2018-01-01"), end.date = today())

ggplot(grizz, aes(x = ObsDate, y = Value, color = StationID)) + geom_point()

#run a quick plot to see what the data look like. Kinda messy
ggplot(grizz, aes(x = ObsDate, y = Value, color = StationID)) + geom_line()

#First model is with the 15 minute data and a random effect of date
b1 = lmer(Value ~ StationID + (1|ObsDate), data = GZLGZB)
summary(b1)
#the stations are significantly different! but are our observations independent?

#these diagnostic plots show you have major deviations from normality of residuals,
#and data are highly autocorrelated
plot(simulateResiduals(b1))
acf(residuals(b1))

#one way to help with temporal autocorrelation is just do daily averages
GZLGZBave = grizz %>%
  mutate(Date = date(ObsDate)) %>% #new variable for date
  group_by(Date, StationID) %>% #summarize by day and station
  summarize(EC = mean(Value, na.rm =T)) %>%
  group_by(StationID) %>%
  filter(!is.nan(EC)) %>% #remove missing values
  mutate(ECn1 = lag(EC), ECn2 = lag(ECn1)) #calculate the one-day and two-day lag of EC
  

#now we can model the daily average EC with yesterday's EC and teh day before yesetrday's EC added as predictors
#note that we no longer need a random effect of date because we only have one observatino per station per date
b2 = lm(EC ~ StationID + ECn1 + ECn2, data = GZLGZBave)
summary(b2)
#STation is no longer significant

plot(simulateResiduals(b2))
#STill some issues with normality of residuals, but much better than before. You could t ry a transformation to make it
#fit better, or not worry aobut it. 
acf(residuals(b2))
#almost all of the autocorrelation is gone! A little left, but I wouldn't worry about that. 
plot(allEffects(b2))


#check on weird hunter value for vivian
dfHUN_long = read_csv("data/dfHUN_long.csv")

HUNtest = HUN %>%
  mutate(Value2 =   case_when((date_time < ymd_hms("2024-04-02 14:10:00") | date_time > ymd_hms("2024-04-02 18:35:00"))  ~  Value )
  )

HUNtest = dfHUN_long %>%
  mutate(Value =   case_when(Parameter == "dissolvedoxygen" &(date_time > ymd_hms("2024-04-02 14:10:00") & date_time < ymd_hms("2024-04-02 18:35:00"))  ~ NA,
                              TRUE ~Value )
  )


foo = filter(HUNtest, is.na(Value))

HUNtestx =filter(dfHUN_long, !(Parameter == "dissolvedoxygen" &(date_time > ymd_hms("2024-04-02 14:10:00") & date_time < ymd_hms("2024-04-02 18:35:00"))))

###################################################################
#good QC'd data ####

combined_df = read_csv("Data/GrizzWQ/combined_df.csv")
combined_df_long = read_csv("Data/GrizzWQ/combined_df_long.csv")
combined_df_1h = read_csv("Data/GrizzWQ/combined_df1h.csv")
combined_df24h = read_csv("Data/GrizzWQ/combined_df24h.csv")

#break these into pairs
GZLGZB = filter(combined_df24h, station %in% c("GZL", "GZB")) %>%
  mutate(Pair = "GZLGZB") 
GZMHUN = filter(combined_df24h, station %in% c("GZM", "HUN"))%>%
  mutate(Pair = "GZMHUN") 

## Start with GZB/GZM ####
ggplot(GZLGZB, aes(x = date_time, y = dailyavg, color = station))+
  facet_wrap(~Parameter, scales = "free_y")+ geom_line()

#Huh, id idn't realize we were missing so much data from GZL
#I guess they only pulled teh action period from 2024


both = bind_rows(GZLGZB, GZMHUN) %>%
  filter(Parameter == "spc")

ggplot(both, aes(x = date_time, y = dailyavg, color = station))+
  facet_wrap(~Pair)+ geom_line()+
  coord_cartesian(xlim = c(ymd("2024-08-10"), ymd("2024-10-15")), ylim = c(0, 18000))+
  ylab("Specific Conductance (uS/cm)")

#we only want to include data where we have values for both stations
include = GZLGZB %>%
  group_by(date_time, Parameter) %>%
  summarize(n = n()) %>% #count number of observations per day. When there are two observations, we have data from both sites
  mutate(include = case_when(n ==2 ~ T,
                             n <2 ~ F))

#Now joint the coints to the full dataset and filter to just the days where we have data for both
GZLGZBfilter = left_join(GZLGZB, include) %>%
  filter(include) %>%
  mutate(DOY = yday(date_time)) %>%
  filter(DOY %in% c(150:310)) #just the summer and fall

#check to make sure we did it right
ggplot(GZLGZBfilter, aes(x = date_time, y = dailyavg, color = station))+
  facet_wrap(~Parameter, scales = "free_y")+ geom_line()

#OK, now the model for salinity
#start with just station and date/time in the model
spcmod1 = lm(dailyavg ~ station + date_time, data = filter(GZLGZBfilter, Parameter == "spc"))
summary(spcmod1)
plot(spcmod1)
plot(simulateResiduals(spcmod1))
#gross
hist(residuals(spcmod1))
#quite gross
acf(residuals(spcmod1))

#let's add a lag term, plus maybe use DOY instead of date
GZLGZBfilter = GZLGZBfilter %>%
  mutate(Year = year(date_time)) %>%
  group_by(station, Parameter, Year) %>% #group by station, parameter, and year, to make sure we lag it correctly
  mutate(LagValue = lag(dailyavg),
         LagValue2 = lag(LagValue),
         DOY = yday(date_time)) %>%
  ungroup()

#next model
spcmod2 = lm(dailyavg ~ station + DOY, data = filter(GZLGZBfilter, Parameter == "spc"))
summary(spcmod2)
plot(spcmod2)
plot(simulateResiduals(spcmod2))
#better
hist(residuals(spcmod2))
#Not as gross
acf(residuals(spcmod2))
#still autocorrelated

#now let's add the lag value
spcmod3 = lm(dailyavg ~ station + DOY + LagValue, data = filter(GZLGZBfilter, Parameter == "spc"))
summary(spcmod3)
plot(spcmod3)
plot(simulateResiduals(spcmod3))
#Yuck
hist(residuals(spcmod3))
acf(residuals(spcmod3))
plot(allEffects(spcmod3))

#do we actually need DOY?
spcmod4 = lm(dailyavg ~ station +LagValue, data = filter(GZLGZBfilter, Parameter == "spc"))
summary(spcmod4)
plot(spcmod4)

#no. Both the anova and AIC tell you that the model is better without DOY in there
anova(spcmod3, spcmod4)
AIC(spcmod3)
AIC(spcmod4)

#We can use the variance inflation factor to see if we have highly colinear predictor factors.
library(car)
vif(spcmod3)
#vif is less than 3 for all parameters, so we are good!

#Most of the autocorrelation is gone, but nowth qq plot is out of watck
#I think the issue is we've got a bunch of low values and then a bunch of hgi values
# But Perry said we don't need to worry about the slightly wonky qq plot, since
#the other assumptions are met. 

GZLGZBfilter2 = filter(GZLGZBfilter, Parameter == "spc")

#what if we did a smooth on day of year?

#this is the package that lets you run Generalized Addative Models (GAMs)
library(mgcv)

#we put a smooth on day of year here. 
spcmod4g = gam(dailyavg ~ station + s(DOY) + LagValue, data = GZLGZBfilter2)
summary(spcmod4g)
plot(simulateResiduals(spcmod4g))
#Meh, not much better.
hist(residuals(spcmod4g))
acf(residuals(spcmod4g))
plot(spcmod4g, all.terms = T)
#Since the linear model without DOY was pretty OK, we'll not worrya bout the GAMs


#turbidity
turbmod1 = lm(dailyavg ~ station + DOY + LagValue, data = filter(GZLGZBfilter, Parameter == "turbidity"))
summary(turbmod1)
plot(turbmod1)
#The QQ plot is skewed, the residuals versus fitted plot is bell-shaped
#and the scale-locaiton red line is slanted. This means our model doesn't fit well.
#let's try log-transforming!


turbmod3 = lm(log(dailyavg) ~ station + DOY + log(LagValue), data = filter(GZLGZBfilter, Parameter == "turbidity"))
summary(turbmod3)
plot(turbmod3)

#beutiful
hist(residuals(turbmod3))
acf(residuals(turbmod3))
plot(allEffects(turbmod3))


#chlorophyll
#we'll probably need to log-transform chlorophyll too. 
chlmod3 = lm(dailyavg ~ station + DOY + LagValue, data = filter(GZLGZBfilter, Parameter == "fluorescence"))
summary(chlmod3)
plot(chlmod3)


chlmod4 = lm(log(dailyavg) ~ station + DOY + log(LagValue), data = filter(GZLGZBfilter, Parameter == "fluorescence"))
summary(chlmod4)
plot(chlmod4)

#beutiful
hist(residuals(chlmod3))
acf(residuals(chlmod3))
plot(allEffects(chlmod3))

###################################################################
#GZMHUN ####

#again filter to just teh days where we have both stations
#and filter to just the summer and fall.
GZMHUNfilter = GZMHUN %>%
  mutate(DOY = yday(date_time), Year = year(date_time)) %>%
  filter(DOY %in% c(150:310)) %>%
  group_by(Year, station, Parameter) %>%
  mutate(Lag1 = lag(dailyavg),
         Lag2 = lag(Lag1)) %>%
  ungroup()

include2 = GZMHUNfilter %>%
  group_by(date_time, Parameter) %>%
  summarize(n = n()) %>%
  mutate(include = case_when(n ==2 ~ T,
                             n <2 ~ F))

GZMHUNfilter = left_join(GZMHUNfilter, include2) %>%
  filter(include)

#make a quick plot
ggplot(GZMHUNfilter, aes(x = date_time, y = dailyavg, color = station)) + geom_line()+
  facet_wrap(~Parameter, scales = "free_y")

#specific conductance
spmod4h = lm(dailyavg ~ station +Lag1, data = filter(GZMHUNfilter, Parameter == "spc"))
summary(spmod4h)
plot(spmod4h)


#we know we need to log-transform turbidity
turbmod4h = lm(log(dailyavg) ~ station +log(Lag1), data = filter(GZMHUNfilter, Parameter == "turbidity"))
summary(turbmod4h)
plot(turbmod4h)

#the residuals versus leverage plot poitns out a few outliers, let's remove those
turbstuff = filter(GZMHUNfilter, Parameter == "turbidity")[-c(498, 500, 502),]

#this looks better
turbmod5h = lm(log(dailyavg) ~ station +log(Lag1), data = turbstuff)
summary(turbmod5h)
plot(turbmod5h)
plot(allEffects(turbmod5h))
#this time there is a difference in turbidity, but it's not very big

#now do cholorophyll
chlmod5h = lm(log(dailyavg) ~ station +log(Lag1), data = filter(GZMHUNfilter, Parameter == "fluorescence"))
summary(chlmod5h)
plot(chlmod5h)
plot(allEffects(chlmod5h))
#a few outliers here too

chlstuff = filter(GZMHUNfilter, Parameter == "fluorescence")[-c(119, 212),]

chlmod5h = lm(log(dailyavg) ~ station +log(Lag1), data = chlstuff)
summary(chlmod5h)
plot(chlmod5h)
plot(allEffects(chlmod5h))
#better! And there is a slight significant difference. 

#We don't expect much difference in temperature, Do, or PH, but we'll look at them 

tempmod5h = lm(dailyavg ~ station +Lag1, data = filter(GZMHUNfilter, Parameter == "watertemperature"))
summary(tempmod5h)
plot(tempmod5h)

domod5h = lm(dailyavg ~ station +Lag1, data = filter(GZMHUNfilter, Parameter == "dissolvedoxygen"))
summary(domod5h)
plot(domod5h)

phmod5h = lm(dailyavg ~ station +Lag1, data = filter(GZMHUNfilter, Parameter == "ph"))
summary(phmod5h)
plot(phmod5h)

#distance between the points

points = data.frame(Station = c("GZM","HUN", "GZL", "GZB"),
                    Latitude = c(38.141304, 38.156000, 38.124250,38.123145),
                    Longitude = c(-122.060196, -122.052700, -122.038117, -122.007582	),
                    type = c("New Station", "Old Station", "Old Station", "New Station"))

library(spacetools)
distances = Waterdist(Water_map = spacetools::Delta,
  Points = points, Latitude_column = Latitude,
                      Longitude_column = Longitude, PointID_column = Station)

library(sf)
library(deltamapr)
library(ggspatial)
points2 = st_as_sf(points, coords = c("Longitude", "Latitude"), crs = 4326)

#quick map

ggplot()+
  geom_sf(data = WW_Delta, fill = "lightskyblue1")+
  geom_sf(data = points2, size = 4, aes(shape = type, fill = type))+
  scale_shape_manual(values = c(22, 21), name = NULL)+
  scale_fill_manual(values = c("tan2", "blue"), name = NULL)+
  geom_sf_label(data = points2, aes(label = Station), nudge_x = 0.005, nudge_y = 0.005)+
  coord_sf(ylim = c(38.1, 38.2), xlim = c(-122.1, -121.99))+
  annotation_north_arrow(location = "tl")+
  annotation_scale()+
  ylab(NULL)+ xlab(NULL)+
  theme_bw()


TRB = filter(WQdata, station == "TRB")
write.csv(TRB, file = "data/TRB_waterquality.csv", row.names = F)

ggplot()