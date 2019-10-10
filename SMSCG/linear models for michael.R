#Generalized linear models of water quality data for the SMSCG experiment in 2018
#Data is from sondes stationed along Montezuma Slough and at Collinsville, put into
#R by Michael Koohofkin. 

#Rosemary Hartman
#September 6, 2019

#First load required libraries
library(tidyverse)
library(DataCombine)
library(lubridate)
library(visreg)

#now load the data
load("~/salinity control gates/SMSCG/actiondata.RData")
load("~/salinity control gates/SMSCG/actiondata-october.RData")
action.timeseries = rbind(action.timeseries, action.timeseries.october)

#########################################################################################################
#We want to see if there was any difference between the month when the gates were operating (August) and
#the "before" time period (July) and the "after" time period (September)

#LEt's start with Chlorophyll Fluorescence
#If we use a linear model, we assume the data is normally distributed and has homogeneous variance.

#A histogram will give us some idea of normality
hist(filter(action.timeseries, Analyte == "Fluorescence")$Value)
#Close, but not great

#A Shapiro-wilks test is better
shapiro.test(filter(action.timeseries, Analyte == "Fluorescence")$Value)
#but I guess our dataset is too big for that!

#let's try a linear model and look at the diagnostic plots
lm1 = lm(Value ~ Datetime + Station, #the formula we want to model: value is predicted by date/time and station
         data = filter(action.timeseries, Analyte == "Fluorescence")) #tell it what data to use

#look at the results
summary(lm1)

#look at diagnostic plots to make sure you have met the assumptions of the model
plot(lm1)
#The q-q plot looks a little overdisperssed at the high end (not surprising given the histogram)
#but it's not terrible.

#the "visreg" function plots the affect of each variable while holding the others constant,
#so it's nice for seeing what is going on.
visreg(lm1)

#However, Ted suggested we just have "Month" (as a factor) rather than "datetime" as a continuous variable.
#First make a new variable for "month" using the lubridate package
action.timeseries$Month = month(action.timeseries$Datetime) 
#now convert it to a factor
action.timeseries$Month = factor(action.timeseries$Month, labels = c("July", "Aug", "Sep", "Oct", "Nov"))
action.timeseries = filter(action.timeseries, Month != "Nov")

#try the model again
#There will also be an interaction term between month and station, because we expect more of an
#effect of gate operations on the marsh stations than Collinsville.
lm2 = lm(Value ~ Month*Station, data = filter(action.timeseries, Analyte == "Fluorescence"))
summary(lm2)
plot(lm2)
visreg(lm2, xvar = "Month", by = "Station")

#But I dont' know if we want to use the whole time series, or the daily averages, or the weekly averages!
#Let's try the daily averages

#need to put a "month variable in our daily data
action.daily = rbind(action.daily, action.daily.october)
action.daily$Month = month(action.daily$Datetime)
action.daily$Month = factor(action.daily$Month, labels = c("Jul", "Aug", "Sep", "Oct"))
dailyF = filter(action.daily, Analyte == "Fluorescence")

#look at the histogram again
hist(dailyF$Mean)
#still not great

#look at the diagnostic plots
am2 = lm(Mean ~ Month*Station, data = dailyF)
summary(am2)
plot(am2)
#There aren't as many significant differences, but the diagnostic plots look better in this version.

visreg(am2, xvar = "Month", by = "Station")
#it's easier to see what's going on at least

#check for autocorrelation
acf(am2$residuals)
#because the lines cross the blue line, we've got a lot of autocorrelation we need to deal with 


########################################################################################################
#I haven't really dealt with autocorrelation before, but apparently one way is to use the  "gls" function
#and add a correlation structure

#first do it with the daily data
mdl.ac <- gls(Mean ~ Month*Station, data=dailyF, 
              correlation = corAR1(form=~Datetime|Station),
              na.action=na.omit)
summary(mdl.ac)

mdl.ac2 <- gls(Mean ~ Month*Station, data=dailyF, 
              correlation = corARMA(form=~Datetime|Station),
              na.action=na.omit)
summary(mdl.ac2)


#we want to test this versus our simple model without correlation structure
mdl <- gls(Mean ~ Month*Station, data=dailyF, 
           na.action=na.omit)
summary(mdl)

#see if adding autocorrelation improves the model
anova(mdl, mdl.ac)
acf(mdl.ac$residuals)
#We improved the model, but we still have a lot of autocorrelation. I'm also not totally sure what the correlation
#sturcture is doing.

##########################################################################################################
#another option is putting a lag in the dependent variable
library(DataCombine)

#Use the "slide" funciton to create a new variable that is one time step off from the origional variable
dailyF = slide(dailyF, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagF", slideBy = -1)
dailyF = slide(dailyF, "lagF", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagF2", slideBy = -1)


#look at the model now.
mdll <- lm(log(Mean) ~ Month*Station + lagF, data= dailyF)
summary(mdll)
visreg(mdll, xvar = "Month", by = "Station")
plot(mdll)
acf(mdll$residuals)
#Much better!
# I like the simiplicity of this version.
#look at the model now.
mdllx <- lm(log(Mean) ~ Month*Station + log(lagF) + log(lagF2), data= dailyF)
summary(mdllx)
visreg(mdllx, xvar = "Month", by = "Station")
plot(mdllx)
acf(mdllx$residuals)


#Put a lag in the turbidity data
dailyTurb =filter(action.daily, Analyte == "Turbidity")
dailyTurb = slide(dailyTurb, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT", slideBy = -1)
dailyTurb = slide(dailyTurb, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT2", slideBy = -2)


#model the turbidity
mdllT <- lm(log(Mean) ~ Month*Station + log(lagT) + log(lagT2), data= dailyTurb)
summary(mdllT)
visreg(mdllT, xvar = "Month", by = "Station")
acf(mdllT$residuals)


#put a lag in the temperature data
dailyTemp =filter(action.daily, Analyte == "Temperature")
dailyTemp = slide(dailyTemp, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT", slideBy = -1)
dailyTemp = slide(dailyTemp, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT2", slideBy = -2)


#model the temperature
mdllC <- lm(Mean ~ Month*Station + lagT, data= dailyTemp)
summary(mdllC)
visreg(mdllC, xvar = "Month", by = "Station")
#there was a trend towards cooler temperatures in Spetember (to be expected), but no difference between stations. 
mdllC2 <- lm(Mean ~ Month*Station + lagT + lagT2, data= dailyTemp)
summary(mdllC2)
visreg(mdllC2, xvar = "Month", by = "Station")



#put a lag in the salinity
dailysal =filter(action.daily, Analyte == "Salinity")
dailysal = slide(dailysal, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT", slideBy = -1)
dailysal = slide(dailysal, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagT2", slideBy = -2)


#model the salinity
mdlls <- lm(Mean ~ Month*Station + lagT, data= dailysal)
summary(mdlls)
visreg(mdlls, xvar = "Month", by = "Station")
#We defnintely have differences in salinilty by month and by station!

mdlls <- lm(Mean ~ Month*Station + lagT + lagT2, data= dailysal)
summary(mdlls)
visreg(mdlls, xvar = "Month", by = "Station")


#################################################################################################
