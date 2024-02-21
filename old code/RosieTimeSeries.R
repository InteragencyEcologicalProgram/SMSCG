#Generalized linear models of water quality data for the SMSCG experiment in 2018
#Data is from sondes stationed along Montezuma Slough and at Collinsville, 

#Rosemary Hartman and Michael Koohofkin
#September 10, 2019

#First load required libraries
library(tidyverse)
library(DataCombine)
library(lubridate)
library(visreg)
library(nlme)
remotes::install_github("tidyverts/fable")
library(fable)
library(feasts)

#load the data
dailyF = read.csv("dailyF.csv", stringsAsFactors = F)
str(dailyF)

#fix the date/time column
dailyF$Datetime = as.Date(dailyF$Datetime)

#create a new variable for "month"
dailyF$Month = factor(month(dailyF$Datetime), labels = c("Jul", "Aug", "Sep"))


#########################################################################################################
#We want to see if there was any difference between the month when the gates were operating (August) and
#the "before" time period (July) and the "after" time period (September)

#let's try a linear model and look at the diagnostic plots
lm1 = lm(log(Mean) ~ Month*Station, data = dailyF) 

#look at the results
summary(lm1)

#the "visreg" function plots the affect of each variable while holding the others constant,
#so it's nice for seeing what is going on.
visreg(lm1, xvar = "Month", by = "Station")

#look at diagnostic plots to make sure you have met the assumptions of the model
plot(lm1)
#it's not terrible.

#check for autocorrelation
acf(lm1$residuals)
#because the lines cross the blue line, we've got a lot of autocorrelation we need to deal with 


########################################################################################################
#I haven't really dealt with autocorrelation before, but apparently one way is to use the  "gls" function
#and add a correlation structure

#first do it with the daily data
mdl.ac <- gls(Mean ~ Month*Station, data=dailyF, 
              correlation = corAR1(form=~Datetime|Station),
              na.action=na.omit)
summary(mdl.ac)

#we want to test this versus our simple model without correlation structure
mdl <- gls(Mean ~ Month*Station, data=dailyF, 
           na.action=na.omit)
summary(mdl)

#see if adding autocorrelation improves the model
anova(mdl, mdl.ac)
acf(mdl.ac$residuals)
#We improved the model, but we still have a lot of autocorrelation. I'm  not totally sure what the correlation
#sturcture is doing.

##########################################################################################################
#another option is putting a lag in the dependent variable
#Use the "slide" funciton to create a new variable that is one time step off from the origional variable
dailyF = slide(dailyF, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagF", slideBy = -1)

#look at the model now.
mdll <- lm(log(Mean) ~ Month*Station + lagF, data= dailyF)
summary(mdll)
visreg(mdll, xvar = "Month", by = "Station")
plot(mdll)
acf(mdll$residuals)
#Better!
# I like the simiplicity of this version. But we do have some autocorrelation still going on. 


##########################################################################################################
#OR, I could include a lag in the residuals from the origional model as a response in the later model. 
lmF = lm(log(Mean)~Month*Station, data = dailyF)
resF = residuals(lmF, type = "response")

#add the residuals and a lag of the residuals to the origional data set.
dailyF = cbind(dailyF, resF) 
dailyF = slide(dailyF, "resF", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagRes", slideBy = -1)

#new model that includes a lag in the residuals as a predictor.
mdll2 <- lm(log(Mean) ~ Month*Station + lagRes, data= dailyF,
            na.action=na.omit)
summary(mdll2)
visreg(mdll2, xvar = "Month", by = "Station")
plot(mdll2)
acf(mdll2$residuals)
#the ACF plot is a little better than the lag in the dependant variable. Plus there are more significant differences.

######################################################################################################
# we can use the fable package to fit ARIMA models and 
# Exponential Smoothing (ETS) models
# https://otexts.com/fpp2/arima-ets.html

fit2 = as_tsibble(dailyF, index = "Datetime", key = c("Station", "Analyte")) %>%
  group_by(Station) %>%
  model(arima = ARIMA(log(Mean)), ets = ETS(log(Mean)))

# look at the coefficients
fit2 %>% tidy()

# look at the residuals
fitaug2 = fit2 %>% augment()

# plot residuals
fitaug2 %>%
  ggplot() + aes(x = Datetime, y = .resid) +
  geom_line() +
  facet_grid(Station~.model)

fitaug2$Month = factor(month(fitaug2$Datetime), labels = c("July", "Aug", "Sep"))

#linear model with the origional data
lm1 = lm(log(Mean) ~Month*Station, data = filter(fitaug2, .model == "arima"))
plot(lm1)
summary(lm1)

#linear model with the ARIMA residuals
lm2 = lm(.resid ~Month*Station, data = filter(fitaug2, .model == "arima"))
plot(lm2)
summary(lm2)
visreg(lm2, xvar = "Month", by = "Station")

#linear model with the ETS residuals
lm3 = lm(.resid ~Month*Station, data = filter(fitaug2, .model == "ets"))
plot(lm3)
summary(lm3)
visreg(lm3, xvar = "Month", by = "Station")
