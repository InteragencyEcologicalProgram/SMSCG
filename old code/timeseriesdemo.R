#Some examples of time-series models

#Rosemary Hartman
#7/1/2020

#These examples use the data from teh SMSCG action of 2018

#First load required libraries

library(tidyverse)
#remotes::install_github("tidyverts/fable")
#library(fable)
#library(feasts)
#library(tsibble)
library(TTR)

#now load the data
load("actiondata.RData")

#https://stats.stackexchange.com/questions/9506/stl-trend-of-time-series-using-r

############################################################################################
#examine autocorrelation and partial autocorrelation

library(lme4)

lm = lmer(Sepal.Length~Sepal.Width + (1|Species), data = iris)

# we want to look at the autoregressive and moving-average structure
# of the data. The moving average is generally interpreted from the 
# autocorrelation function (acf) plot, while the autoregressive
# structure is interpreted from the partial autocorrelation function 
# (acf) plot.
# - lags in acf: correlation of a stationary time series with its 
#   own lagged values
#' - lags in pacf: correlation of a stationary time series with its 
#   own lagged values, *controlling for correlation accounted for by
#   smaller lags*

#let's just look at the salinity
dailySal = filter(action.daily, Analyte == "Salinity")

#autocorrelation plot
dailySal %>%
  filter(Station == first(Station)) %>%
  pull(Mean) %>%
  acf()

#Partial autocorrelation plot
dailySal %>%
  filter(Station == first(Station)) %>%
  pull(Mean) %>%
  pacf()

#################################################################################################
#trend decomposition
#look at the daily trend in temperature
tempseries = filter(action.timeseries, Analyte == "Temperature", Station == first(Station)) %>%
  select(Datetime, Value)

#first convert just the temperature values into a time series
ts3 = as.ts(tempseries)

#Now use the "stl" function to extract the seasonal component. In this case the "season" is
#one day, which was 96 observations (every 15 minutes)
dec1 = stl(ts(ts3[,2], freq = 96), s.window = "per")
plot(dec1)

#If we want to run a model with just the trend component, we can adjust the origional 
#time series by subtracting the daily component and/or the random component

ts_adj = ts(ts3[,2], freq = 96) - dec1$time.series[,"seasonal"]
ts_adj2 = ts(ts3[,2], freq = 96) - dec1$time.series[,"remainder"]- dec1$time.series[,"seasonal"]


plot(ts(ts3[,2], freq = 96))
plot(ts_adj2)
plot(ts_adj)

#Of course, you can also just take the daily average, but it's not as cool.
tempday = filter(action.daily, Analyte == "Temperature", Station == first(Station)) %>%
  select(Datetime, Mean)
tsday = as.ts(tempday[,2])
plot(tsday)

#######################################################################################################
# Now, we have autocorrelation even in our daily averages, and we need to get rid of them.
#one way is to use the  "gls" function
#and add a correlation structure

#first do it with the daily data
tempday$Month = as.factor(month(tempday$Datetime))
mdl.ac <- gls(Mean ~ Month, data= tempday, 
              correlation = corAR1(form=~Datetime),
              na.action=na.omit)
summary(mdl.ac)

mdl.ac2 <- gls(Mean ~ Month, data=tempday, 
               correlation = corARMA(form=~Datetime, p = 2, q = 0, fixed = F))
summary(mdl.ac2)


#we want to test this versus our simple model without correlation structure
mdl <- gls(Mean ~ Month, data=tempday, 
           na.action=na.omit)
summary(mdl)

#see if adding autocorrelation improves the model
anova(mdl, mdl.ac, mdl.ac2)
acf(mdl$residuals)
acf(mdl.ac2$residuals)

#WEll, this is a bit of a mess, but maybe a different variable will work better

#let's try salinity
dailySal$Month = as.factor(month(dailySal$Datetime))
mdl.ac <- gls(Mean ~ Month*Station, data= dailySal, 
              correlation = corAR1(form=~Datetime | Station),
              na.action=na.omit)
summary(mdl.ac)

mdl.ac2 <- gls(Mean ~ Month*Station, data=dailySal, 
               correlation = corARMA(form=~Datetime | Station, p = 3, q = 0, fixed = F))
mdl <- gls(Mean ~ Month*Station, data=dailySal, 
           na.action=na.omit)

anova(mdl, mdl.ac, mdl.ac2)
acf(mdl.ac2$residuals)
pacf(mdl.ac2$residuals)
pacf(mdl.ac$residuals)
pacf(mdl$residuals)
#I'll have to work a little more to better understand how this works. But it's a start


##########################################################################################################
#another option is putting a lag in the dependent variable


#Use the "slide" funciton to create a new variable that is one time step off from the origional variable
dailySal = slide(dailySal, "Mean", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagS", slideBy = -1)
dailySal = slide(dailySal, "lagS", TimeVar = "Datetime", GroupVar = "Station", NewVar = "lagS2", slideBy = -1)


#look at the model now.
mdll <- lm(Mean ~ Month*Station + lagS, data= dailySal)
summary(mdll)
visreg(mdll)
plot(mdll)
acf(mdll$residuals)
#Better, but still problematic. let's try two time steps.

mdllx <- lm(Mean ~ Month*Station + lagS + lagS2, data= dailySal)
summary(mdllx)
visreg(mdllx, xvar = "Month", by = "Station")
plot(mdllx)
acf(mdllx$residuals)
pacf(mdllx$residuals)
#That's quite a bit better. 

#####################################################################################################

#now the ARIMA models
ts.data = dailySal %>%
  filter(Station == "(S-64)  National Steel") %>%
  as_tsibble(index = "Datetime", key = c("Station", "Analyte"))

fit = ts.data %>%
  model(arima = ARIMA(Mean), ets = ETS(Mean))

# look at the coefficients
fit %>% tidy()

#Part of the time-series fitting process removes the seasonal trend. 
#but we might be really interested in the seasonal trend. 
#could remove teh monthly average and fit the residuals, then add the 
#monthly average back in. 

# look at the residuals
fitaug = fit %>% augment()

# plot residuals
fitaug %>%
  ggplot() + aes(x = Datetime, y = .resid) +
  geom_boxplot() +
  facet_wrap(~.model)

fitaug %>%
  ggplot() + aes(x = Datetime, y = .resid) +
  geom_line() +
  facet_wrap(~.model)


# try a forecast
fc = fit %>%
  forecast(h = "20 days")

fc %>% autoplot(ts.data)

# try a linear model of the original data
lm1 = lm(Mean ~ Datetime, data = fitaug)
lm1.2= lm(Mean ~as.factor(Month), data = fitaug)
summary(lm1.2)
plot(lm1.2)
####QUESTION: How do I plot this versus month instead?

plot(lm1)
summary(lm1)
## not good

# try a linear model of the residuals
fitaug$Month = as.factor(month(fitaug$Datetime))
lm2 = lm(.resid~ Month, fitaug) 

plot(lm2)
summary(lm2)
visreg(lm2)
#Not super significatn. Maybe it detrended it so we aren't seeing the difference
#Figure out how to deal with the trend. Or maybe it 