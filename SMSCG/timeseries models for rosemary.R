#time series models of water quality data for the SMSCG experiment in 2018
#Data is from sondes stationed along Montezuma Slough and at Collinsville, put into
#R by Michael Koohafkan. 

#Michael Koohafkan
#September 10, 2019

#First load required libraries
library(tidyverse)
remotes::install_github("tidyverts/fable")
library(fable)
library(feasts)

#now load the data
load("actiondata.RData")

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

action.daily %>%
  filter(Analyte == "Fluorescence", Station == first(Station)) %>%
  pull(Mean) %>%
  acf()

action.daily %>%
  filter(Analyte == "Fluorescence", Station == first(Station)) %>%
  pull(Mean) %>%
  pacf()

# we can use the fable package to fit ARIMA models and 
# Exponential Smoothing (ETS) models
# https://otexts.com/fpp2/arima-ets.html
ts.data = action.daily %>%
  filter(Analyte == "Fluorescence", Station == first(Station)) %>%
  as_tsibble(index = "Datetime", key = c("Station", "Analyte"))

fit = ts.data %>%
  model(arima = ARIMA(log(Mean)), ets = ETS(log(Mean)))

# look at the coefficients
fit %>% tidy()

# look at the residuals
fitaug = fit %>% augment()

# plot residuals
fitaug %>%
  ggplot() + aes(x = Datetime, y = .resid) +
  geom_line() +
  facet_wrap(~.model)

# try a forecast
fc = fit %>%
  forecast(h = "20 days")

fc %>% autoplot(ts.data)

# try a linear model of the original data
lm1 = lm(log(Mean) ~ Datetime, data = fitaug)

plot(lm1)
summary(lm1)
## not good

# try a linear model of the residuals
lm2 = lm(.resid~ Datetime, data = fitaug) 

plot(lm2)
summary(lm2)
## better
#################################################################################################
#Try the model Ted wants
fitaug$Month = factor(month(fitaug$Datetime), labels = c("July", "Aug", "Sep"))

lm3 = lm(.resid~Month, data = fitaug)
summary(lm3)


#We need station in there too
ts.data = action.daily %>%
  filter(Analyte == "Fluorescence") %>%
  as_tsibble(index = "Datetime", key = c("Station", "Analyte"))
ts.data$Month = factor(month(ts.data$Datetime), labels = c("July", "Aug", "Sep"))

fit2 = ts.data %>%
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
  facet_wrap(Station~.model)

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

#try something different
lm3 = gls(log(Mean) ~ Month*Station, correlation = corARMA(0.5, form =~Datetime), data = dailyF)
summary(lm3)
visreg(lm3, xvar = "Month", by = "Station")
