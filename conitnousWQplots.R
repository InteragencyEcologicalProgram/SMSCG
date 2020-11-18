#Water quality time series plots for 2020.

#Rosemary Hartman 11/18/2020

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(wql)


#load data 
WQdat = read.csv("Data/des_data.csv")
str(WQdat)

WQdat = mutate(WQdat, time = as.POSIXct(time), Date = date(time))

USGSDeckerCHL = readNWISdata(service = "iv", site = "11455478",  parameterCd = "32316", 
                          startDate="2020-06-01T00:00Z",endDate="2020-10-31T12:00Z")
USGSDeckerCHL2 = mutate(USGSDeckerCHL, analyte_name = "Fluorescence",  cdec_code = "Decker") %>%
  rename(time = dateTime, qaqc_flag_id = X_32316_00000_cd, value = X_32316_00000)

USGSDeckerSC = readNWISdata(service = "iv", site = "11455478",  parameterCd = "00095", 
                            startDate="2020-06-01T00:00Z",endDate="2020-10-31T12:00Z")
USGSDeckerSC2 = mutate(USGSDeckerSC, analyte_name = "Specific Conductance",  cdec_code = "Decker") %>%
  rename(time = dateTime,  qaqc_flag_id = X_BGC.PROJECT_00095_00000_cd, 
         value = X_BGC.PROJECT_00095_00000)

USGSDeckerTemp = readNWISdata(service = "iv", site = "11455478",  parameterCd = "00010", 
                            startDate="2020-06-01T00:00Z",endDate="2020-10-31T12:00Z")
USGSDeckerTemp2 = mutate(USGSDeckerTemp, analyte_name = "Temperature", cdec_code = "Decker") %>%
  rename(time = dateTime,qaqc_flag_id = X_BGC.PROJECT_00010_00000_cd, 
         value = X_BGC.PROJECT_00010_00000)


USGSDeckerTurb = readNWISdata(service = "iv", site = "11455478",  parameterCd = "63680", 
                              startDate="2020-06-01T00:00Z",endDate="2020-10-31T12:00Z")
USGSDeckerTurb2 = mutate(USGSDeckerTurb, analyte_name = "Turbidity",  cdec_code = "Decker") %>%
  rename(time = dateTime, qaqc_flag_id = X_BGC.PROJECT_63680_00000_cd, 
         value = X_BGC.PROJECT_63680_00000)


USGSdeker = rbind(USGSDeckerTemp2, USGSDeckerCHL2, USGSDeckerSC2, USGSDeckerTurb2)


WQdatx = select(USGSdeker, cdec_code, time, value, qaqc_flag_id, analyte_name) %>%
  mutate(Date = date(time)) %>%
               rbind(select(WQdat, -X, -unit_name))

sal = filter(WQdatx, analyte_name== "Specific Conductance") %>%
  mutate(salinity = ec2pss(value/1000, t = 25), analyte_name = "Salinity", value = NULL) %>%
  rename(value = salinity)

WQdatax2 = rbind(WQdatx, sal)

WQdat2 = group_by(WQdatax2, cdec_code, Date, analyte_name) %>%
  summarize(meanV = mean(value, na.rm =T))

WQplot = function(data, param) {
  dat = filter(data, analyte_name == param)
  ggplot(dat, aes(x = Date, y = meanV)) + 
           geom_point() + 
           geom_smooth()+
           ylab(param)+
    facet_wrap(~cdec_code)
}

WQplot(WQdat2, "Fluorescence") + ylab("Chlorophyll Fluorescence (RFU)")
WQplot(WQdat2, "Salinity")  + 
  geom_hline( yintercept = 6, color = "red", linetype = 2)+
 ylab("Salinity (PSU)")
WQplot(WQdat2, "Specific Conductance") 
WQplot(WQdat2, "Turbidity") +
  geom_hline( yintercept = 12, color = "red", linetype = 2)+
  ylab("Turbidity (NTU)")
WQplot(WQdat2, "Temperature") +
  geom_hline( yintercept = 23.9, color = "red", linetype = 2)+
  ylab("Temperature (C)")

WQplot2 = function(data, param) {
  dat = filter(data, analyte_name == param)
  ggplot(dat, aes(x = time, y = value)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth()+
    ylab(param)+
    facet_wrap(~cdec_code)
}


WQplot2(WQdatax2, "Fluorescence") + ylab("Chlorophyll Fluorescence")
WQplot2(WQdatax2, "Salinity") + geom_hline( yintercept = 6, color = "red", linetype = 2)+
  ylab("Salinity (PSU)")
WQplot2(WQdatax2, "Temperature") + geom_hline( yintercept = 23.6, color = "red", linetype = 2)+
  coord_cartesian(ylim = c(15,28))+
  ylab("Temperature (C)")
WQplot2(WQdatax2, "Specific Conductance") 
WQplot2(WQdatax2, "Turbidity") + coord_cartesian(ylim = c(0,150))+ 
  geom_hline( yintercept = 12, color = "red", linetype = 2)+
  ylab("Turbidity (NTU)")
