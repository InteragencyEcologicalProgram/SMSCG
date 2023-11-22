#Check the continuous data with the QAQC stuff
library(tidyverse)
library(lubridate)

CSE = read_csv("Data/CSE All.csv") %>%
  mutate(DATETIME = mdy_hm(DATETIME))

names(CSE)

ggplot(CSE, aes(x = DATETIME, y = Turbidity, color = `Turbidity - QAQC Flag`))+
  geom_point()

ggplot(CSE, aes(x = DATETIME, y = Turbidity, color = `Turbidity - QAQC Flag`))+
  geom_point()+
  coord_cartesian(ylim = c(0,200))

summary(CSE)



ggplot(test, aes(x = DATETIME, y = Turbidity, color = `Turbidity - QAQC Flag`))+
  geom_point()+
  coord_cartesian(ylim = c(0,200))
summary(test)

#The SOP says we should use 3, but Jason used 2. He also use dthe absolute value of slope instead of positive negative slope. 
CSE = mutate(CSE, Lagturb = lag(Turbidity, 1), 
             Leadturb = lead(Turbidity, 1),
             Leadslope = ((Leadturb-Turbidity)/15), 
             lagslope =(Turbidity-Lagturb)/15,
             turbflag = case_when(abs(Leadslope) > 2 & abs(lagslope) > -2 ~ "X",
                                 TRUE ~ "G" ),
             turbflagA = case_when((Leadslope < -3) & (lagslope > 3) ~ "X",
                                  TRUE ~ "G" )) 

test = filter(CSE, `Turbidity - QAQC Flag` == "X")
testA = filter(CSE, `turbflagA` == "X")
test2 = filter(CSE, Leadslope >1.5)
test3 = filter(CSE, lagslope < -1.5)

ggplot(filter(CSE, turbflag == "X"), aes(x = DATETIME, y = Turbidity, color= `Turbidity - QAQC Flag`))+
  geom_point()+
  coord_cartesian(ylim= c(0,200))

ggplot(filter(CSE, turbflag == "G"), aes(x = DATETIME, y = Turbidity, color= `Turbidity - QAQC Flag`))+
  geom_point()+
  coord_cartesian(ylim= c(0,200))

ggplot(CSE, aes(x = DATETIME, y = Turbidity, color= `Turbidity - QAQC Flag`))+
  geom_point()+
  coord_cartesian(ylim= c(0,200))

ggplot(CSE, aes(x = DATETIME, y = Turbidity, color= turbflagA))+
  geom_point()#+
  #coord_cartesian(ylim= c(0,200))

ggplot(CSE, aes(x = DATETIME, y = Turbidity, color= turbflagA))+
  geom_point()+
coord_cartesian(ylim= c(0,200))

ggplot(CSE, aes(x = DATETIME, y = Turbidity, color= `Turbidity - QAQC Flag`))+
  geom_point()+
  coord_cartesian(ylim = c(0,200), xlim = c(ymd_hm("2021-01-18 00:00"), ymd_hm("2021-02-01 00:00")))

ggplot(CSE, aes(x = DATETIME, y = Turbidity, color= `turbflagA`))+
  geom_point()+
  coord_cartesian(ylim = c(0,200), xlim = c(ymd_hm("2021-01-18 00:00"), ymd_hm("2021-02-01 00:00")))

CSEx = filter(CSE, turbflagA=="X"|`Turbidity - QAQC Flag`=="X")

ggplot(CSEx, aes(x = DATETIME, y = Turbidity, color = turbflagA))+ geom_point()+
  coord_cartesian(ylim= c(0,200))

CSEraw = read_csv("Data/CSE RAW.csv", skip =10)
CSEraw = mutate(CSEraw, DATE = mdy_hm(DATETIME)) %>%
  filter(CONSTITUENT == "[Water]  Turbidity (NTU) -(n/a) Sensor Reading { Time Series }  @ 15 min Inst 1 m deep")
ggplot(CSEraw, aes(x = DATE, y =VALUE, color = `QAQC Flag`))+ geom_point()+
  coord_cartesian(ylim = c(0,200), xlim = c(ymd_hm("2021-01-18 00:00"), ymd_hm("2021-02-01 00:00")))


CYGtest = read_csv("https://cawater.sharepoint.com/:x:/r/teams/des-smb-wqp/Shared%20Documents/Data%20Backup/CYG%20SPC.csv?d=we16cf03ee5bb4b218d01e95a3c9137d7&csf=1&web=1&e=HcJ9de")
