#Monthly WQ update plot for Britt

library(tidyverse)
library(wql)
library(cder)

#pull most recent water quality data from CDEC
WQ = cdec_query(c("GZM", "GZL", "BDL", "NSL", "RVB",  "HUN", "CSE"), 
                sensors = c(100, 25, 27, 28, 324),
                start.date = as.Date("2026-05-01"), end.date =  today())
str(WQ)

#convert conductivity to salinity, convert farenheight to celcius
#filter out any temperautres below 60 or above 90
#also filter out relly high chlorophyll and turbidity
WQx = mutate(WQ, Value2 = case_when(SensorNumber %in% c(100, 324) ~ ec2pss(Value/1000, 25),
                                     SensorNumber == 25 ~ (Value - 32)*5/9,
                                     SensorNumber == 25 & Value <40 ~ NA,
                                     TRUE~ Value),
             Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W", "SPC @25C"), 
                              labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity", "Salinity"))) %>%
  filter(Value2 >0, !(SensorNumber ==25 & Value2>26),
         !(SensorNumber ==25 & Value<60),
         !(SensorNumber ==25 & Value>90), 
         !(SensorNumber ==27 & Value2>200), 
         !(SensorNumber ==28 & Value2>50))

#lines for high values
cuttoffs = data.frame(Analyte = c("Salinity", "Chlorophyll", "Temperature", "Turbidity", "Temperature"),
                      cutoff = c(6, 10, 22, 12, 25))

#15-minute data
ggplot(WQx, aes(x = DateTime, y = Value2, color = StationID)) + 
  geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", linetype =2)+
  facet_wrap(~Analyte, scales = "free_y")+
  theme_bw()   #+
#coord_cartesian(xlim = c(ymd_hms("2024-06-01 00:00:00"), now()))


#Calculate daily means
WQmean = WQx %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))


gatedates26 = data.frame(StartDate = c(ymd("2026-07-01")),
                         EndDate = c(today()),
                         Type = c("SMSCG"),
                         xval = c(ymd("2026-07-07")),
                         ynudge =c(0, 0))

yvals = data.frame(Analyte = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                   yval = c(11,8,25,95), yoff = c(0.08, 0.08, 0.015, 0.08))

gatedates2 = cross_join(gatedates26, yvals)

#plot for monthly update #######################################
ggplot(filter(WQmean, Date != today()),
       aes(x = Date, y = Value2, color = StationID)) + 
  geom_rect(data = gatedates26, aes(ymin = -Inf, ymax = Inf,xmin = StartDate, xmax = EndDate,
                                    fill = Type), inherit.aes = FALSE, alpha = 0.4, fill = "grey")+
  geom_hline(data = cuttoffs, aes(yintercept = cutoff), color = "red", 
             linetype =2, linewidth =1)+
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line( linewidth =1)   + theme_bw() +ylab(NULL)



##########plots for Lenny #####################

cuttoffs2 = data.frame(Analyte = c("Salinity", "Temperature", "Temperature", "Temperature"),
                       cutoff = c(6,  22,25, 20),
                       Type = c("Max Good Habitat", "Max Good Habitat", "Lethal", "Offramp"))

lennyWQ = filter(WQmean, StationID %in% c("RVB", "BDL", "GZL"), Analyte %in% c("Salinity", "Temperature")) %>%
  group_by(StationID, Analyte) %>%
  mutate(RollValue = rollmean(Value2, 3, na.pad =T)) %>%
  ungroup()

ggplot(lennyWQ,
       aes(x = Date, y = RollValue, color = StationID)) + 
  geom_rect(data = gatedates26, aes(ymin = -Inf, ymax = Inf,xmin = StartDate, xmax = EndDate),
            inherit.aes = FALSE, alpha = 0.3, fill = "grey")+
  geom_hline(data = cuttoffs2, aes(yintercept = cutoff, linetype = Type), linewidth =1)+
  geom_text(data = cuttoffs2, aes(x = ymd("2026-05-01"), y = cutoff+.1, label = Type), 
            inherit.aes = F, vjust =0, hjust =0)+
  geom_text(data = filter(cuttoffs2, Type == "Max Good Habitat"), 
            aes(x = ymd("2026-07-05"), y = cutoff+.1, label = "SMSCG Operations"), 
            inherit.aes = F, vjust =0, hjust =0, angle = 90)+
  facet_wrap(~Analyte, scales = "free_y")+
  scale_linetype(guide = NULL)+
  scale_color_manual(values = c("salmon", "green3", "blue"), labels = c("Belden's Landing", "Grizzly Bay", "Rio Vista"))+
  geom_line( linewidth =1)   + theme_bw() +ylab("Three Day Rolling Average") +
  xlab(NULL)
