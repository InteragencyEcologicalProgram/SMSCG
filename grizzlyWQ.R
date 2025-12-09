#grizzly bay data

library(tidyverse)
  library(wql)

grizzwq = read_csv("Data/SMSCG_wq_data_2017-2024_final.csv") %>%
  filter(station %in% c("GZB", "GZM", "TRB", "GZL"))

grizzwq = mutate(grizzwq, Salinity = ec2pss(spc/1000, 25), Date = date(date_time_pst))

ggplot(grizzwq, aes(x = date_time_pst, y = Salinity, color = station))+ geom_line()+
  coord_cartesian(xlim = c(ymd_hm("2023-01-01 00:00"), ymd_hm("2023-12-31 23:00")),
                  ylim = c(0, 12.6))+ xlab("Date")+ theme_bw()

AllStationsDate = complete

grizzwq2 = group_by(grizzwq, Date, station) %>%
  summarize(Salinity = mean(Salinity, na.rm =T), Turbidity = mean(turbidity, na.rm =T), 
            Temperature = mean(watertemperature, na.rm =T)) %>%
  pivot_longer(cols = c(Salinity, Turbidity, Temperature), names_to = "Parameter", values_to = "Value") %>%
  filter(Date %in% seq(ymd("2022-10-01"),ymd("2023-09-30"))) %>%
  ungroup() %>%
  complete(station, Date, Parameter)
  

ggplot(grizzwq2, aes(x = Date, y = Value, color = station))+ geom_line(linewidth =.75)+
   xlab("Date")+ theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~Parameter, nrow =3, scales = "free_y") + theme(legend.position = "bottom") +
  ylab("Turbidity (FNU)          Temperature (degrees C)             Salinity PSU")

grizzwq2_2024 = group_by(grizzwq, Date, station) %>%
  summarize(Salinity = mean(Salinity, na.rm =T), Turbidity = mean(turbidity, na.rm =T), 
            Temperature = mean(watertemperature, na.rm =T)) %>%
  pivot_longer(cols = c(Salinity, Turbidity, Temperature), names_to = "Parameter", values_to = "Value") %>%
  filter(Date %in% seq(ymd("2023-10-01"),ymd("2024-09-30"))) %>%
  ungroup() %>%
  complete(station, Date, Parameter)


ggplot(grizzwq2_2024, aes(x = Date, y = Value, color = station))+ geom_line(linewidth =.75)+
  xlab("Date")+ theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~Parameter, nrow =3, scales = "free_y") + theme(legend.position = "bottom") +
  ylab("Turbidity (FNU)          Temperature (degrees C)             Salinity PSU")


GZM = filter(grizzwq, Date %in% seq(ymd("2023-01-01"),ymd("2023-12-31")), station == "GZM")
