#Plot the DWR forcast and the CNRFC forcast

library(lubridate)
library(tidyverse)
library(rvest)

dwrurl = read_html("https://cdec.water.ca.gov/reportapp/javareports?name=WSI")


#Extract th block of text with the water year type
dwr <- dwrurl %>% 
  html_elements("pre") %>% 
  html_text2() %>% 
  str_split("\r\n") %>% 
  unlist() %>% 
  str_trim()


head = str_locate(dwr,'SACRAMENTO VALLEY WATER YEAR TYPE INDEX') 
headlocal = which(head[,1]== 1)

dwrdf  = data.frame(text = dwr[c(headlocal:(7+headlocal))]) 

dwrtable = read_table(dwrdf[5:8,], col_names = FALSE)
names(dwrtable) = c("Month", "Day", "Year", "p99", "p90", "p75", "p50", "p25", "p10")

dwrtablelong = mutate(dwrtable, Date = mdy(paste(Month, Day, Year))) %>%
  pivot_longer(cols = c(p99:p10), names_to = "Percent", values_to = "WYI") %>%
  mutate(Percentnum = as.numeric(str_remove(Percent, "p")))

ggplot(dwrtablelong, aes(x = Percentnum, y = WYI, color = Month))+
  geom_point()+
  geom_line()+
  ylab("Water Year Index")+
  xlab("Percent Exceedance")

###########################################
#ok, now the CNRFC forcast

# Index =     0.4 * Current Apr-Jul Runoff   (1)
# + 0.3 * Current Oct-Mar Runoff   (1)
# + 0.3 * Previous Year's Index   (2)

cnrfcurl = read_html("https://www.cnrfc.noaa.gov/rawESP.php?id=SACC0")

cnrfc <- cnrfcurl%>% 
  html_elements("pre") %>% 
  html_text2() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_trim()

head = str_locate(cnrfc,'Seasonal Mean') 
headlocal = which(head[,1]== 1)

#this is the april-july forcast
cnrfcdf  = data.frame(text = cnrfc[c((headlocal+1):(2+headlocal))]) 
cnAJ = str_split(cnrfcdf[2,1], " ") %>%
  unlist()

cnAJ = as.numeric(cnAJ[which(!is.na(as.numeric(cnAJ)))])

cnAJforcast = data.frame(AJ = cnAJ, Percentnum = c(90, 75, 50, 25, 10))

cnrfcurl2 = read_html("https://www.cnrfc.noaa.gov/rawESP_WY.php?id=SACC0")

cnrfc2 <- cnrfcurl2%>% 
  html_elements("pre") %>% 
  html_text2() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_trim()


head = str_locate(cnrfc2, "March") 
headlocal = which(head[,1]== 1)

#year to date
ytd = cnrfc2[(headlocal)]
ytd = str_split(ytd, " ") %>%
  unlist()
ytd = as.numeric(ytd[which(!is.na(as.numeric(ytd)))])[3]


#this is the Oct-March forcast
cnrfcdf2  = data.frame(text = cnrfc2[c((headlocal+1):(3+headlocal))]) 
cnOM = str_split(cnrfcdf2[2,1], " ") %>%
  unlist()
cnOM = as.numeric(cnOM[which(!is.na(as.numeric(cnOM)))])

cnOMforcast = data.frame(OM = cnOM+ytd, Percentnum = c(90, 75, 50, 25, 10))

#put them all together and calculate the index
cnrfcall = left_join(cnOMforcast, cnAJforcast) %>%
  mutate(WYI = (0.4*AJ+ 0.3*OM+9350*0.3)/1000,
         forcast = "CNRFC")

ggplot(cnrfcall, aes(x = Percentnum, y = WYI))+ geom_point()+ geom_line()

################################
#join to DWR and compare
dwrtm = filter(dwrtablelong, Month == "Mar") %>%
  select(WYI, Percentnum) %>%
  mutate(forcast = "DWR")

WYIall = bind_rows(cnrfcall, dwrtm)

#add cuttoffs
# Year Type Classification:     Index based on flow in million acre-feet:
#   Wet                      Equal to or greater than 9.2
# Above Normal             Greater than 7.8, and less than 9.2
# Below Normal             Greater than 6.5, and equal to or less than 7.8
# Dry                      Greater than 5.4, and equal to or less than 6.5
# Critical                 Equal to or less than 5.4

cuttoffs = data.frame(YT = c("Critical", "Dry", "Below Normal", "Above Normal"),
                      WYI = c(5.4, 6.5, 7.8, 9.2)) %>%
  mutate(YT = factor(YT, levels = c("Critical", "Dry", "Below Normal", "Above Normal")))

ggplot(WYIall, aes(x = Percentnum, y = WYI, linetype = forcast))+ geom_point()+ geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = WYI, color = YT ))+
  theme_bw()+
  scale_linetype(name = "Forecast Source", labels = c(paste("CNRFC", today()), "DWR 2024-3-1"))+ 
  scale_color_manual(values = c("darkred", "orange", "springgreen4", "blue"), guide = NULL)+
  annotate("text", x = 25, y = c(5.3, 5.6, 6.8, 8, 9.3), 
           label = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))+
  ylab("Water Year Index")+
  xlab("Percent Exceedance Forecast")
