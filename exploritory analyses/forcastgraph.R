#Plot the DWR forcast and the CNRFC forcast

library(lubridate)
library(tidyverse)
library(rvest)

##########################################################
#manual copy-paste download
library(readxl)

forecast2025 = read_excel("Data/forecast.xlsx")

cuttoffs = data.frame(YT = c("Critical", "Dry", "Below Normal", "Above Normal"),
                      WYI = c(5.4, 6.5, 7.8, 9.2)) %>%
  mutate(YT = factor(YT, levels = c("Critical", "Dry", "Below Normal", "Above Normal")))


forec2025 = pivot_longer(forecast2025, cols = c(`0.99`:`0.1`), names_to = "Exceedance", values_to = "Index")

ggplot()+ 
  geom_ribbon(data = forecast2025, aes(x = `Forecast Date`, ymin = `0.9`, ymax = `0.1`), alpha = 0.2, fill = "skyblue")+
  geom_ribbon(data = forecast2025, aes(x = `Forecast Date`, ymin = `0.75`, ymax = `0.25`), alpha = 0.5, fill = "skyblue")+
   geom_line(data = filter(forec2025, Exceedance == 0.5), aes(x = `Forecast Date`, y = Index), size =1)+
geom_hline(data = cuttoffs, aes(yintercept = WYI, linetype = YT))+
  annotate("text", x = ymd_hm("2025-01-01 00:00"), y = c(5.3, 5.6, 6.8, 8, 9.3), 
           label = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))+
  ylab("Water Year Index")+
  xlab("Date - 2025")+
  theme_bw()+
  theme(legend.position = "none")

ggsave("plots/WYI2025.png", device = "png", width =8, height =6)



#############################################################
#direct frkom the interenet
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

dwrdf  = data.frame(text = dwr[c(headlocal:(9+headlocal))]) 

dwrtable = read_table(dwrdf[5:10,], col_names = FALSE)
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

head = str_locate(cnrfc,'Seasonal') 
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


head = str_locate(cnrfc2, "February") 
headlocal = which(head[,1]== 1)

#year to date
ytd = cnrfc2[(headlocal)]
ytd = str_split(ytd, " ") %>%
  unlist()
ytd = as.numeric(ytd[which(!is.na(as.numeric(ytd)))])[3]


#this is the Oct-March forcast
cnrfcdf2  = data.frame(text = cnrfc2[c((headlocal+1):(5+headlocal))]) 
cnOM = str_split(cnrfcdf2[5,1], " ") %>%
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
dwrtm = filter(dwrtablelong, Month == "May") %>%
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
  scale_linetype(name = "Forecast Source", labels = c(paste("CNRFC", today()), "DWR 2024-5-1"))+ 
  scale_color_manual(values = c("darkred", "orange", "springgreen4", "blue"), guide = NULL)+
  annotate("text", x = 25, y = c(5.3, 5.6, 6.8, 8, 9.3), 
           label = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))+
  ylab("Water Year Index")+
  xlab("Percent Exceedance Forecast")

################################################################
#add the B120 update to the plot 

b120update <- read_html("https://cdec.water.ca.gov/reportapp/javareports?name=B120DIST")

extract_td_row <- function(min_element, max_element) {
  col_names <- c(
    "Watershed",
    "Oct_thru_Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "WY_total",
    "80_perc_range_lower",
    "80_perc_range_upper",
    "WY_perc_avg"
  )
  
  html_elements(b120update, "td")[seq.int(min_element, max_element, 1)] %>%
    html_text2() %>% 
    set_names(col_names) %>% 
    as_tibble_row()
}

create_index_seq <- function(i) {
  c(min_seq = 14 * i + 1, max_seq = 14 * (i + 1))
}

b120table <- map(0:15, create_index_seq) %>% 
  enframe() %>% 
  unnest_wider(value) %>% 
  mutate(df_data = map2(min_seq, max_seq, extract_td_row)) %>% 
  unnest_wider(df_data) %>% 
  select(-c(name, min_seq, max_seq))

b120t = b120table %>%
  mutate(across(Oct_thru_Jan:last_col(), function(x) as.numeric(str_remove(x, ","))))

# It’s a bit manual, but by adding up rows 2 to 5, across columns 1 to 3,  - Does he mean 3 to 6?
# you can get the median October through March. Then by adding the same 
# rows over columns 4 to 7, you get the median April through July. Then 
# I sort of take a leap that all the uncertainty of the WY 80% probability 
# range is coming from the April through July, and I apply those offsets to the AJ 
# value. This is correct for updates in April and May, but not quite for updates in February and March.
# Anyway, this gives an estimate of the 10%, 50%, and 90% exceedance forecasts:
  
OctMar = sum(b120t[3:6,2:4])
AprJul = sum(b120t[3:6,5:8])
CI =c(sum(b120t[3:6,11])- sum(b120t[3:6,12]), sum(b120t[3:6,13])- sum(b120t[3:6,12]))

b120svi = 9350*0.3+OctMar*0.3 + 0.4*AprJul
b120sviL = 9350*0.3+OctMar*0.3 + 0.4*(AprJul-CI[1])
b120sviU = 9350*0.3+OctMar*0.3 + 0.4*(AprJul-CI[2])

b120df = data.frame(Month = month(today()), Day = day(today()), Year = 2024, Percentnum = c(10,50,90),
                    WYI = c(b120sviL, b120svi , b120sviU)/1000, Source = "B120")

WYIall = bind_rows(cnrfcall, dwrtm, b120df)

ggplot(WYIall, aes(x = Percentnum, y = WYI, linetype = forcast))+ geom_point()+ geom_line()+
  geom_hline(data = cuttoffs, aes(yintercept = WYI, color = YT ))+
  theme_bw()+
  scale_linetype(name = "Forecast Source", labels = c(paste("CNRFC", today()), "DWR 2024-5-1", "B120"))+ 
  scale_color_manual(values = c("darkred", "orange", "springgreen4", "blue"), guide = NULL)+
  annotate("text", x = 25, y = c(5.3, 5.6, 6.8, 8, 9.3), 
           label = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))+
  ylab("Water Year Index")+
  xlab("Percent Exceedance Forecast")

#ok, that's not working. Not sure what he did there. 