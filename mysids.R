#now look at the mysid and amphipod data


library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)

#import the data
mysids = read_excel("SMSCG MysidAmphipodCPUEJuly2019.xlsx", sheet = "Mysid CPUE")
Amps = read_excel("SMSCG MysidAmphipodCPUEJuly2019.xlsx", sheet = "AmphipodCPUE")

str(mysids)
str(Amps)

bugs = merge(mysids, Amps)
bugs2 = bugs[,c(1,4,5,8,10,24:31, 39:48)]
bugs2[,9] = NULL


#Now filter it so it's just the stations and time period we are interested in
stas = data.frame(Station = as.character(c(513, 520, 801, 802, 606, 609, 610, "MONT", "NZ032")), 
                  Region = c(rep("River", 4), rep("Suisun Marsh", 5)))

bugs2 = merge(bugs2, stas, by = "Station")

bugs3 = mutate(bugs2, Other = `Unidentified Amphipod` + `Unidentified Corophium` + `Unidentified Gammarus` + `Unidentified Mysid`)
bugs3 = bugs3[,c(1:3, 6:11, 13:19, 23:24)]
bugs3$sample = c(1:nrow(bugs3))

#go from wide to long for a quick plot
bugslong = gather(bugs3, key = "Species", value = "CPUE", -Year, -SampleDate, -Station, -Region.y, -sample)
bugslong$Month = factor(month(bugslong$SampleDate), labels = c("Jul", "Aug","Sep", "Oct"))

bugslong$Species = factor(bugslong$Species, levels = c("Acanthomysis aspera", 
                                                       "Acanthomysis hwanhaiensis", "Deltamysis holmquistae",
                                                       "Neomysis kadiakensis", "Neomysis mercedis",
                                                       "Hyperacanthomysis longirostris",
                                                       "Americorophium spinicorne", "Americorophium stimpsoni",  
                                                       "Ampelisca abdita",               
                                                        "Corophium alienense", "Crangonyx sp_", "Gammarus daiberi",              
                                                        "Hyalella sp_", "Other"))

mypal = c(brewer.pal(12, "Set3"), brewer.pal(5, "Set2"))

bugssum = group_by(bugslong, Month, Region.y, Species) %>% summarize(total = mean(CPUE)) 

ggplot(data = bugssum, aes(x=Month, y = total)) + geom_bar(stat = "identity",aes(fill = Species)) +
  scale_fill_manual(values = mypal) + facet_wrap(~Region.y)

bugstots = group_by(bugslong, Month, Region.y, sample) %>% summarize(total = sum(CPUE))

ggplot(data = bugstots, aes(x=Month, y = total)) + geom_boxplot() +facet_wrap(~Region.y)
