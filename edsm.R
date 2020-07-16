#fix ted's edsm data
library(readxl)
library(tidyverse)

edsmcatch = read_excel("EDSMcatch2018.xlsx", sheet = "catch")

edsmcatch2 = pivot_longer(edsmcatch, cols = c("Suisun Bay":"Lower San Joaquin"), 
                          names_to = "region", values_to = "catch")

edsmeffort = read_excel("EDSMcatch2018.xlsx", sheet = "effort")
edsmeffort2 = pivot_longer(edsmeffort, cols = c("Suisun Bay":"Lower San Joaquin"), 
                          names_to = "region", values_to = "Trawls")

edsmdata = merge(edsmcatch2, edsmeffort2)
edsmdata = rename(edsmdata, EndWeek = `End Of Week`)
edsmdata$catch[which(is.na(edsmdata$catch))] = 0
write.csv(edsmdata, "EDSMsmelt_summer_2018.csv")
