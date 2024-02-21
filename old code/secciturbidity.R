#Secchi versus turbidity

library(readxl)
library(tidyverse)

library(readxl)
FMWT<- read_excel("Data/FMWT 1967-2018 Catch Matrix_updated.xlsx", 
                                                  sheet = "FlatFile", guess_max = 20000)
str(FMWT)

turb = select(FMWT, Year, Date, Survey, Station, `Turbidity (NTUs)`, `Secchi (m)`) %>%
  rename(turbidity = `Turbidity (NTUs)`, Secchi = `Secchi (m)`) %>%
  filter(!is.na(turbidity), !is.na(Secchi))

ggplot(turb, aes(x= turbidity, y = Secchi)) + 
  geom_point() +
  geom_smooth()

ggplot(turb, aes(x= log(turbidity), y = Secchi)) + 
  geom_point() +
  geom_smooth()


write.csv(turb, "FMWTturbidity.csv")

