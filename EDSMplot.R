#Plot of EDSM's annual summer fall population number

library(tidyverse)

smelt = read_csv("Data/DSM_aggregated_index_EDSM_Jul-Sep.csv")

smelt = mutate(smelt, ymin = Mean_of_nHats-SE_of_mean_of_nHats+1, ymax =  Mean_of_nHats+SE_of_mean_of_nHats)

ggplot(smelt, aes(x = Year, y = Mean_of_nHats)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.6)+
  scale_y_log10()+
  ylab("Mean Jul-Sep Delta Smelt \nAbundance Estimate")+
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023),
                     minor_breaks = NULL)+
  theme_bw()

ggsave("Plots/AnnualSmeltPlot.tiff", device = "tiff", width =5, height =5)
