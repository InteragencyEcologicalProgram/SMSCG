#discrete water quality

library(readxl)
library(tidyverse)

ICFwq = read_excel("Data/ICF_WQ_2022-11-07.xlsx", na = "NA")

ICFwq2022 = mutate(ICFwq, Year = year(date), Month = month(date)) %>%
  filter(Year == 2022, Month %in% c(6, 7,8,9, 10)) %>%
  mutate(Month = factor(Month, levels = c(6, 7,8,9,10), labels = c("Jun", "Jul", "Aug", "Sep", "Oct")),
         region = factor(region, levels = c("Suisun Marsh", "Suisun Bay","Lower Sacramento", "Lower San Joaquin",
                                            "Upper Sacramento River", "Cache Slough and Liberty Island",
                                             "Sacramento Deep Water Ship Channel",
                                             "Western"),
                         labels = c("Suisun Marsh", "Suisun Bay", "Lower Sac", "Lower SJ", 
                                    "Upper Sac", "Cache/Liberty", "SDWSC", "Western")))

ggplot(ICFwq2022, aes(x = region, y = chla_fluor, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Chlorophyll-a (ug/L)")+ xlab(NULL)

  ICFwq2022 %>%
    group_by(region) %>%
    summarize(MaxCHL = max(chla_fluor, na.rm = t), MeanCHL = mean(chla_fluor, na.rm = T))

ggplot(ICFwq2022, aes(x = region, y = turbidity, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Turbidity (NTU)")+ xlab(NULL)


ICFwq2022 %>%
  group_by(region) %>%
  summarize(Maxturb = max(turbidity, na.rm = t), MeanCHL = mean(turbidity, na.rm = T))


ggplot(ICFwq2022, aes(x = region, y = secchi_depth, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Secchi cm")+ xlab(NULL)




ggplot(ICFwq2022, aes(x = region, y = temperature, fill = region)) + geom_boxplot()+
  facet_wrap(~Month, nrow = 1) +
  scale_fill_discrete(guide = NULL)+
  geom_hline(yintercept = 23.9, color = "red", linetype = 2)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))+
  ylab("Temperature")+ xlab(NULL)

