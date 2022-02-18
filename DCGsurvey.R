library(readxl)
library(tidyverse)

Q1 = read_excel("DCG survey.xlsx", sheet = "Q1")
Q1x = pivot_longer(Q1, cols = -ID, names_to = "Metric", values_to = "Uncertainty")

Q1m = group_by(Q1x, Metric) %>%
  summarize(Uncertainty = mean(Uncertainty, na.rm = T)) %>%
  arrange(Uncertainty) %>%
  mutate(Metric = factor(Metric, levels = unique(Metric)))

Q1x = mutate(Q1x, Metric = factor(Metric, levels = levels(Q1m$Metric)))

ggplot(Q1x, aes(x = Metric, y = Uncertainty)) + geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab("Confidence in prediction")

ggplot(Q1x, aes(x = as.factor(Uncertainty), fill = as.factor(Uncertainty))) +
  geom_histogram(stat = "count")+
  scale_fill_viridis_d(guide = NULL)+
  facet_wrap(~Metric)+
  theme_bw()

Q2 = read_excel("DCG survey.xlsx", sheet = "Q2")
Q2x = pivot_longer(Q2, cols = -ID, names_to = "Action", values_to = "Usefulness")

ggplot(Q2x, aes(x = Action, y = Usefulness)) + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme_bw()

Q2m = group_by(Q2x, Action) %>%
  summarize(Usefulness = mean(Usefulness, na.rm = T)) %>%
  arrange(Usefulness) %>%
  mutate(Action = factor(Action, levels = unique(Action)))

Q2x = mutate(Q2x, Action = factor(Action, levels = levels(Q2m$Action)))


ggplot(Q2x, aes(x = Action, y = Usefulness)) + geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(Q2x, aes(x = as.factor(Usefulness), fill = as.factor(Usefulness))) +
  geom_histogram(stat = "count")+
  scale_fill_viridis_d(guide = NULL)+
  facet_wrap(~Action)+
  theme_bw()
