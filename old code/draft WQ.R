#draft figure for 2024 report

library(tidyverse)
library(cder)
library(wql)
load("data/df_habitat_year.RData")

#take the cdec data, pivot it wider, add smelt thresholds
df_habitat_2024 = WQmeanally %>%
  pivot_wider(id_cols = c(Date, StationID, region), names_from = "Analyte", values_from = "Value2",
              values_fn = mean) %>%
  mutate(turbcut = case_when(Turbidity >= 12 ~1, TRUE~0), salcut = case_when(Salinity<=6 ~1, TRUE~0), 
         tempcut = case_when(Temperature<=22 ~1, TRUE~0),
         year = year(Date),
         good4smelt = case_when(turbcut ==1 & salcut==1 &tempcut==1 ~1,
                                TRUE ~0)) %>%
  rename(date = Date)
  
  
#group by region and calculate total days  
df_limiter_2024 <- df_habitat_2024 %>%
  group_by(date, year, region) %>%
  summarize(good_smelt_habitat= max(good4smelt), turbmax = max(turbcut), 
            tempmax = max(tempcut),  salmax = max(salcut))

df_limitor_2024 <- df_limiter_2024 %>%
  group_by(year, region) %>%
  summarize(good_smelt_habitat_days= sum(good_smelt_habitat),
            turbdays = sum(turbmax), tempdays = sum(tempmax), saldays = sum(salmax))

#bind it to the older data
df_limitor = bind_rows(df_limitor_2024, df_limitor_region)

#pivot longer
df_limitor_rlong = pivot_longer(df_limitor, cols = c(good_smelt_habitat_days, turbdays, tempdays, saldays),
                                names_to = "Metric", values_to = "Days") %>%
  mutate(Region = case_when(region == "Bay" ~ "Suisun Bay",
                            region == "Marsh" ~ "Suisun Marsh",
                            region == "River" ~ "Sacramento River",
                            TRUE ~ region),
         Metric = factor(Metric, levels = c("saldays", "tempdays", "turbdays", "good_smelt_habitat_days"),
                         labels = c("Salinity <6", "Temperature <22", "Turbidity >12", "Combined")))

#plot for dec 9 dcg presentation

#this is the graph that will go into the report
limitor_region_plot <- ggplot(df_limitor_rlong, aes(x = year, y = Days, fill = Region))+
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024), 
                     labels = c("2017\n(W)", "2018\n(BN)", "2019\n(W)", "2020\n(D)", "2021\n(CD)", 
                                "2022\n(CD)", "2023\n(W)", "2024\n(AN)")) +
  facet_wrap(~Metric, nrow = 4)+
  geom_col(position = "dodge") +
  #scale_fill_brewer(palette = "Set2", name=NULL, labels = c("Combined", "Salinity","Temperature", "Turbidity")) +
  labs(x = "Year") +
  labs(y = "Number of Suitable Habitat Days")+ theme_bw()
plot(limitor_region_plot)

ggsave(filename = "Plots/limitor_regionPlot.tiff", device = "tiff", width =8, height = 6)

####################################
#quick qc compring 2023 and 2024


WQ2 = cdec_query(c("GOD", "GZB", "GZM", "HON", "MAL", "MSL", "RYC", "SSI",
                    "GZL", "BDL", "NSL", "RVB",  "HUN", "CSE"), sensors = c(100, 25, 27, 28),
                 start.date = as.Date("2023-06-01"), end.date = "2023-11-01")
WQ2b = mutate(WQ2, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                                      SensorNumber == 25 ~ (Value - 32)*5/9,
                                      SensorNumber == 25 & Value >30 ~ NA,
                                      TRUE~ Value),
              Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W"), 
                               labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity"))) %>%
  filter(Value2 >0, !(SensorNumber ==25 & Value2>26), !(SensorNumber ==27 & Value2>200), 
         !(SensorNumber ==28 & Value2>20))

WQmean = WQ2b %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType, Analyte) %>%
  summarize(Value = mean(Value, na.rm = T), Value2 = mean(Value2, na.rm = T))


stations = read_csv("Data/station_data.csv")

WQmeanallx2023 = left_join(WQmean, stations, by = c("StationID"="station"))

WQmeanally2023 = mutate(WQmeanallx2023, region = factor(region, levels = c("Bay", "Marsh", "River"), 
                                                labels = c("Suisun Bay", "Suisun Marsh", "Sacramento River")),
                    Analyte2 = factor(Analyte, levels = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                                      labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU")))
cuttoffs$Analyte2 = factor(cuttoffs$Analyte, levels = c("Chlorophyll", "Salinity", "Temperature", "Turbidity"),
                           labels = c("Chlorophyll ug/L", "Salinity PSU", "Temperature C", "Turbidity FNU"))


df_habitat_2023 = WQmeanally2023 %>%
  pivot_wider(id_cols = c(Date, StationID, region), names_from = "Analyte", values_from = "Value2",
              values_fn = mean) %>%
  mutate(turbcut = case_when(Turbidity >= 12 ~1, TRUE~0), salcut = case_when(Salinity<=6 ~1, TRUE~0), 
         tempcut = case_when(Temperature<=22 ~1, TRUE~0),
         year = year(Date),
         good4smelt = case_when(turbcut ==1 & salcut==1 &tempcut==1 ~1,
                                TRUE ~0)) %>%
  rename(date = Date)


#group by region and calculate total days  
df_limiter_2023 <- df_habitat_2023 %>%
  group_by(date, year, region) %>%
  summarize(good_smelt_habitat= max(good4smelt), turbmax = max(turbcut), 
            tempmax = max(tempcut),  salmax = max(salcut))

df_limitor_2023 <- df_limiter_2023 %>%
  group_by(year, region) %>%
  summarize(good_smelt_habitat_days= sum(good_smelt_habitat),
            turbdays = sum(turbmax), tempdays = sum(tempmax), saldays = sum(salmax))

foo2023 = filter(df_habitat_2023, Temperature > 22)
foo2024 = filter(df_habitat_2024, Temperature > 22)


wq2324 = bind_rows(WQmeanally, WQmeanally2023) %>%
  mutate(DOY = yday(Date), Year = year(Date))

ggplot(filter(wq2324, StationID == "BDL", Analyte == "Temperature"), 
       aes(x = DOY, y = Value2, color = as.factor(Year)))+
  geom_line()+ylab("Temperature")+
  geom_hline(yintercept = 22)
