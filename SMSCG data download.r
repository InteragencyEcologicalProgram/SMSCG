#                                                                   #
#####################################################################
#                                                                   #
# This R script retrieves data from the WQP Marsh and EMP Programs. #
#                                                                   #
#####################################################################

library(tidyverse)
library(lubridate)
library(wqpr)


# source data definitions

## Suisun Marsh
sm.stations = c(
  "CSE", # Collinsville
  "MSL", # Montezuma Slough at Roaring River
  "NSL", # National Steel
  "HUN", # Hunter Cut
#  "MSM", # Montezuma Slough Mouth
  "TRB"  # Tule Red Breach
)
sm.analytes = c(
  "Specific Conductance",
  "Temperature",
  "Turbidity"
)
sm.type = "Time Series"


## CEMP
cemp.stations = c(
  "GZL", # Grizzly Bay
  "GZB"  # Grizzly Bay Buoy
)
cemp.analytes = c(
  "SpC",          # Specific Conductance in EMP
  "Temperature",
  "Turbidity"
)
cemp.type = "Data"

# Time Window
from.time = as_datetime("2020-01-01", tz = wqp_tz())
to.time = as_datetime("2021-01-01", tz = wqp_tz())

# Get Data
sm.data = wqp_result_details(program = "marsh") %>%
  filter(
    cdec_code %in% sm.stations,
	analyte_name %in% sm.analytes,
	reading_type_name == sm.type
  ) %>%
  transmute(
    cdec_code, analyte_name, unit_name,  
    data = wqp_result_data(result_id, from.time, to.time,
      program = "marsh")
  ) %>%
  unnest(data) 
cemp.data = wqp_result_details(program = "emp") %>%
  filter(
    cdec_code %in% cemp.stations,
	analyte_name %in% cemp.analytes,
	reading_type_name == cemp.type
  ) %>%
  transmute(
    cdec_code, analyte_name, unit_name,  
    data = wqp_result_data(result_id, from.time, to.time,
      program = "emp")
  ) %>%
  unnest(data)

# Cleanup
all.data = bind_rows(sm.data, cemp.data) %>%
  select(-row, -version, -result_id)%>%
  mutate(analyte_name = str_replace(analyte_name, "SpC",
    "Specific Conductance"))

# QAQC Checks, if any


#ggplot(all.data) +
#  aes(x = time, y = value) +
#  geom_line() +
#  facet_grid(analyte_name ~ cdec_code, scales = "free_y")
