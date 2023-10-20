#Suisun Marsh Salinity Control Gate Action
#Gate operations
#2017-2022

#Nick Rasmussen
#2023-10-18

#packages
library(tidyverse)
library(janitor)
library(lubridate)

#read in data--------------
#gate operations data from EDI
gates <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=11a83e3d943d76e50ed9b0a1de970487") %>% 
  clean_names() %>% 
  glimpse()

#look at date range
range(gates$date) #"1988-11-01" "2022-12-31"

#filter to just the period of interest
gates_recent <- gates %>% 
  #add month column
  mutate(month=month(date),.after=date) %>% 
  filter(date>"2016-12-31" & month > 5 & month <11) %>% 
  glimpse()

#look at date range again
range(gates_recent$date) #"2017-06-01" "2022-10-31" as expected

#look at unique combinations of flash boards, gates, remark, and operating
gate_op_types <- gates_recent %>% 
  distinct(flashboards,gate1,gate2,gate3,operating) %>% 
  arrange(operating, flashboards,gate1,gate2,gate3)
#12 combos

#look at frequency of each operating type
gate_op_freq <-table(gates_recent$operating)
#mostly variations of open, which makes sense for this period
#202 days of operations
#33 days closed

#look at cases of open with one or more gates closed
gate_op_part_cl <- gates_recent %>% 
  filter(operating == "Open with one or more gates closed")

#look at cases of operating with one or more gates closed
gate_op_part_op <- gates_recent %>% 
  filter(operating == "Operating with one or more gates closed")

#look at dates closed; are they sporatic or continuous?
gates_closed <-gates_recent %>% 
  filter(operating == "Closed with flashboards in")
#two periods where gates were malfunctioning or being refurbished
#2021-09-03 to 2021-09-12
#2022-08-03 to 2022-08-25

#look at dates operating
gates_operate <- gates_recent %>% 
  filter(grepl("Operating",operating))
#2018-08-02 to 2018-09-06
#2018-10-17 to 2018-10-31
#2020-09-08 to 2020-09-23
#2020-10-09 to 2020-10-31
#2021-09-01 to 2021-09-02
#2021-09-13 to 2021-10-31
#2022-09-01 to 2022-10-31

#made a file summarizing the start and end dates for operations
#"./Data/gate_operations_2017-2022.csv"