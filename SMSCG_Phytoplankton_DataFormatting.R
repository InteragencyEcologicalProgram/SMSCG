#Suisun Marsh Salinity Control Gate
#Phytoplankton Data 2020-2021
#Format raw data in preparation for visualization and analysis
#Biovolume calculations have been corrected

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(hms) #working with date/time
library(lubridate) #working with dates
library(readxl) #importing data from excel files
#library(algaeClassify) #grab taxonomy info from AlgaeBase; doesn't work currently

#Notes
#For all BSA files from 2013 to 2021, the column "Number of cells per unit" really means "Total cells", 
#which is the total number of cells counted for that taxon in a particular sample
#calculations in this script were corrected accordingly on 2/10/2022

#create version of data set for phytoplankton synthesis effort
#just needs to include the samples specific to the SMSCG action
#hard to split the SMSCG and EMP data sets because NZS42 and NZ032 station names are
#shared by the two surveys
#probably should add a column to the imported data sets with the source file
#that will allow us to keep track of which samples are from which survey

#to do list
#should check to see if organisms per ml and cells per ml are equal for cases where phyto_form = individual

# Read in and combine the EMP data----------------------------------------------


#Create character vectors of all 2020 EMP phytoplankton files (n = 5) 
phyto_emp_20 <- dir(path = "Data/phytoplankton/2020", pattern = "EMP", full.names = T)

#Combine all of the 2020 emp sample data files into a single df
#specify format of columns because column types not automatically read consistently among files
column_type20<-c(rep("text",4),rep("numeric",10),rep("text",5),rep("numeric",5),rep("text",5)
               ,rep("numeric",10),"text",rep("numeric",26)) 

phytoplankton_emp_20 <- phyto_emp_20 %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  map_dfr(~read_excel(.x, col_types = column_type20), .id = "source") %>% 
  #reduce file name to just the needed info (ie, survey)
  mutate(collected_by = as.factor(str_sub(source,25,27))) %>% 
  glimpse()
#succeeded in combining all the sample files
#but date and time are in weird format
#also the sampling depth column has three variations: "Depth (m)", "Depth (ft.)", "Depth (ft)"
#so when the files are combined, there are two extra depth columns added


#Create character vectors of all 2021 EMP phytoplankton files (n = 5) and then for the DFW file
#doing this separately from 2020 because there's an extra column in these files
#the "Full Code" column is the FLIMS code; relevant to EMP samples but not DFW samples
phyto_emp_21 <- dir(path = "Data/phytoplankton/2021", pattern = "EMP", full.names = T)

#Combine all of the 2021 sample data files into a single df
#specify format of columns because column types not automatically read consistently among files
#accounts for extra column in 2021 files
column_type21<-c(rep("text",5),rep("numeric",10),rep("text",5),rep("numeric",5),rep("text",5)
                 ,rep("numeric",10),"text",rep("numeric",26)) 

phytoplankton_emp_21 <- phyto_emp_21 %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  map_dfr(~read_excel(.x, col_types = column_type21), .id = "source") %>% 
  #reduce file name to just the needed info (ie, survey)
  mutate(collected_by = as.factor(str_sub(source,25,27))) %>% 
  glimpse()
#succeeded in combining all the sample files
#but date and time are in weird format

#combine the 2020 and 2021 EMP data sets
#bind_rows can handle the fact that not all columns will match between data sets
phytoplankton_emp <- bind_rows(phytoplankton_emp_20,phytoplankton_emp_21) %>% 
  clean_names() %>% 
  glimpse()
#the three non-matching columns get kicked to the end of the combined df


# Read in and combine the DFW data----------------------------------------------

#Create character vectors of all 2020 DFW phytoplankton files 
phyto_dfw_20 <- dir(path = "Data/phytoplankton/2020", pattern = "DFW", full.names = T)

phytoplankton_dfw_20 <- phyto_dfw_20 %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  map_dfr(~read_excel(.x, col_types = column_type20), .id = "source") %>% 
  glimpse()

#Create character vectors of all 2020 EMP phytoplankton files (n = 5) and then for the DFW file
#doing this separately from 2020 because there's an extra column in these files
#the "Full Code" column is the FLIMS code; relevant to EMP samples but not DFW samples
phyto_dfw_21 <- dir(path = "Data/phytoplankton/2021", pattern = "DFW", full.names = T)

phytoplankton_dfw_21 <- phyto_dfw_21 %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  map_dfr(~read_excel(.x, col_types = column_type21), .id = "source") %>% 
  glimpse()

#combine the 2020 and 2021 EMP data sets
#bind_rows can handle the fact that not all columns will match between data sets
phytoplankton_dfw <- bind_rows(phytoplankton_dfw_20,phytoplankton_dfw_21) %>% 
  clean_names() %>% 
  glimpse()


# Read in the other files----------------

#read in taxonomy data
#this probably needs to be updated with each new batch of data
#update this file with the updates/corrections I got from AlgaeBase 2/24/2022
taxonomy <- read_excel(path = "Data/phytoplankton/PhytoplanktonTaxonomy_2022-02-09.xlsx")

#read in station name info
#includes region categories, station names, and names that identify comparable stations through time
stations <- read_csv("Data/phytoplankton/stations.csv")


#clean up EMP station names and drop unneeded stations-------------

#look at stations in the data set
#unique(phytoplankton_emp$station_code)

phyto_emp_stations <- phytoplankton_emp %>% 
  #remove empty rows created by linear cell measurement rows (length, width, depth)  
  #a little tricky just because the survey name appears in every row including the othewise empty ones
  #chose the taxon column as the ones to check for missing data
  drop_na(taxon) %>%
  mutate(
    #format date
    date = as.Date(as.numeric(sample_date),origin = "1899-12-30")
    #format time and specify that time zone is PST
    ,time = as_hms(as.numeric(sample_time)*60*60*24)
    #create a date time colum
    ,date_time_PST = ymd_hms(as.character(paste(date, time)),tz="Etc/GMT+8")
    #create a month column
    ,month = as.numeric(month(date))
    #correct two typos in station names
    ,'station_corr' = case_when(grepl("E26", station_code) ~ "EZ6"
                               ,grepl("NZ542", station_code) ~"NZS42"
         ,TRUE ~ as.character(station_code)
                  ))  %>% 
  #add column that will be prefix to station names
  add_column(station_pre = "EMP") %>% 
  #add prefix to station names
  unite('station', c(station_pre,station_corr),sep="_",remove=F) %>% 
  #drop the June samples
  filter(month!=6) %>% 
  #drop one sample that was submitted to BSA empty
  filter(!(station=="EMP_EZ6" & date=="2020-09-10")) %>% 
  glimpse()
#NOTE: there are 9 rows that don't format date-time
#it's for a sample from a station I don't need
#the time is missing for that sample

#check time zone
#tz(phyto_emp_stations$date_time_PST)

#look at station names again
#unique(phyto_emp_stations$station)

#filter the data set to just the stations needed for SMSCG  
phyto_emp <- inner_join(phyto_emp_stations, stations)

#make sure the right stations were retained
#unique(phyto_emp$station)
#looks good


#clean up DFW station names-------------

#look at stations in the data set
#unique(phytoplankton_dfw$station_code)

phyto_dfw_stations <- phytoplankton_dfw %>% 
  #remove empty rows created by linear cell measurement rows (length, width, depth)  
  #a little tricky just because the survey name appears in every row including the othewise empty ones
  #chose the taxon column as the ones to check for missing data
  drop_na(taxon) %>% 
  mutate(
    #format date
    date = as.Date(as.numeric(sample_date),origin = "1899-12-30")
    #format time
    ,time = as_hms(as.numeric(sample_time)*60*60*24)
    #create a date time column; DFW records time in PDT
    ,date_time_PDT = ymd_hms(as.character(paste(date, time)),tz="America/Los_Angeles")
    #change PDT to PST to match the EMP times
    ,date_time_PST = with_tz(date_time_PDT,tzone="Etc/GMT+8")
    #create a month column
    , month = as.numeric(month(date))
    #add column that indicates which survey collected samples
    ,'collected_by' = case_when(
      station_code == "GZB" ~ "EMP"
      ,month < 9 ~ "STN"
      ,month > 8 ~ "FMWT")
    #fix some station names
    ,'station' = case_when(
      #River stations
      grepl("704", station_code) & month < 9 ~ "STN_704"
      ,grepl("704", station_code) & month > 8 ~ "FMWT_704"
      ,grepl("706", station_code) & month < 9 ~ "STN_706"
      ,grepl("706", station_code) & month > 8 ~ "FMWT_706"
      ,grepl("801", station_code) ~ "STN_801"
      ,grepl("802", station_code) ~ "FMWT_802"
      #East Marsh stations
      ,grepl("MON", station_code) ~ "MONT"
      ,grepl("609", station_code) ~ "STN_609"
      ,grepl("610", station_code) ~ "STN_610"
      #West Marsh stations
      ,grepl("605", station_code) ~ "FMWT_605"
      ,grepl("606", station_code) & month < 9 ~ "STN_606"
      ,grepl("606", station_code) & month > 8 ~ "FMWT_606"
      ,grepl("NZS42", station_code) ~ "EMP_NZS42"
      #Bay stations
      #note that GZB was only during 2021 and 519 station data starts in 2022
      ,grepl("519", station_code) & month < 9 ~ "STN_519"
      ,grepl("519", station_code) & month > 8 ~ "FMWT_519"
      ,grepl("602", station_code) & month < 9 ~ "STN_602"
      ,grepl("602", station_code) & month > 8 ~ "FMWT_602"
      ,TRUE ~ as.character(station_code)            
    )) %>%
  glimpse()
#4 rows for date time didn't parse because no time recorded for sample

#make sure conversion of DFW time from PDT to PST worked
#tz_check <- phyto_dfw_stations %>%
#  select(date_time_PDT,date_time_PST) %>% 
#  mutate(time_dif = ymd_hms(date_time_PDT) - ymd_hms(date_time_PST)) %>% 
#  glimpse()
#looks good

#look at NAs for date time
#time_nas <- phyto_dfw_stations %>% 
 # select(station_code,date,time,date_time_PDT,date_time_PST) %>% 
  #filter(is.na(date_time_PDT))

#check time zone
#tz(phyto_dfw_stations$date_time_PST)

#add the region and combo station data 
phyto_dfw <- left_join(phyto_dfw_stations, stations)

phyto_dfw_combo <- phyto_dfw %>% 
  distinct(region, station, month, collected_by) %>% 
  arrange(month, station, collected_by)


#format the sample data set------------
#NOTE: based on changes made above, need to update the column names retained below
#ie, station

#combine EMP and DFW sample data
phytoplankton <- bind_rows(phyto_emp,phyto_dfw) %>% 
  glimpse()

phyto_cleanest <- phytoplankton %>% 
  #rename the confusingly incorrectly name column
  rename(total_cells=number_of_cells_per_unit) %>% 
  #subset to just the needed columns
  select(collected_by
         , region
         , station
         , station_comb
         , date
         , date_time_PST
         , genus
         , taxon
         , colony_filament_individual_group_code
         , unit_abundance
         , slide_chamber_area_mm2
         , volume_analyzed_m_l
         , field_of_view_mm2
         , number_of_fields_counted
         , factor
         , total_cells
         , biovolume_1:biovolume_10) %>%     
  rowwise() %>% 
  mutate(  
    #use the date-time column with standardized time zone to extract time
    time = as_hms(date_time_PST)
    #create new column that calculates mean biovolume per cell
    ,mean_cell_biovolume = mean(c_across(biovolume_1:biovolume_10),na.rm=T)
    #create new column that calculates organisms per mL; round number to nearest tenth
    #different from cells per mL because some organisms are multicellular
    ,organisms_per_ml = round((unit_abundance*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,organisms_per_ml_easy = (unit_abundance*factor)
    #create new column that calculates cells per mL; round number to nearest tenth
    ,cells_per_ml = round((total_cells*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,cells_per_ml_easy = (total_cells*factor)
    #create a column that calculates biovolume per mL
    #units for biovolume are cubic microns; old version is incorrect calculations; round number to nearest tenth
    #,biovolume_per_ml_old = organisms_per_ml * total_cells * mean_cell_biovolume
    ,biovolume_per_ml = round((total_cells* mean_cell_biovolume*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,biovolume_per_ml_easy = factor * total_cells * mean_cell_biovolume
    ) %>% 
  #simplify column names
  rename(phyto_form =  colony_filament_individual_group_code) %>% 
  #subset and reorder columns again to just those needed
  select(collected_by
         ,region
         ,station
         ,station_comb
         ,date
         ,time
         ,genus
         ,taxon                              
         ,phyto_form           
         ,organisms_per_ml
         #,organisms_per_ml_easy
         ,cells_per_ml
         #,cells_per_ml_easy
         #,biovolume_per_ml_old
         ,biovolume_per_ml
         #,biovolume_per_ml_easy
           ) %>% 
  glimpse()
#I prefer to use the formulas based on the more raw version of the data 
#rather than the ones based on the factor column
#which is a derived column and therefore more prone to errors
#warnings indicate some missing times; I know some times weren't recorded

#look at station names
#unique(phyto_cleanest$station)

#look at number of samples per station
samp_count<-phyto_cleanest %>% 
  distinct(region,station, date) %>% 
  group_by(region,station) %>% 
  summarize(count = n())
#looks fine

#check for NAs
#check_na <- phyto_cleanest[rowSums(is.na(phyto_cleanest)) > 0,]
#most are just missing time, which is fine because time not always recorded
#could look up fish survey data to get times for some of these

#Add higher level taxonomic information using the algaeClassify package--------
#as of 3/4/2022 this package wasn't working

#started by trying out examples from documentation. they didn't work
#algae_search(genus='Anabaena',species='flos-aquae',long=FALSE)

#data(lakegeneva)
#lakegeneva=lakegeneva[1,] ##use 1 row for testing
#lakegeneva.algaebase<-
#  spp_list_algaebase(lakegeneva,phyto.name='phyto_name',long=FALSE,write=FALSE)

#Add higher level taxonomic information and habitat information manually-------------

#names(taxonomy)

#subset to just the needed columns
#will match genus name between the two data frames
taxon_high<-taxonomy %>% 
  clean_names() %>% 
  select(kingdom
  ,phylum
  ,class
  ,genus
  ,algal_type) %>%  #confirmed that there is just one type per genus
#this approach is very simple because it removes the need for exact matches in the taxon names
#part of the difficulty in matching taxon names is presence of "cf." for many taxa
#unfortunately by just matching by genus and not taxon, we lose the habitat type, and salinity range info
#also with species level data, there's a chance the taxonomy dataset doesn't include every species in the samples
  #remove some (likely incorrect) combinations that are creating duplicates for genus
  #check AlgaeBase to see which taxonomic info is correct
  filter(!(genus == "Achnanthidium" & class =="Fragilariophyceae") & 
           !(genus == "Elakatothrix" & class =="Chlorophyceae") &
           !(genus == "Leptocylindrus" & algal_type =="Centric diatom"))%>% 
  #condense taxonomy data set to just the unique combinations
  distinct(kingdom,phylum,class,genus)  
#initially some genera appeared more than once in this taxonomy data set
#but this has been corrected

#investigating duplicates for genus---------------

#count number of times each genus appears
#ideally this would be once
tax_gen_sum<-data.frame(table(taxon_high$genus)) 

#look at repeat genera
tax_gen_sum_sub<-filter(tax_gen_sum, Freq >1)
#first time doing this there were three genera plus "unknown"
#Achnanthidium    n=2, Elakatothrix    n=2, Leptocylindrus   n=2
#fixed this so that only "unknown" has duplicates

#now go back to taxonomy data set and look at these three genera
#gen_dup<-taxon_high_rn %>% 
#  filter(genus == "Achnanthidium" | genus =="Elakatothrix" | genus=="Leptocylindrus" )
#remove the combos that are duplicates from the main data set

#combine sample data and high level taxonomy by genus----------
names(phyto_cleanest)
names(taxon_high)
phyto_tax<-left_join(phyto_cleanest,taxon_high) %>% 
  glimpse()

#final edits to complete final data version of data set
phyto_final<-phyto_tax %>% 
  #order by date and time
  arrange(date, time) %>%
  mutate(
    #fix one case of phyto_form from "f." to "f"
    phyto_form = case_when(phyto_form =="f." ~ "f",TRUE ~ phyto_form)
    #make time a character column for export; otherwise will automatically convert to UTC
    ,time_pst = as.character(time)
                 ) %>% 
  #reorder columns data frame export
  select(station
         ,station_comb
         ,region
         ,collected_by
         ,date
         ,time_pst
         ,kingdom
         ,phylum
         ,class
         ,genus
         ,taxon
         ,phyto_form
         ,organisms_per_ml
         ,cells_per_ml
         #,biovolume_per_ml_old
         ,biovolume_per_ml
  ) %>%
  glimpse()

#check for NAs
#check_na2 <- phyto_final[rowSums(is.na(phyto_final)) > 0,]

#write the formatted data as csv 
#write_csv(phyto_final,file = "Data/phytoplankton/SMSCG_phytoplankton_formatted_2020-2021.csv")

#write version of data set that includes only the samples collected by DFW--------
#this is for the phytoplankton synthesis effort

#look at number of rows associated with DFW vs EMP
phyto_surv_sum <- phyto_tax %>% 
  group_by(survey) %>% 
  summarize(n = n())

#look at number of samples associated with DFW vs EMP
phyto_surv_sum2 <- phyto_tax %>%
  distinct(survey,station,date) %>% 
  group_by(survey) %>% 
  summarize(n = n())

phyto_dfw<-phyto_tax %>% 
  filter(survey=="DFW") %>% 
  select(-station) %>% 
  rename(station=station_clean) %>% 
  select(station
         ,date
         ,time
         ,kingdom
         ,phylum
         ,class
         ,phyto_form
         ,genus
         ,taxon
         ,organisms_per_ml
         ,cells_per_ml
         #,biovolume_per_ml_old
         ,biovolume_per_ml
  ) %>%
  glimpse()
#write_csv(phyto_dfw,file = "Data/phytoplankton/SMSCG_phytoplankton_formatted_DFW_only_2020-2021.csv")

#look at list of station names in DFW samples data set
unique(phyto_dfw$station)

#look for NAs
check_na <- as.data.frame(colSums(is.na(phyto_dfw))) 
#only time column has NAs
check_time <- phyto_dfw %>% 
  distinct(station,date,time) %>% 
  filter(is.na(time))
#three samples are missing times which makes sense


#exploring taxonomy data set--------------------

#how many genera in samples not in taxonomy data?
#look for NA in Class column
sum(is.na(phyto_tax$class))
#31 cases in which a genus in a sample didn't match the taxonomy data

#look at set of samples without matching taxonomy
misfits <- phyto_tax %>% 
  filter(is.na(class)) %>% 
  distinct(genus)
#12 new genera plus NA

#how often do genera have multiple algae types? if always just one, then can just add the algae type column
#if more than one, then we have to match up taxa to use the algae type
tax_at<-unique(taxonomy[,c('Genus','Algal Type')])

#also just how many algal types in total
unique(taxonomy$'Algal Type')
#n = 32 but some of these are just duplicates created by formatting differences and also unknown categories
#but I bet many of these are uncommon and can be lumped into an "Other" category

#look at algal types and Class together to see if that helps
tax_atc<-unique(taxonomy[,c('Phylum','Class','Algal Type')])
sorted<-tax_atc[order(tax_atc$Phylum, tax_atc$Class),]
  
#count number of unique algal types within Genus
#ideally this is one for every genus
tax_at_sum<-data.frame(table(tax_at$Genus)) 
#looks like there are some genera with multiple algal types

#look at genera with multiple algal types
tax_at_sum_sub<-filter(tax_at_sum, Freq >1)
#most of these are associated with "Unknown" genus
#there are two associated with genus Leptocylindrus

#look at Leptocylindrus in main taxonomy data set
lept<-taxonomy %>% 
  filter(Genus =="Leptocylindrus")
#the two algal types are simply different forms of the same name "Centric diatom" and "Centric Diatom"

#create a secondary data set that has the same information but 
#but based on synonyms of the current names
#subset this data set to exclude rows with "Synonym(s)" = "None" or "Unknown"

#create columns that separates the genus and species of the synonyms
#this way, any out of date names in the sample data can be matched by genus



            
  
  
  
  
  
  
  
  
