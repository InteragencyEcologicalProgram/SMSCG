#Suisun Marsh Salinity Control Gate
#Phytoplankton Data 2020-2022
#Format raw data in preparation for visualization and analysis
#Biovolume calculations have been corrected

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(hms) #working with date/time
library(lubridate) #working with dates
library(readxl) #importing data from excel files

#Notes
#For all BSA files from 2013 to 2021, the column "Number of cells per unit" really means "Total cells", 
#which is the total number of cells counted for that taxon in a particular sample
#calculations in this script were corrected accordingly on 2/10/2022

# Read in the EMP data from EDI------------------

phytoplankton_emp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1320.5&entityid=67b9d4ee30d5eee6e74b2300426471f9") %>% 
  clean_names() %>% 
  glimpse()

# Read in and combine the DFW data----------------------------------------------

#Create character vectors of DFW phytoplankton files for all years  
phyto_files_dfw <- dir(path = "EDI/data_input/phytoplankton", pattern = "DFW", full.names = T, recursive=T)

phytoplankton_dfw <- phyto_files_dfw %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  map_dfr(~read_excel(.x, col_types = "text"), .id = "source") %>% 
  #specify the survey
  mutate(collected_by = as.factor("DFW")) %>% 
  #code below would pull the survey from the file name; need to update the character range though
  # mutate(collected_by = as.factor(str_sub(source,25,27))) %>% 
  clean_names() %>% 
  glimpse()
#succeeded in combining all the sample files
#but date and time are in weird format
#columns that don't match across files get kicked to back of data set
#sampling depth column has three variations: "Depth (m)", "Depth (ft.)", "Depth (ft)"
#after 2020: 'Full Code' is added as column
#after 2021: Unit Abundance becomes Unit Abundance (# of Natural Units); Number of cells per unit becomes Total Number of Cells

#unique(phytoplankton_dfw$station_code)
#why is there an NA for station code? Because cell width and depth are on separate lines
#it's fine for now

# Read in the other files----------------

#read in taxonomy data
#this probably needs to be updated with each new batch of data
#update this file with the updates/corrections I got from AlgaeBase 2/24/2022
taxonomy <- read_csv("./EDI/data_input/phytoplankton/PhytoplanktonTaxonomy_2022-02-09.csv")

#read in EMP taxonomy data from PESP GitHub repo
taxonomy_emp <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/PESP/main/admin/global_data/phyto_classification.csv") %>% 
  clean_names()

#read in station name info
#includes region categories, station names, and names that identify comparable stations through time
stations <- read_csv("EDI/data_input/phytoplankton/smscg_stations_phyto.csv")
#NOTE: once we have updated this on EDI, we can just pull it from there instead of GitHub repo

#EDI: format EMP data-------------------------
#add month column to use to filter data set to just the needed time period
#filter stations to just the ones I need
#"EMP_NZ032" "EMP_D4"    "EMP_NZS42" "EMP_EZ2"   "EMP_D7"    "EMP_D22"   "EMP_EZ6" 

#filter EMP data set to just the stations and dates needed
phyto_emp_recent <- phytoplankton_emp %>% 
  mutate(
    #add a column to indicate who collected the samples
    collected_by = "EMP"
    #create a month column
    ,month = as.numeric(month(sample_date))
    #create a year column
    ,year = as.numeric(year(sample_date))
    ) %>% 
  unite('station', c(collected_by,station_code),sep="_",remove=F) %>% 
  #only keep 2018 and beyond and July-Oct
  filter(year > 2017 & month > 6 & month < 11)
  
#check date range
range(phyto_emp_stations$sample_date)
#looks good; "2018-07-09" "2022-10-20"

#look at list of stations
unique(phyto_emp_stations$station)
#29 stations

#create list of EMP stations needed from the station metadata file
stations_emp <- stations %>% 
  filter(grepl("EMP",alias)) %>% 
  pull(alias)

phyto_emp_stations <- phyto_emp_recent %>% 
  filter(station %in% stations_emp) %>% 
  #reorder and rename columns as needed
  select(station
         ,collected_by
         ,date = sample_date
         ,time_pst = sample_time
         ,lab
         ,taxon_original = orig_taxon
         ,taxon
         ,kingdom:species
         ,organisms_per_ml = units_per_m_l
         ,cells_per_ml = cells_per_m_l
         ,biovolume_cubic_um_per_ml = average_biovolume_per_m_l
         ,gald_um = gald
         ,phyto_form
         ,quality_check
  ) %>% 
  glimpse()

#look at list of stations remaining
unique(phyto_emp_stations$station)
#"EMP_EZ2"   "EMP_EZ6"   "EMP_NZS42" "EMP_NZ032" "EMP_D7"    "EMP_D22"   "EMP_D4"   

#look at summary of samples for these stations
phyto_emp_samp_sum <- phyto_emp_stations %>% 
  distinct(station,date,time_pst) %>% 
  group_by(station) %>% 
  count()
#expecting 20 samples per station (4 months x 5 years)
#no missing samples

#clean up DFW station names-------------

#look at stations in the data set
#unique(phytoplankton_dfw$station_code)

phyto_dfw_stations <- phytoplankton_dfw %>% 
  #remove empty rows created by linear cell measurement rows (length, width, depth)  
  #a little tricky just because the survey name appears in every row including the otherwise empty ones
  #chose the taxon column as the ones to check for missing data
  drop_na(taxon) %>% 
  #drop the GZB data because only two samples collected there ever
  filter(station_code!="GZB") %>% 
  mutate(
    #format date
    date = as.Date(as.numeric(sample_date),origin = "1899-12-30")
    #format time
    ,time = as_hms(as.numeric(sample_time)*60*60*24)
    #create a date time column; DFW records time in PDT
    ,date_time_pdt = ymd_hms(as.character(paste(date, time)),tz="America/Los_Angeles")
    #change PDT to PST to match the EMP times
    ,date_time_pst = with_tz(date_time_pdt,tzone="Etc/GMT+8")
    #create a month column
    ,month = as.numeric(month(date))
    #add column that indicates which survey collected samples
    #writes over existing DFW collected_by column
    ,'collected_by' = case_when(
      month < 9 ~ "STN"
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
      ,grepl("MON|MONT", station_code) ~ "STN_MONT"
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
  relocate(station,.after = station_code) %>% 
  glimpse()
#4 rows for date time didn't parse because no time recorded for sample

#look at stations again
#unique(phyto_dfw_stations$station)
#17 stations, which is correct

#make sure conversion of DFW time from PDT to PST worked
#tz_check <- phyto_dfw_stations %>%
#  select(date_time_pdt,date_time_pst) %>% 
#  mutate(time_dif = ymd_hms(date_time_pdt) - ymd_hms(date_time_pst)) %>% 
#  glimpse()
#looks good

#look at NAs for date time
#time_nas <- phyto_dfw_stations %>% 
 # select(station_code,date,time,date_time_pdt,date_time_pst) %>% 
  #filter(is.na(date_time_pdt))

#add the region and combo station data 
phyto_dfw <- left_join(phyto_dfw_stations, stations) %>% 
  glimpse()

#make sure all stations matched
phyto_dfw_na <- phyto_dfw %>% 
  filter(is.na(station_group))
#no NAs so matched correctly

phyto_dfw_combo <- phyto_dfw %>% 
  distinct(region, station, month, collected_by) %>% 
  arrange(month, station, collected_by)


#format the DFW sample data set------------
#NOTE: need to specify column types
#majority are still character because we read in files with all columns as text

#combine EMP and DFW sample data
# phytoplankton <- bind_rows(phyto_emp,phyto_dfw) %>% 
#   mutate(
#     #combine data from the two total cells columns (just different names for same thing)
#     total_cells = as.numeric(case_when(!is.na(number_of_cells_per_unit)~number_of_cells_per_unit
#                                  ,!is.na(total_number_of_cells)~total_number_of_cells))
#     #combine data from the two unit abundance columns (just different names for same thing)
#     ,unit_abundance2 = as.numeric(case_when(!is.na(unit_abundance)~unit_abundance
#                               ,!is.na(unit_abundance_number_of_natural_units)~unit_abundance_number_of_natural_units))
#   ) %>% 
#   glimpse()

#did the column merging work
#phyto_tc <- phytoplankton %>% 
#  filter(is.na(total_cells))
#no NAs as expected

#phyto_ua <- phytoplankton %>% 
#  filter(is.na(unit_abundance2))
#no NAs as expected

#format DFW data set
phyto_dfw_cleaner <- phyto_dfw %>% 
  mutate(
         #combine data from the two total cells columns (just different names for same thing)
         total_cells = as.numeric(case_when(!is.na(number_of_cells_per_unit)~number_of_cells_per_unit
                                      ,!is.na(total_number_of_cells)~total_number_of_cells))
         #combine data from the two unit abundance columns (just different names for same thing)
         ,unit_abundance2 = as.numeric(case_when(!is.na(unit_abundance)~unit_abundance
                                   ,!is.na(unit_abundance_number_of_natural_units)~unit_abundance_number_of_natural_units))
       ) %>% 
  #drop old unit abundance column
  select(-unit_abundance) %>% 
  #subset to just the needed columns
  select(station
         , collected_by
         , latitude
         , longitude
         , date_time_pst
         , genus
         , species
         , taxon
         , phyto_form = colony_filament_individual_group_code
         , unit_abundance = unit_abundance2
         , slide_chamber_area_mm2
         , volume_analyzed_m_l
         , field_of_view_mm2
         , number_of_fields_counted
         , factor
         , gald_1
         , total_cells
         , biovolume_1:biovolume_10
         ,comments) %>% 
  mutate(across(c(slide_chamber_area_mm2:biovolume_10),as.numeric)) %>% 
  rowwise() %>% 
  mutate(  
    #use the date-time column with standardized time zone to extract time
    time_pst = as_hms(date_time_pst)
    #use date-time column to extract date
    ,date = date(date_time_pst)
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
    ,biovolume_cubic_um_per_ml = round((total_cells* mean_cell_biovolume*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,biovolume_per_ml_easy = factor * total_cells * mean_cell_biovolume
    ) %>% 
  #subset and reorder columns again to just those needed
  select(station
         ,collected_by
         ,latitude
         ,longitude
         ,date
         ,time_pst
         ,taxon_original = taxon                            
         ,genus
         ,species
         ,organisms_per_ml
         #,organisms_per_ml_easy
         ,cells_per_ml
         #,cells_per_ml_easy
         #,biovolume_per_ml_old
         ,biovolume_cubic_um_per_ml
         #,biovolume_per_ml_easy
          ,gald_um = gald_1
         ,phyto_form 
         ,comments
         ) %>% 
  glimpse()
#I prefer to use the formulas based on the more raw version of the data 
#rather than the ones based on the factor column
#which is a derived column and therefore more prone to errors

#look at station names
unique(phyto_dfw_cleaner$station)

#look at number of samples per station
phyto_dfw_samp_sum<-phyto_dfw_cleaner %>% 
  distinct(station,date) %>% 
  group_by(station) %>% 
  summarize(count = n(),.groups = 'drop') %>% 
  arrange(count)
#max count is 8 surveys x 3 years = 24
#some stations are only collected during half the season so 12
#some stations haven't been sampled whole three years
#also EMP samples some of these stations too so DFW would have skipped them
#also I know some samples have been missed

#look at taxonomist comments
phyto_dfw_comments <- phyto_dfw_cleaner %>% 
  distinct(comments) %>% 
  arrange(comments)
#30 unique comments
#some about not meeting tally in 50 fields
#some comments about presence of fungus in samples
#some about high sediment/detritus
#some about many broken diatoms

#create new columns summarizing comments
phyto_dfw_cleanest <- phyto_dfw_cleaner %>% 
  mutate(
    #add column for quality based on comments
    quality_check = case_when(
       grepl("degraded", comments, ignore.case=T) ~ "Degraded"
       ,grepl("fragment", comments,ignore.case=T) ~"Fragment"
       ,grepl("50 fields",comments,ignore.case=T)~"BadData"
       ,grepl("fungus",comments,ignore.case=T)~"PoorlyPreserved"
       ,TRUE ~ "Good"
     )
    #add column indicating amount of sediment and detritus
    #comments often note differing levels of sediment vs detritus
    #for simplicity combine them and use the highest level indicated
    #eg, low detritus and high sediment simply becomes high
    ,debris = case_when(
      grepl("high sediment",comments, ignore.case=T)~"High"
      ,grepl("high det",comments, ignore.case=T)~"High" #shortened because of typo "detitus"
      ,grepl("moderate sediment",comments, ignore.case=T)~"Moderate"
      ,grepl("moderate detritus",comments, ignore.case=T)~"Moderate"
      ,grepl("low sediment",comments, ignore.case=T)~"Low"
      ,grepl("low detritus",comments, ignore.case=T)~"Low"
      ,TRUE~NA
    )
  ) %>% 
  glimpse()

#look closer at how comments were translated to categories for debris and quality_check
phyto_comment_check <- phyto_dfw_cleanest %>% 
  filter(!is.na(comments)) %>% 
  select(comments,quality_check,debris) %>% 
  distinct(comments,quality_check,debris) %>% 
  arrange(quality_check,debris)


#Add higher level taxonomic information manually-------------

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
names(phyto_dfw_cleaner)
names(taxon_high)
phyto_tax<-left_join(phyto_dfw_cleaner,taxon_high) %>% 
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
  #now that we have a station metadata file, we no longer need some of these columns in the data file
  select(station
         #,alias
         #,station_group
         #,region
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

#look at list of station names again
#unique(phyto_final$station)

#check for NAs
#check_na2 <- phyto_final[rowSums(is.na(phyto_final)) > 0,]

#look at start date for all stations
stn_start <- phyto_final %>% 
  group_by(station) %>% 
  summarize(date_min = min(date),.groups='drop') %>% 
  arrange(date_min)




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


#Combine EMP and DFW datasets into one data frame----------------

#write the formatted data as csv 
#write_csv(phyto_final,file = "EDI/data_output/SMSCG_phytoplankton_formatted_2020-2022.csv")

            
  
  
  
  
  
  
  
  
