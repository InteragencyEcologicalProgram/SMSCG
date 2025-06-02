#Suisun Marsh Salinity Control Gate
#Phytoplankton Data 2020-2023
#Format raw data in preparation for publishing on EDI
#Biovolume calculations have been corrected

#required packages
library(purrr) #for function that downloads EDI data
library(glue) #for function that downloads EDI data
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(hms) #working with date/time
library(lubridate) #working with dates
library(readxl) #importing data from excel files
library(deltamapr) #Delta shape files
library(sf) #spatial tools

# Notes------------------
#starting July 2024, AEU will update phyto enumeration methods to match the new ones that EMP implemented

#Starting July 2023, EMP changed the way BSA enumerates phytoplankton to better detect large, rare taxa.This new method is not
#comparable to the previous enumeration method. To compare the new data with the old data, the taxa enumerated from the transects 
#need to be removed. The transect taxa are those with field-of-view (column J) listed as 5.9 or 5.96 and number of fields counted (column M) of 1 or 2.
#removing those taxa leaves just the field counts, which are directly comparable to the old data
#I have filtered these data to remove transect taxa in this script

#For all BSA files from 2013 to 2021, the column "Number of cells per unit" really means "Total cells", 
#which is the total number of cells counted for that taxon in a particular sample
#calculations in this script were corrected accordingly on 2/10/2022

# Functions-----------------
#these are Perry's functions for summarizing taxonomist comments
#The only difference between my code and Perry's code is I used "comments" instead of "Comments"

#function for generating QualityCheck column
add_qc_col <- function(df){
  df <- df %>%
    mutate(
      QC_1 = case_when(grepl('delete|cross contamination', comments, ignore.case = TRUE) ~ 'BadData'),
      QC_2 = case_when(grepl('did not reach|cannot meet tally|cannot meet natural unit', comments, ignore.case = TRUE) ~ 'TallyNotMet'),
      QC_3 = case_when(grepl('degraded', comments, ignore.case = TRUE) ~ 'Degraded'),
      QC_4 = case_when(grepl('poor preservation|poorly preserved|weak preservation|weakly preserved|fungus', comments, ignore.case = TRUE) ~ 'PoorlyPreserved'),
      QC_5 = case_when(grepl('obscured', comments, ignore.case = TRUE) ~ 'Obscured'),
      QC_6 = case_when(grepl('fragment\\.|diatom fragment', comments, ignore.case = TRUE) ~ 'Fragmented'),
      QC_7 = case_when(grepl('broken diatom', comments, ignore.case = TRUE) & !grepl('broken diatom fragment', comments, ignore.case = TRUE) ~ 'BrokenDiatoms'),
      QC_8 = case_when(grepl('mucilaginous detritus', comments, ignore.case = TRUE) ~ 'MucilaginousDetritus')
    ) %>%
    unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ')
  
  df$QualityCheck[df$QualityCheck == ''] <- 'Good'
  
  return(df)
}

#function for generating Debris column
add_debris_col <- function(df){
  df <- df %>%
    mutate(
      debris =
        case_when(
          grepl('high detritus|high sediment|heavy detritus|heavy sediment', comments, ignore.case = TRUE) ~ 'high',
          grepl('moderate detritus|moderate sediment', comments, ignore.case = TRUE) ~ 'moderate',
          grepl('low detritus|low sediment', comments, ignore.case = TRUE) ~ 'low',
          TRUE ~ NA_character_
        )
    )
  
  return(df)
}

#function for downloading EDI data
get_edi_file = function(pkg_id, fnames){
  # Get revision
  revision_url = glue::glue('https://pasta.lternet.edu/package/eml/edi/{pkg_id}')
  all_revisions = readLines(revision_url, warn = FALSE) 
  latest_revision = tail(all_revisions, 1)
  
  # Get entities 
  pkg_url = glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}')
  all_entities = readLines(pkg_url, warn = FALSE)
  name_urls = glue::glue('https://pasta.lternet.edu/package/name/eml/edi/{pkg_id}/{latest_revision}/{all_entities}')
  names(all_entities) = purrr::map_chr(name_urls, readLines, warn = FALSE)
  
  # Select entities that match fnames
  fname_regex = stringr::str_c(glue::glue('({fnames})'), collapse = '|')
  included_entities = all_entities[stringr::str_detect(names(all_entities), fname_regex)]
  if(length(included_entities) != length(fnames)){
    stop('Not all specified filenames are included in package')
  }
  # Download data
  dfs = purrr::map(glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}/{included_entities}'),
                   readr::read_csv, guess_max = 1000000, show_col_types = FALSE)
  names(dfs) = names(included_entities)
  
  if (length(dfs) == 1) {
    return(dfs[[1]])
  } else {
    return(dfs)
  }
}


# Read in the EMP data from EDI (pre-2023)------------------------------

#these links don't work anymore
# phytoplankton_emp_edi <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1320.8&entityid=634e9843500249d3b96b45fd6a8cad65") %>% 
#   clean_names() %>% 
#   glimpse()

#use function to download data
#specify package number and name of specific file
#if file name not specified, will download all files
phytoplankton_emp_edi = get_edi_file(pkg_id = 876, fnames = "smscg_phytoplankton_samples")  
#this was working but now is not as of 5/30/25 (HTTP status was '429 Unknown Error'); try again later
#also need to apply clean_names() function to this


# Read in and combine 2023 EMP data files from GitHub repo--------------------

#Create character vectors of EMP phytoplankton files for all years  
phyto_files_emp <- dir(path = "EDI/data_input/phytoplankton/2023", pattern = "EMP", full.names = T, recursive=T)

phytoplankton_emp_repo <- phyto_files_emp %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  map_dfr(~read_excel(.x, col_types = "text"), .id = "source") %>% 
  #specify the survey
  mutate(collected_by = as.factor("EMP")) %>% 
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

# Read in and combine the DFW data-------------------------

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
#below, we combine each of these two sets of columns with different names through time

#unique(phytoplankton_dfw$station_code)
#why is there an NA for station code? Because cell width and depth are on separate lines
#it's fine for now

# Read in taxonomy and station metadata files-----------------

#read in taxonomy data
#this probably needs to be updated with each new batch of data
#update this file with the updates/corrections I got from AlgaeBase 2/24/2022
# taxonomy <- read_csv("./EDI/data_input/phytoplankton/PhytoplanktonTaxonomy_2022-02-09.csv") %>% 
#   clean_names() %>% 
#   #rename taxonomic name column to taxon_original
#   rename(taxon_original = taxon) %>% 
#   glimpse()

#read in EMP taxonomy data from PESP GitHub repo (10/10/2023)
# taxonomy_emp <- read_csv("./EDI/data_input/phytoplankton/phyto_classification_PESP_2023_10_10.csv") %>% 
#   #for some reason, a few names in the current_name column of the file from PESP Github repo taxonomy file could not be edited with code
#   #Actinocyclus cuneiformis, Gomphonema lingulatum var. constrictum
#   #had to create new file, which I put in the SMSCG repo, delete these two cells and retype them; then things worked like normal
#   #read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/PESP/main/admin/global_data/phyto_classification.csv") %>% 
#   clean_names()  %>% 
#   #rename taxonomic name column to taxon_original
#   rename(taxon_original = name) %>% 
#   glimpse()

#read in EMP taxonomy data from PESP GitHub repo (3/21/2024)
# taxonomy_emp_new <- read_csv("./EDI/data_input/phytoplankton/phyto_classifications_PESP_2024-03-21.csv") %>% 
#   clean_names() %>% 
#   #rename taxonomic name column to taxon_original
#   rename(taxon_original = taxon) %>% 
#   glimpse()

#read in additional EMP taxonomy data from PESP GitHub repo (3/21/2024)
#not sure what this is but probably taxa not in EMP survey but in other phyto surveys
#there is redundancy between these two new EMP dataframes so "extra" might include everything in the other df plus the extras
taxonomy_emp_extra <- read_csv("./EDI/data_input/phytoplankton/phyto_classifications_extra_PESP_2024-03-21.csv") %>% 
  clean_names() %>% 
  #rename taxonomic name column to taxon_original
  rename(taxon_original = taxon) %>% 
  glimpse()

#combine new EMP taxonomy data frames
# taxonomy_emp_comb <- bind_rows(taxonomy_emp_new, taxonomy_emp_extra_new) %>% 
#   distinct() %>% 
#   glimpse()
  

#read in file with taxa from AWCA that didn't match EMP taxonomy
# taxonomy_awca_mism <- read_csv("https://raw.githubusercontent.com/EMRR-DISE/DSRS_AWCA/main/phyto/data_input/other/phyto_taxonomy_mismatch_fixed_2023-08-11.csv") %>% 
#   rename(taxon_original = name
#          ,current_taxon = current_name
#          ) %>% 
#   select(-species)

#read in supplementary taxonomy info that fills gaps in PESP list
taxonomy_fix <- read_csv("./EDI/data_input/phytoplankton/smscg_phyto_taxonomy_mismatch_fixed_2024-03-21.csv") %>% 
  rename(current_taxon = current_name) %>% 
  glimpse()

#add the missing taxa to the PESP/EMP taxonomy dataset and then add to SMSCG dataset
#some differences in columns between datasets but bind_rows will figure it out
#nonmatching columns won't be in final version anyway
#glimpse(taxonomy_fix) 
#glimpse(taxonomy_emp_extra)
taxonomy_emp_amend <- bind_rows(taxonomy_fix,taxonomy_emp_extra)  %>% 
  select(kingdom:genus) %>% 
  distinct() %>% 
  glimpse()

#read in SMSCG station name info
#includes region categories, station names, and names that identify comparable stations through time
stations <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=08de2a97cf2a3743af06e3ff6e0e9b39")

#create vector of EMP station names from station metadata file
station_names <- stations %>% 
  pull(station)

#Read in EMP station info
stn_emp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1320.6&entityid=857fb9a315cd6bc47f2090f74fd1c938") %>% 
  clean_names() %>% 
  #rename lat/long columns in prep for adding to EMP phyto dataset which already has a lat/long column (but just for EZ stations)
  rename(station = station_code
         ,latitude2 = latitude
         ,longitude2 = longitude) %>% 
  #drop locality description
  select(-location) %>% 
  glimpse()

#Repo: format EMP data-------------------------
#2023 data
#clean up EMP station names and drop unneeded stations

#look at stations in the data set
#unique(phytoplankton_emp_repo$station_code)

phyto_emp_repo_stations <- phytoplankton_emp_repo %>% 
  #remove empty rows created by linear cell measurement rows (length, width, depth)  
  #a little tricky just because the survey name appears in every row including the otherwise empty ones
  #chose the taxon column as the ones to check for missing data
  drop_na(taxon) %>%
  mutate(
    #format date
    date = as.Date(as.numeric(sample_date),origin = "1899-12-30")
    #format time and specify that time zone is PST
    ,time = as_hms(as.numeric(sample_time)*60*60*24)
    #create a date time colum
    ,date_time_pst = ymd_hms(as.character(paste(date, time)),tz="Etc/GMT+8")
    #create a month column
    ,month = as.numeric(month(date))
    #correct two typos in station names
    # ,'station_corr' = case_when(grepl("E26", station_code) ~ "EZ6"
    #                             ,grepl("NZ542", station_code) ~"NZS42"
    #                             ,TRUE ~ as.character(station_code))
     )  %>% 
  #add prefix to station names
  unite('station', c(collected_by,station_code),sep="_",remove=F) %>% 
  #drop the June samples
  #filter(month!=6) %>% 
  #drop one sample that was submitted to BSA empty
  #filter(!(station=="EMP_EZ6" & date=="2020-09-10")) %>% 
  glimpse()

#look at combinations of field of view and number of field counted
#transects are ones with 5.9 or 5.96 for field of view and 1 or 2 for number of fields
# transect_fields <- phyto_emp_repo_stations %>% 
#   distinct(field_of_view_mm2,number_of_fields_counted) %>% 
#   arrange(field_of_view_mm2,number_of_fields_counted)
#the combinations of numbers for these two columns makes sense

#check time zone
#tz(phyto_emp_repo_stations$date_time_PST)

#look at station names again
#unique(phyto_emp_repo_stations$station)

#filter the data set to just the stations needed for SMSCG and add additional station metadata
phyto_emp_repo <- phyto_emp_repo_stations %>% 
  filter(station %in% station_names) %>% 
  left_join(stations) %>% 
  #for now, leave out EZ stations because we don't have coordinates for them handy
  filter(station!="EMP_EZ2" & station!="EMP_EZ6") %>% 
  glimpse()

#make sure all stations matched
# phyto_emp_repo_na <- phyto_emp_repo %>%
#   filter(is.na(station_group))
#no NAs so matched correctly

#summary of station info
# phyto_emp_repo_combo <- phyto_emp_repo %>%
#   distinct(region, station, month, collected_by) %>%
#   arrange(month, station, collected_by)



#format EMP repo data set
phyto_emp_repo_cleaner <- phyto_emp_repo %>% 
  #mutate(
    #combine data from the two total cells columns (just different names for same thing)
    # total_cells = as.numeric(case_when(!is.na(number_of_cells_per_unit)~number_of_cells_per_unit
    #                                    ,!is.na(total_number_of_cells)~total_number_of_cells))
    #combine data from the two unit abundance columns (just different names for same thing)
  #   ,unit_abundance2 = as.numeric(case_when(!is.na(unit_abundance)~unit_abundance
  #                                           ,!is.na(unit_abundance_number_of_natural_units)~unit_abundance_number_of_natural_units))
  # ) %>% 
  #drop old unit abundance column
  #select(-unit_abundance) %>% 
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
         , unit_abundance = unit_abundance_number_of_natural_units
         , slide_chamber_area_mm2
         , volume_analyzed_m_l
         , field_of_view_mm2
         , number_of_fields_counted
         , factor
         , gald_1
         , total_cells = total_number_of_cells
         , biovolume_1:biovolume_10
         ,comments) %>% 
  mutate(across(c(unit_abundance:biovolume_10),as.numeric)) %>% 
  rowwise() %>% 
  #drop the taxa from the transects and only keep the ones from field counts
  #this makes the new data comparable with the old data
  filter(field_of_view_mm2<5.9) %>% 
  mutate(  
    #use the date-time column with standardized time zone to extract time
    time_pst = as_hms(date_time_pst)
    #use date-time column to extract date
    ,date = date(date_time_pst)
    #create new column that calculates mean biovolume per cell
    ,mean_cell_biovolume = mean(c_across(biovolume_1:biovolume_10),na.rm=T)
    #create new column that calculates organisms per mL; round number to nearest tenth
    #different from cells per mL because some organisms are multicellular
    ,units_per_ml = round((unit_abundance*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,units_per_ml_easy = (unit_abundance*factor)
    #create new column that calculates cells per mL; round number to nearest tenth
    ,cells_per_ml = round((total_cells*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,cells_per_ml_easy = (total_cells*factor)
    #create a column that calculates biovolume per mL
    #units for biovolume are cubic microns; old version is incorrect calculations; round number to nearest tenth
    #,biovolume_per_ml_old = units_per_ml * total_cells * mean_cell_biovolume
    ,biovolume_per_ml = round((total_cells* mean_cell_biovolume*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
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
         ,units_per_ml
         #,units_per_ml_easy
         ,cells_per_ml
         #,cells_per_ml_easy
         #,biovolume_per_ml_old
         ,biovolume_per_ml
         #,biovolume_per_ml_easy
         ,gald_um = gald_1
         ,phyto_form 
         ,comments
  ) %>% 
  glimpse()
#I prefer to use the formulas based on the more raw version of the data 
#rather than the ones based on the factor column
#which is a derived column and therefore more prone to errors

#Summarize comments
#simplify them into two columns QualityCheck and Debris

#add QualityCheck and Debris columns to my dataset
phyto_emp_repo_cleanest <- phyto_emp_repo_cleaner %>% 
  add_qc_col() %>% 
  add_debris_col() %>% 
  clean_names() %>% 
  glimpse()

#add taxonomic info to EMP repo data set
phyto_emp_repo_tax <- left_join(phyto_emp_repo_cleanest,taxonomy_emp_amend)   %>%  
  mutate(
    #create taxon column that is taxon_original with old names replaced with current names
    #ie, shouldn't have any missing names in the new taxon column
    taxon = case_when((current_taxon!="None" & current_taxon!="Unknown")~current_taxon
                      ,TRUE~taxon_original)
    #create new taxon_original column that only includes a name if there is a new one to replace it
    ,taxon_original2 = case_when((current_taxon!="None" & current_taxon!="Unknown")~taxon_original
                                 ,TRUE ~ NA)
    #looks like genus is wrong in cases where the new name is a different genus
    #so need to make new genus column
    #start by copying taxon column and dropping all the qualifiers
    ,taxon2 =str_replace_all(taxon, pattern = c('cf[.] ' = '',' cf[.]'="",' var[.]' = "", ' fo[.]' = ""," sp[.]"="","2"="","  "=" "))
    #now make new genus column
    ,genus2 = word(taxon2, 1, sep=" ")
    # #for some reason, a few taxa didn't work with word(); ie still full name, not genus
    #Actinocyclus cuneiformis, Gomphonema lingulatum var. constrictum
    #nothing could edit these names from the PESP Github repo taxonomy file
    #had to create new file, which I put in the SMSCG repo, delete these two cells and retype them
    #then the functions worked as expected
    #next, make a couple of corrections for phylum and class
    ,phylum2 = case_when(phylum == "Cryptophycophyta incertae sedis"~"Cryptista",TRUE ~ phylum)
    ,class2 = case_when(class == "Cryptophycophyta incertae sedis"~"Katablepharidophyceae",TRUE ~ class )
    ,.after=taxon_original
  ) %>% 
  select(
    station:time_pst
    ,taxon_original = taxon_original2
    ,taxon
    ,kingdom
    ,phylum = phylum2
    ,class = class2
    ,algal_group
    ,genus = genus2
    ,species:phyto_form
    ,quality_check
    ,debris
  ) %>%
  #combining by taxon original and genus created duplicated rows in some cases
  #for now, fix this with distinct function. this gets back to correct number of rows
  distinct() %>% 
  glimpse()

#look for non-matching taxa
# tax_mism_emp <- phyto_emp_repo_tax %>%
#   filter(is.na(kingdom)) %>%
#   select(taxon_original:species) %>%
#   distinct() %>%
#   arrange(taxon) %>%
#   select(taxon_original = taxon
#          ,genus
#          ,species
#          )
#9 taxa that didn't match initially

#format DFW data-------------

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
    #all PDT time should be an hour ahead of PST during July to Oct
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
      #in FMWT survey, MONT is now 611
      ,grepl("MON|MONT|611", station_code) ~ "STN_MONT"
      ,grepl("609", station_code) ~ "STN_609"
      #in 2022, some samples were collected at FMWT 608 instead of FMWT 610
      #probably close enough to just lump with rest of 610 data
      #though 608 is upstream of SMSCGs while 610 is downstream of them
      ,grepl("610|608", station_code) ~ "STN_610"
      #West Marsh stations
      ,grepl("605", station_code) ~ "FMWT_605"
      ,grepl("606", station_code) & month < 9 ~ "STN_606"
      ,grepl("606", station_code) & month > 8 ~ "FMWT_606"
      ,grepl("NZS42", station_code) ~ "EMP_NZS42"
      #Bay stations
      ,grepl("519", station_code) & month < 9 ~ "STN_519"
      ,grepl("519", station_code) & month > 8 ~ "FMWT_519"
      ,grepl("602", station_code) & month < 9 ~ "STN_602"
      ,grepl("602", station_code) & month > 8 ~ "FMWT_602"
      ,TRUE ~ as.character(station_code)            
    )) %>%
  relocate(station,.after = station_code) %>% 
  glimpse()

#look at stations again
#unique(phyto_dfw_stations$station)
#17 stations, which is correct

#make sure conversion of DFW time from PDT to PST worked
# tz_check <- phyto_dfw_stations %>%
#  select(date_time_pdt,date_time_pst) %>%
#  mutate(time_dif = ymd_hms(date_time_pdt) - ymd_hms(date_time_pst)) %>%
#  glimpse()
#looks good

#look at NAs for date time
# time_nas <- phyto_dfw_stations %>%
# select(station_code,date,time,date_time_pdt,date_time_pst) %>%
# filter(is.na(date_time_pdt))
#none

#add the region and combo station data 
phyto_dfw <- left_join(phyto_dfw_stations, stations) %>% 
  glimpse()

#make sure all stations matched
# phyto_dfw_na <- phyto_dfw %>%
#   filter(is.na(station_group))
#no NAs so matched correctly

#summary of station info
# phyto_dfw_combo <- phyto_dfw %>% 
#   distinct(region, station, month, collected_by) %>% 
#   arrange(month, station, collected_by)

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
    ,units_per_ml = round((unit_abundance*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,units_per_ml_easy = (unit_abundance*factor)
    #create new column that calculates cells per mL; round number to nearest tenth
    ,cells_per_ml = round((total_cells*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,cells_per_ml_easy = (total_cells*factor)
    #create a column that calculates biovolume per mL
    #units for biovolume are cubic microns; old version is incorrect calculations; round number to nearest tenth
    #,biovolume_per_ml_old = units_per_ml * total_cells * mean_cell_biovolume
    ,biovolume_per_ml = round((total_cells* mean_cell_biovolume*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
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
         ,units_per_ml
         #,units_per_ml_easy
         ,cells_per_ml
         #,cells_per_ml_easy
         #,biovolume_per_ml_old
         ,biovolume_per_ml
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
#unique(phyto_dfw_cleaner$station)

#look at number of samples per station
# phyto_dfw_samp_sum<-phyto_dfw_cleaner %>% 
#   distinct(station,date) %>% 
#   group_by(station) %>% 
#   summarize(count = n(),.groups = 'drop') %>% 
#   arrange(count)
# #max count is 8 surveys x 3 years = 24
# #some stations are only collected during half the season so 12
# #some stations haven't been sampled whole three years
# #also EMP samples some of these stations too so DFW would have skipped them
# #also I know some samples have been missed
# 
# #look at taxonomist comments
# phyto_dfw_comments <- phyto_dfw_cleaner %>%
#   distinct(comments) %>%
#   arrange(comments)
#30 unique comments
#some about not meeting tally in 50 fields
#some comments about presence of fungus in samples
#some about high sediment/detritus
#some about many broken diatoms

#Summarize comments
#simplify them into two columns QualityCheck and Debris

#add QualityCheck and Debris columns to my dataset
phyto_dfw_cleanest <- phyto_dfw_cleaner %>% 
  add_qc_col() %>% 
  add_debris_col() %>% 
  clean_names() %>% 
  glimpse()
  
#look closer at how comments were translated to categories for debris and quality_check
# phyto_comment_check <- phyto_dfw_cleanest %>% 
#   filter(!is.na(comments)) %>% 
#   select(comments,quality_check,debris) %>% 
#   distinct(comments,quality_check,debris) %>% 
#   arrange(quality_check,debris)
#write_csv(phyto_comment_check,"./EDI/data_input/phytoplankton/SMSCG_phytoplankton_taxonomist_comments.csv")

#Add taxonomy info to DFW dataset---------------

#create df with unique taxa
 # tax_scg <- phyto_dfw_cleanest %>% 
 #   distinct(taxon_original,genus,species) 
# #156 taxa
# 
# #compare taxa between SMSCG and EMP
# 
# #look at non-matches
 # tax_mism <- anti_join(tax_scg,taxonomy_emp)
# #33 mismatches between SMSCG data set and EMP taxonomy

#try matching the taxa without exact matches in the EMP taxonomy with just the genus in the EMP taxonomy
#tried this and didn't work well because of multiple genus matches per taxon

#start by dropping taxon_original from EMP taxonomy
# taxonomy_emp_gn <- taxonomy_emp %>% 
#   select(-taxon_original)
# 
# #now try matching between genus level version of EMP taxonomy and mismatched taxa
# tax_mism_gn <- left_join(tax_mism,taxonomy_emp_gn)
# #did get multiple hits from the EMP taxonomy for some taxa

#also look at the taxa that I added to my version of the taxonomy file
#tax_nick <- taxonomy %>% 
#   filter(nick_addition=="x")
# #17 taxa
# 
# #how many of the non-matches from EMP are in my version
# tax_mism_nick <- left_join(tax_mism,tax_nick)
# #only filled in two more taxa
# 
# #how many of the non-matches from EMP are in my AWCA supplemental taxonomy file?
# tax_mism_awca <- left_join(tax_mism,taxonomy_awca_mism)
#only one match

#export mismatch taxa to fill in results
#write_csv(tax_mism,"./EDI/data_input/phytoplankton/smscg_phyto_taxonomy_mismatch_2023-08-25.csv")


#look at cf. Chlorella sp.
# chlor <- taxonomy_emp_amend %>% 
#   filter(taxon_original == "cf. Chlorella sp.")
#there are two different rows for this taxon; one is an error
#for now, just make sure to match taxonomy and data by both taxon_original and genus to avoid issue

#look at non-matches again, shouldn't be any now
#tax_mism2 <- anti_join(tax_scg,taxonomy_emp_amend)
#zero as expected

#now add taxonomic info to SMSCG abundance dataset
#join by taxon_original and genus
#also, need to tweak the taxon, taxon_original, and genus columns
# glimpse(taxonomy_emp_amend) 
# glimpse(phyto_dfw_cleanest) #need to drop comments (converted to quality_check and debris columns)
phyto_dfw_tax <- left_join(phyto_dfw_cleanest,taxonomy_emp_amend) %>%   
  mutate(
    #create taxon column that is taxon_original with old names replaced with current names
    #ie, shouldn't have any missing names in the new taxon column
    taxon = case_when((current_taxon!="None" & current_taxon!="Unknown")~current_taxon
                      ,TRUE~taxon_original)
    #create new taxon_original column that only includes a name if there is a new one to replace it
    ,taxon_original2 = case_when((current_taxon!="None" & current_taxon!="Unknown")~taxon_original
                                 ,TRUE ~ NA)
    #looks like genus is wrong in cases where the new name is a different genus
    #so need to make new genus column
    #start by copying taxon column and dropping all the qualifiers
    ,taxon2 =str_replace_all(taxon, pattern = c('cf[.] ' = '',' cf[.]'="",' var[.]' = "", ' fo[.]' = ""," sp[.]"="","2"="","  "=" "))
    #now make new genus column
    ,genus2 = word(taxon2, 1, sep=" ")
    # #for some reason, a few taxa didn't work with word(); ie still full name, not genus
    #Actinocyclus cuneiformis, Gomphonema lingulatum var. constrictum
    #nothing could edit these names from the PESP Github repo taxonomy file
    #had to create new file, which I put in the SMSCG repo, delete these two cells and retype them
    #then the functions worked as expected
    #next, make a couple of corrections for phylum and class
    ,phylum2 = case_when(phylum == "Cryptophycophyta incertae sedis"~"Cryptista",TRUE ~ phylum)
    ,class2 = case_when(class == "Cryptophycophyta incertae sedis"~"Katablepharidophyceae",TRUE ~ class )
    ,.after=taxon_original
  ) %>% 
  select(
    station:time_pst
    ,taxon_original = taxon_original2
    ,taxon
    ,kingdom
    ,phylum = phylum2
    ,class = class2
    ,algal_group
    ,genus = genus2
    ,species:phyto_form
    ,quality_check
    ,debris
  ) %>%
  #combining by taxon original and genus created duplicated rows in some cases
  #for now, fix this with distinct function. this gets back to correct number of rows
  distinct() %>% 
  glimpse()

#look for non-matching taxa
# tax_mism2 <- phyto_dfw_tax %>% 
#   filter(is.na(kingdom)) %>% 
#   select(taxon_original:species) %>% 
#   distinct() %>% 
#   arrange(taxon) %>% 
#   select(taxon_original = taxon
#          ,genus
#          ,species
#          )
#initially 11 non-matching taxa to add to taxonomy file
#after updating taxonomy file, there are now zero non-matches as expected
#write_csv(tax_mism2,"./EDI/data_input/phytoplankton/smscg_phyto_taxonomy_mismatch_2024-03-21.csv")

#EDI: format EMP data-------------------------
#pre-2023 data

#look at list of stations in phyto dataset
#unique(phytoplankton_emp$station)
#31 stations

#is there any data in published EMP phyto data set for D24?
#d24 <- phytoplankton_emp %>% 
#  filter(station_code == "D24")
#yes but only during 2016 and 2017
#maybe they call it NZ068 after 2017

#nz068 <- phytoplankton_emp %>% 
#  filter(station_code =="NZ068")
#range(nz068$date) #"2017-05-19" "2022-12-15"
#so yeah, D24 was phased out and replaced with NZ068

#filter EMP data set to just the dates needed
phyto_emp_recent <- phytoplankton_emp_edi %>% 
  mutate(
    #add a column to indicate who collected the samples
    collected_by = "EMP"
    #create a month column
    ,month = as.numeric(month(date))
    #create a year column
    ,year = as.numeric(year(date))
  ) %>% 
  unite('program_station', c(collected_by,station),sep="_",remove=F) %>% 
  #only keep 2018 and beyond and July-Oct
  filter(year > 2017 & month > 6 & month < 11) %>% 
  glimpse()

#check date range
#range(phyto_emp_recent$date)
#looks good; "2018-07-09" "2022-10-20"

#look at list of stations
#unique(phyto_emp_recent$station)
#29 stations

#create list of EMP stations needed from the station metadata file
# stations_emp <- stations %>% 
#   filter(grepl("EMP",alias)) %>% 
#   pull(alias)

#add station lat/long to phyto data file; should join by station
phyto_emp_coords <- left_join(phyto_emp_recent, stn_emp) %>% 
  #create lat/long columns that combine the two sets of lat/long
  mutate(latitude3 = case_when(!is.na(latitude)~latitude,
                               TRUE~latitude2)
         ,longitude3 = case_when(!is.na(longitude)~longitude,
                                 TRUE~longitude2)) %>% 
  #drop the old lat/long columns
  select(-c(latitude,longitude,latitude2,longitude2)
         ,latitude = latitude3
         ,longitude = longitude3
  ) %>% 
  #there are a few samples still missing lat/long so drop them for now
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  #add geometry column
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude',y='latitude')
           ,crs = 4326 #EPSG code for WGS84
           ,remove = F
  )   %>%
  glimpse()

#are there any rows without lat/long?
#there shouldn't be
# phyto_emp_coords_na <- phyto_emp_coords %>% 
#   filter(is.na(latitude) | is.na(longitude)) %>% 
#   distinct(station,date,time)
#yes there are; mostly EZ samples that didn't have coordinates added by EMP
#exception is one sample from EMP_NZ328; probably a typo for NZ325 (Grizzly Bay station)

#write file to share with EMP
#write_csv(phyto_emp_coords_na,"./EDI/data_input/phytoplankton/phyto_emp_miss_coords.csv")

#Prepare shapefile for filtering data spatially

#look at coordinate reference system (CRS) of regions and basemap
#EDSM 2019 Phase 3 Subregions
# st_crs(R_EDSM_Subregions_19P3) #NAD83 / UTM zone 10N which is EPSG = 26910
# st_crs(WW_Delta) #NAD83 which is EPSG = 4269

#change CRS of both to match zoop data sets EPSG = 4326
ww_delta_4326 <- st_transform(WW_Delta, crs = 4326)
subregions_4326 <- st_transform(R_EDSM_Subregions_19P3, crs = 4326)

#make map
# (map_region_all<-ggplot()+
#     #CDFW Delta waterways
#     geom_sf(data = ww_delta_4326, fill= "lightblue", color= "black")+
#     #EDSM 2017-18 Phase 1 Strata
#     geom_sf(data = subregions_4326, aes(fill=SubRegion), alpha=0.8)+
#     #add title
#     ggtitle("R_EDSM_Subregions_19P3")+
#     theme_bw()
# )

#only keep the needed subregions

#create vector of subregions to keep
subregions_focal <- c("Suisun Marsh","Grizzly Bay","Mid Suisun Bay","Honker Bay","Confluence","Lower Sacramento River","Sacramento River near Rio Vista")

#categorize subregions into regions useful for SMSCG
regions_new <- as.data.frame(
  cbind(
    "Region_smscg" = c("Suisun Marsh",rep("Suisun Bay",3),rep("River",3))
    ,"SubRegion" = c("Suisun Marsh","Grizzly Bay","Mid Suisun Bay","Honker Bay","Confluence","Lower Sacramento River","Sacramento River near Rio Vista")
  )
)

region_focal <- subregions_4326 %>% 
  #just keep the needed subregions
  filter(SubRegion %in% subregions_focal) %>% 
  #add SMSCG regions which groups the subregions appropriately
  left_join(regions_new) %>% 
  group_by(Region_smscg) %>% 
  summarise(SQM = sum(SQM), do_union = TRUE)

#remake map with SMSCG regions  
# (map_region_focal<-ggplot()+
#     #CDFW Delta waterways
#     geom_sf(data= ww_delta_4326, fill= "lightblue", color= "black")+
#     #reduced region
#     geom_sf(data =region_focal, aes(fill=Region_smscg), alpha=0.8)+
#     #add title
#     ggtitle("Focal Region")+
#     theme_bw()
# )

#filter phyto data by region
phyto_emp_spatial_filter <- phyto_emp_coords %>% 
  #assign samples to regions
  st_join(region_focal,join = st_within) %>% 
  #drop any samples outside of focal region
  st_filter(region_focal) %>% 
  glimpse()

#look at program_stations
#unique(phyto_emp_spatial_filter$program_station)
#"EMP_D10"   "EMP_EZ2"   "EMP_EZ6"   "EMP_D8"    "EMP_NZS42" "EMP_NZ032" "EMP_D7"    "EMP_D22"   "EMP_NZ068" "EMP_D4"   

#let's look at which stations are retained, what region they were assigned, and and how many samples
# phyto_emp_freq <- table(phyto_emp_spatial_filter$Region_smscg,phyto_emp_spatial_filter$station)
#looks good

#format EMP data to then combine with DFW data
phyto_emp_format <- phyto_emp_spatial_filter %>% 
  #switched to spatial filter above so don't need to filter by station name
  #filter(station %in% stations_emp) %>% 
  #for now, add debris column with "Unknown" for all
  #hopefully, EMP will add this to their published dataset at some point
  add_column(debris = "Unknown") %>% 
  #drop geometry column
  st_set_geometry(NULL) %>% 
  #reorder and rename columns as needed
  select(station = program_station
         ,collected_by
         ,date 
         ,time_pst =time
         ,latitude
         ,longitude
         ,taxon_original = orig_taxon
         ,taxon
         ,kingdom:species
         ,units_per_ml = units_per_m_l
         ,cells_per_ml = cells_per_m_l
         ,biovolume_per_ml = average_biovolume_per_m_l
         ,gald_um = gald
         ,phyto_form
         ,quality_check
         ,debris
  ) %>% 
  glimpse()

#look at list of stations remaining
#unique(phyto_emp_format$station)
#"EMP_D10"   "EMP_D8"    "EMP_NZS42" "EMP_NZ032" "EMP_D7"    "EMP_D22"   "EMP_NZ068" "EMP_D4"    "EMP_D12"  
#looks good 

#look at data start date by station
# emp_start_date <- phyto_emp_format %>% 
#   group_by(station) %>% 
#   summarize(date_min = min(date))
#all start in 2018 as expected

#look at summary of samples for these stations
# #phyto_emp_samp_sum <- phyto_emp_format %>% 
#   distinct(station,date,time_pst) %>% 
#   group_by(station) %>% 
#   count()
#expecting 20 samples per station (4 months x 5 years)
#no missing samples

#combine EMP and SMSCG data sets---------------
# glimpse(phyto_emp_format) 
# glimpse(phyto_dfw_tax)
phyto_all <- bind_rows(phyto_emp_format,phyto_dfw_tax,phyto_emp_repo_tax) %>% 
  arrange(date,time_pst,station) %>% 
  mutate(
    #to be safe, change time to character for exporting data
    time_pst = as.character(time_pst)
    #also need to change some EMP quality check from Fragmented to Fragment
    ,quality_check2 = case_when(quality_check=="Fragmented" | quality_check=="Fragment"~"BrokenDiatoms",TRUE~quality_check)
    ) %>% 
  select(-quality_check) %>% 
  rename(quality_check = quality_check2) %>% 
  glimpse()

#Look at how well algal_group, kingdom, phylum, and class match
# tax_consist <- phyto_all %>% 
#   distinct(algal_group,kingdom,phylum,class,genus) %>% 
#   arrange(algal_group,kingdom,phylum,class,genus)
#write file
#write_csv(tax_consist,"./EDI/data_input/phytoplankton/phyto_high_tax_old.csv")

#look at genus Mallomonas
# mall <- phyto_all %>%
#   filter(genus=="Mallomonas")
#just one observation of Mallomonas sp., so rare enough doesn't matter what we do with this

#make some adjustments to taxonomy based on review of algaebase higher taxonomy (2023/10/12)
#https://www.algaebase.org/browse/taxonomy/
#refer to phyto_high_tax_old_corrected.csv
phyto_all_tax <- phyto_all %>% 
  mutate(
    #algal_group
    #replace Synurophytes with Chrysophytes
    #there was just one case from this group Mallomonas sp.
    algal_group2 = case_when(algal_group =="Synurophytes"~"Chrysophytes",TRUE~algal_group)
    #kingdom
    #change kingdom for dinoflagellates
    #change kingdom for cyanobacteria
    ,kingdom2 = case_when((kingdom=="Protozoa" & phylum=="Dinozoa")~"Chromista"
                          ,kingdom=="Bacteria"~"Eubacteria"
                          ,TRUE~kingdom)
    #phylum
    #Bacillariophyta and Ochrophyta have been replaced with Heterokontophyta
    #Dinozoa replaced with Miozoa
    #Euglenozoa replaced with Euglenophyta
    #replace Cryptophyta with Cryptista
    ,phylum2 = case_when((phylum =="Bacillariophyta" | phylum=="Ochrophyta")~"Heterokontophyta"
                        ,phylum=="Dinozoa"~"Miozoa"
                        ,phylum=="Euglenozoa"~"Euglenophyta"
                        ,phylum=="Cryptophyta"~"Cryptista"
                        ,TRUE~phylum
                        )
    #class
    #replace Prymnesiophyceae with Coccolithophyceae
    #replace Fragilariophyceae with Bacillariophyceae
    #replace Synurophyceae with 
    ,class2 = case_when(class =="Prymnesiophyceae"~"Coccolithophyceae"
                        ,class=="Fragilariophyceae"~"Bacillariophyceae"
                        ,class=="Synurophyceae"~"Chrysophyceae"
                        ,TRUE~class
    ),.after=algal_group
    #fix an error in one taxon 
    ,species2 = case_when(taxon =="Teleaulax amphioxeia" & taxon_original=="Plagioselmis prolonga" & species == "prolonga"~"amphioxeia"
                         ,TRUE~species
                         )
  ) %>% 
  select(-c(kingdom:algal_group,species)) %>% 
  rename(
    kingdom = kingdom2
    ,phylum = phylum2
    ,class = class2
    ,algal_group = algal_group2
    ,species = species2
  ) %>% 
  glimpse()

#Look again at how well algal_group, kingdom, phylum, and class match
# tax_consist2 <- phyto_all_tax %>% 
#   distinct(algal_group,kingdom,phylum,class) %>% 
#   arrange(algal_group,kingdom,phylum,class)
#looks like all my changes worked

#look for taxon that are NA; shouldn't be any
# phyto_all_taxon_na <- phyto_all_tax %>%
#  filter(is.na(taxon))
#none as expected

#look for "None" or "Unknown" in taxon_original
# phyto_all_tax_check <- phyto_all_tax %>% 
#   filter(taxon == "None" | taxon == "Unknown")
#none which is good because we want taxon to always be filled

#look at unique stations
#unique(phyto_all_tax$station) #looks good; 26 stations

#look for NAs in data set
# phyto_all_tax_na <- phyto_all_tax %>% 
#   summarise(across(everything(), ~ sum(is.na(.))))
#only taxon_original and debris columns have NAs which is fine

#create taxonomy file for SMSCG EDI package
phyto_tax_final <- phyto_all_tax %>% 
  distinct(
    taxon
    ,taxon_original
    ,genus
    ,species
    ,algal_group
    ,kingdom
    ,phylum
    ,class
  ) %>% 
  arrange(kingdom,phylum,class,genus,species,taxon) %>% 
  glimpse()

#write the output data file for SMSCG EDI
#write_csv(phyto_tax_final, "./EDI/data_output/smscg_phytoplankton_taxonomy.csv")

#create version for SMSCG EDI package
#will later drop the EMP stations back out because EMP will provide all their data for PESP separately
phyto_smscg <- phyto_all_tax %>% 
  select(
    station
    ,collected_by 
    ,latitude 
    ,longitude 
    ,date
    ,time_pst
    ,taxon_original
    ,taxon 
    # ,algal_group
    # ,kingdom:class
    # ,genus
    # ,species
    ,units_per_ml
    ,cells_per_ml
    ,biovolume_per_ml
    ,gald_um
    ,phyto_form
    ,quality_check
    ,debris #only in SMSCG data set which is fine because we will drop EMP data anyway
    ) %>% 
  glimpse()

#write the output data file for SMSCG EDI
#write_csv(phyto_smscg, "./EDI/data_output/smscg_phytoplankton_samples_2020-2023.csv")

#recombine taxonomy and abunance data to make sure it works correctly
#phyto_all_tax_recomb <- left_join(phyto_smscg,phyto_tax_final) 
#2966 instead of 2924 rows (42 extra created when recombining data frames)
#fixed one error in taxonomy so now there are no duplicates

#look for duplicates of station, date, time, taxon, taxon_original
# find_dups <- phyto_all_tax_recomb %>% 
#   group_by(station, date, time_pst, taxon, taxon_original) %>% 
#   filter(n()>1) %>% 
#   summarize(n=n())
#in all cases, there are two copies of a record Teleaulax amphioxeia

#look at this taxon's records more closely
# ta_check <- phyto_all_tax_recomb %>% 
#   filter(taxon =="Teleaulax amphioxeia")
#for some reason, we have two records that have taxon == Teleaulax amphioxeia and taxon_original == Plagioselmis prolonga
#one is genus == Teleaulax and species == amphioxeia (correct)
#the other is genus == Teleaulax and species == prolonga (incorrect)
#for now just do a quick change from incorrect to correct species (done above)


