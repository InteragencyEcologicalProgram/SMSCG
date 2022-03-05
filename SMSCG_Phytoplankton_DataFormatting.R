#Suisun Marsh Salinity Control Gate
#Phytoplankton Data 2020-2021
#Format raw data in preparation for visualization and analysis

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(hms) #working with date/time
library(readxl) #importing data from excel files
#library(algaeClassify) #grab taxonomy info from AlgaeBase; doesn't work currently

#missing times from a few 2021 samples; could request the times from DFW to make sure records are complete

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
#round up higher taxonomy data for the 12 new genera present in the 2021 data set

# 1. Read in the data----------------------------------------------

#read in taxonomy data
#this probably needs to be updated with each new batch of data
#update this file with the updates/corrections I got from AlgaeBase 2/24/2022
taxonomy <- read_excel(path = "Data/phytoplankton/PhytoplanktonTaxonomy_2022-02-09.xlsx")

#Create character vectors of all 2020 phytoplankton files
#five from EMP and one from DFW
phyto_files20 <- dir(path = "Data/phytoplankton/2020", pattern = "\\.xlsx", full.names = T)

#Create character vectors of all 2021 phytoplankton files
#five from EMP and one from DFW
#doing this separately from 2020 because there's an extra column in these files
#the "Full Code" column is the FLIMS code; relevant to EMP samples but not DFW samples
phyto_files21 <- dir(path = "Data/phytoplankton/2021", pattern = "\\.xlsx", full.names = T)

#Combine all of the 2020 sample data files into a single df
#specify format of columns because column types not automatically read consistently among files
column_type20<-c(rep("text",4),rep("numeric",10),rep("text",5),rep("numeric",5),rep("text",5)
               ,rep("numeric",10),"text",rep("numeric",26)) 

phytoplankton20 <- phyto_files20 %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  map_dfr(~read_excel(.x, col_types = column_type20), .id = "source") %>% 
  #reduce file name to just the needed info (ie, survey)
  mutate(survey = as.factor(str_sub(source,25,27))) %>% 
  glimpse()
#succeeded in combining all the sample files
#but date and time are in weird format
#also the sampling depth column has three variations: "Depth (m)", "Depth (ft.)", "Depth (ft)"
#so when the files are combined, there are two extra depth columns added

#Combine all of the 2021 sample data files into a single df
#specify format of columns because column types not automatically read consistently among files
#accounts for extra column in 2021 files
column_type21<-c(rep("text",5),rep("numeric",10),rep("text",5),rep("numeric",5),rep("text",5)
                 ,rep("numeric",10),"text",rep("numeric",26)) 
phytoplankton21 <- phyto_files21 %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  map_dfr(~read_excel(.x, col_types = column_type21), .id = "source") %>% 
  #reduce file name to just the needed info (ie, survey)
  mutate(survey = str_sub(source,25,27)) %>% 
  glimpse()
#succeeded in combining all the sample files
#but date and time are in weird format

#combine the 2020 and 2021 data sets
#bind_rows can handle the fact that not all columns will match between data sets
phytoplankton <- bind_rows(phytoplankton20,phytoplankton21) %>% 
  clean_names() %>% 
  glimpse()
#the three non-matching columns get kicked to the end of the combined df

#format the sample data set------------

phyto_cleanest <- phytoplankton %>% 
  #rename the confusingly incorrectly name column
  rename(total_cells=number_of_cells_per_unit) %>% 
  #subset to just the needed columns
  select(survey
         , station_code
         , sample_date
         , sample_time
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
  #remove empty rows created by linear cell measurement rows (length, width, depth)  
  #a little tricky just because the survey name appears in every row including the othewise empty ones
  #chose the genus and taxon column as the ones to check for missing data
  drop_na(genus:taxon) %>%   
  rowwise() %>% 
  mutate(
    #format date
    date = as.Date(as.numeric(sample_date),origin = "1899-12-30")
    #format time
    ,time = as_hms(as.numeric(sample_time)*60*60*24)
    #create new column that calculates mean biovolume per cell
    ,mean_cell_biovolume = mean(c_across(biovolume_1:biovolume_10),na.rm=T)
    #create new column that calculates organisms per mL
    #different from cells per mL because some organisms are multicellular
    ,organisms_per_ml = (unit_abundance*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted)
    #,organisms_per_ml_easy = (unit_abundance*factor)
    #create new column that calculates cells per mL
    ,cells_per_ml = (total_cells*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted)
    #,cells_per_ml_easy = (total_cells*factor)
    #create a column that calculates biovolume per mL
    #units for biovolume are cubic microns; old version is incorrect calculations
    #,biovolume_per_ml_old = organisms_per_ml * total_cells * mean_cell_biovolume
    ,biovolume_per_ml = (total_cells* mean_cell_biovolume*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted)
    #,biovolume_per_ml_easy = factor * total_cells * mean_cell_biovolume
    ) %>% 
  #simplify column names
  rename(station = station_code
         ,phyto_form =  colony_filament_individual_group_code) %>% 
  #subset and reorder columns again to just those needed
  select(survey
         ,station
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
#should check the NAs for dates and times; should be some NAs for time but not date

#look at station names
unique(phyto_cleanest$station)
#station names need to be cleaned up
#why is NA present?

#clean up station names
#delete all spaces in names
#then delete "STN" and "FMWT" from names
#change all "MONT" to "MON"
#fix case of NZS42 incorrectly called NZ542

# Making data frame with existing strings and their replacement
stnm <- data.frame(target = c(" ","STN","FMWT","MONT","NZ542"),
                 replacement = c("","","","MON","NZS42"))

# Making the named replacement vector from stnm
replacements <- c(stnm$replacement)
names(replacements) <- c(stnm$target)

#fix the names
phyto_clean_stnm <- phyto_cleanest %>% 
  mutate(station_clean = str_replace_all(station,pattern = replacements)) 

#look at station names again
unique(phyto_clean_stnm$station_clean)
#all the relevant station names look fixed now

#look at number of samples per station
samp_count<-phyto_clean_stnm %>% 
  distinct(station_clean, date) %>% 
  group_by(station_clean) %>% 
  summarize(count = n())
#there is one NA but there shouldn't be any

#look closer at rows with NA for station
station_na<-phyto_clean_stnm %>% 
  filter(is.na(station))
#There are two rows with the value 4 for biovolume_2 but are otherwise blank rows
#maybe values were typed into wrong row?

#check for NAs
check_na <- phyto_clean_stnm[rowSums(is.na(phyto_clean_stnm)) > 0,]
#most of these rows aren't needed for SMSCG data set
#of the rows that are relevant, most are just missing time, which is fine

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
names(phyto_clean_stnm)
names(taxon_high)
phyto_tax<-left_join(phyto_clean_stnm,taxon_high)

#reorder columns once more for data frame export
phyto_final<-phyto_tax %>% 
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

#write the formatted data as csv 
#write_csv(phyto_final,file = "Data/phytoplankton/SMSCG_phytoplankton_formatted_2020-2021.csv")
#NOTE: the time look fine in df in R but is wrong when viewed in exported csv

#write version of data set that includes only the samples collected by DFW
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



            
  
  
  
  
  
  
  
  
