#Suisun Marsh Salinity Control Gate
#Phytoplankton Data
#Format raw data in preparation for visualization and analysis

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(hms) #working with date/time
library(readxl) #importing data from excel files
#library(algaeClassify) #grab taxonomy info from AlgaeBase; doesn't work currently

#to do list
#round up higher taxonomy data for the 12 new genera present in the 2021 data set

# 1. Read in the Data----------------------------------------------

#read in taxonomy data
#this probably needs to be updated with each new batch of data
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
phytoplankton20 <- map_dfr(phyto_files20, ~read_excel(.x, col_types = column_type20))
#glimpse(phytoplankton20)
#succeeded in combining all the sample files
#but date and time are in weird format
#also the sampling depth column has three variations: "Depth (m)", "Depth (ft.)", "Depth (ft)"
#so when the files are combined, there are two extra depth columns added

#Combine all of the 2021 sample data files into a single df
#specify format of columns because column types not automatically read consistently among files
#accounts for extra column in 2021 files
column_type21<-c(rep("text",5),rep("numeric",10),rep("text",5),rep("numeric",5),rep("text",5)
                 ,rep("numeric",10),"text",rep("numeric",26)) 
phytoplankton21 <- map_dfr(phyto_files21, ~read_excel(.x, col_types = column_type21))
#glimpse(phytoplankton21)
#succeeded in combining all the sample files
#but date and time are in weird format

#combine the 2020 and 2021 data sets
#bind_rows can handle the fact that not all columns will match between data sets
phytoplankton <- bind_rows(phytoplankton20,phytoplankton21)
#glimpse(phytoplankton) 
#the three non-matching columns get kicked to the end of the combined df

#format the sample data set------------

#format date and time columns
phytoplankton$SampleDate2<-as.Date(as.numeric(phytoplankton$SampleDate),origin = "1899-12-30")
#converted numeric excel dates to date format; compared with some original data and looks correct
phytoplankton$SampleTime2<-as_hms(as.numeric(phytoplankton$SampleTime)*60*60*24)
#checked some formatted times against original data and it looks like they converted correctly
#glimpse(phytoplankton)

phyto_cleanest <- phytoplankton %>% 
  #subset to just the needed columns
  select("StationCode"
         , "SampleDate2"
         , "SampleTime2"
         , "Genus"
         , "Taxon"
         , "Colony/Filament/Individual Group Code"
         , "Unit Abundance"
         , "Slide/ Chamber Area (mm²)"
         , "Volume Analyzed (mL)"
         , "Field-of-view (mm²)"
         , "Number of Fields Counted"
         , "Factor"
         , "Number of cells per unit"
         , "Biovolume 1":"Biovolume 10") %>% 
  #remove empty rows created by linear cell measurement rows (length, width, depth)  
  remove_empty(which = "rows") %>% 
  rowwise() %>% 
  mutate(
    #create new column that calculates mean biovolume per cell
    mean_cell_biovolume = mean(c_across(`Biovolume 1`:`Biovolume 10`),na.rm=T)
    #create new column that calculates organisms per mL
    ,organisms_per_ml = (`Unit Abundance`*`Slide/ Chamber Area (mm²)`)/(`Volume Analyzed (mL)`*`Field-of-view (mm²)`*`Number of Fields Counted`)
    #create a column that calculates biovolume per mL
    #units for biovolume both for individual cells and per mL is cubic microns
    ,biovolume_per_ml = organisms_per_ml * `Number of cells per unit` * mean_cell_biovolume) %>% 
  #simplify column names
  rename(station = StationCode
         ,date = SampleDate2 
         ,time = SampleTime2
         ,genus = Genus
         ,taxon = Taxon
         ,phyto_form =  "Colony/Filament/Individual Group Code") %>% 
  #subset and reorder columns again to just those needed
  select("station"
         , "date"
         ,"time"
         ,"genus"
         ,"taxon"                              
         ,"phyto_form"           
         , "organisms_per_ml"
         ,"biovolume_per_ml"
  ) %>% 
  glimpse()
#organisms per ml can also be calculated by simply multiplying factor by unit abundance
#Though it requires more calculations, I think I prefer to use the formula based on the more raw
#version of the data rather than the one based on the factor column which is a derived column
#phyto_cleaner$organisms_per_ml_alt<-(phyto_cleaner$Factor*phyto_cleaner$`Unit Abundance`)

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
#There are two rows with the value 4 for Biovolume2 but are otherwise blank rows
#maybe values were typed into wrong row?

#check for NAs
check_na <- phyto_clean_stnm[rowSums(is.na(phyto_clean_stnm)) > 0,]
#most of these rows aren't needed for SMSCG data set
#of the rows that are relevant, most are just missing time, which is fine

#Add higher level taxonomic information using the algaeClassify package--------
#NOTE: couldn't get the functions from this package that interface with AlgaeBase to work
#Package hasn't been updated in over two years and AlgaeBase has launched a new website since then
#emailed the package author on 12/13/2021

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
  select("Kingdom"
  ,"Phylum"
  ,"Class"
  ,"Genus"
  ,"Algal Type") %>%  #confirmed that there is just one type per genus
#this approach is very simple because it removes the need for exact matches in the taxon names
#part of the difficulty in matching taxon names is presence of "cf." for many taxa
#unfortunately by just matching by genus and not taxon, we lose the habitat type, and salinity range info
#also with species level data, there's a chance the taxonomy dataset doesn't include every species in the samples
  rename(kingdom = Kingdom
         ,phylum = Phylum
         ,class = Class
         ,genus = Genus
         ,common_name = 'Algal Type')%>% 
#remove some (likely incorrect) combinations that are creating duplicates for genus
  filter(!(genus == "Achnanthidium" & class =="Fragilariophyceae") & 
           !(genus == "Elakatothrix" & class =="Chlorophyceae") &
           !(genus == "Leptocylindrus" & common_name =="Centric diatom"))

#condense taxonomy data set to just the unique combinations
taxon_high_cond<-unique(taxon_high[,c('kingdom','phylum','class','genus','common_name')])
#initially some genera appeared more than once in this taxonomy data set
#but this has been corrected

#investigating duplicates for genus---------------

#count number of times each genus appears
#ideally this would be once
tax_gen_sum<-data.frame(table(taxon_high_cond$genus)) 

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
names(taxon_high_cond)
phyto_tax<-left_join(phyto_clean_stnm,taxon_high_cond)

#reorder columns once more for data frame export
#decided to drop the common names column here too
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
         ,biovolume_per_ml
  ) %>%
  glimpse()

#write the formatted data as csv 
#write_csv(phyto_final,file = "Data/phytoplankton/SMSCG_phytoplankton_formatted_2020-2021.csv")
#NOTE: the time look fine in df in R but is wrong when viewed in exported csv


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



            
  
  
  
  
  
  
  
  
