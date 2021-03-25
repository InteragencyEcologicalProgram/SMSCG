#SMSCG
#Format raw data for data visualization and analysis

#required packages
library(tidyverse)
library(janitor)
library(hms) #formatting times
library(readxl) #importing data from excel files


# 1. Import Data from SharePoint (under construction) ----------------------------------------------------------
# Dataset is on SharePoint site for the Seasonal Monitoring Report

# Define path on SharePoint site for data
#sharepoint_path <- normalizePath(
#  file.path(
#    Sys.getenv("USERPROFILE"),
#    "California Department of Water Resources/SMSCG - Summer Action - Data/Phytoplankton"
#  )
#)  

# Import data
#load(file = file.path(sharepoint_path, "SMSCG_Phytoplankton_2020.xlsx"))


#if that didin't work
#load(file = file.path("data","BayStudyFish.RData"))

#set working directory for date on OneDrive--------

setwd("C:/Users/nrasmuss/OneDrive - California Department of Water Resources/SMSCG/Phyto/Data_2020")

#import and combine all sample files at once (under construction)------------

#these two lines of code work fine to read the files into R
#samp_file_list <- list.files(pattern = "*.Samples.*\\.xlsx") 
#samp_df_list <- lapply(samp_file_list, read_excel) 

#combine the files of all the sample data (EMP: n=5, SMSCG: n=1)
#none of this works
#I think the formatting of the date and time columns are part of the problem
#also there are lots of mostly empty rows because of the way the files are formatted
#.id = "id" adds a column with the name of the original file
#emp<-bind_rows(samp_df_list, .id = "id")
#phytoplankton <- map_df(samp_df_list, read_xlsx, .id = "id")
#phytoplankton <- map(samp_file_list, ~ read_excel(.x, col_types = cols(.default = "?", SampleDate = "date", SampleTime="date")))


#Import individual files from OneDrive-------------

#import phytoplankton sample data
phytoplankton<-read_excel("DFW_Phyto_Samples_2020_all.xlsx")

#import taxonomic info to be combined with sample data
taxonomy<-read_excel("PhytoplanktonTaxonomy_2021-03-19.xlsx")


#look at number of different taxonomists
#if more than one, this likely introduces variation in phytoplankton IDs
unique(phytoplankton$Taxonomist)
#just one taxonomist

#subset to just the needed columns
phyto_thinner<-phytoplankton[,c(
  "StationCode"
  , "SampleDate"
  , "SampleTime"
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
  , "Biovolume 1"
  , "Biovolume 2"
  , "Biovolume 3"
  , "Biovolume 4"
  , "Biovolume 5"
  , "Biovolume 6"
  , "Biovolume 7"
  , "Biovolume 8"
  , "Biovolume 9"
  , "Biovolume 10"
)]

#remove empty rows
#lots of these because the initial linear cell measurements were put on different rows (length, width, depth)
#we just need the biovolume calculated from the linear measurements
phyto_shorter<-remove_empty(phyto_thinner, which = "rows")

#look at data structure
glimpse(phyto_shorter)
#everything looks fine except the time, which includes nonsense years
#we will want to keep the time info
#probably even want a date-time column

#look at station names
unique(phyto_shorter$StationCode)
#n=14 statons; looks like the names were entered consistently; check to make sure they are all the correct stations

#look at unique dates to make sure they make sense after the autoformatting
unique(phyto_shorter$SampleDate)
#looks correct though they are labeled UTC when they should be Pacific Daylight(?) Time

#should add some code to detect any NAs in date

#format the time column
phyto_shorter$SampleTime2<-as_hms(phyto_shorter$SampleTime)
#should add some code to detect any NAs in time

#create new column that calculates organisms per mL
phyto_shorter$organisms_per_ml<-(phyto_shorter$`Unit Abundance`*phyto_shorter$`Slide/ Chamber Area (mm²)`)/
  (phyto_shorter$`Volume Analyzed (mL)`*phyto_shorter$`Field-of-view (mm²)`*phyto_shorter$`Number of Fields Counted`)

#organisms per ml can also be calculated by simply multiplying factor by unit abundance
#Though it requires more calculations, I think I prefer to use the formula based on the more raw
#version of the data rather than the one based on the factor column which is a derived column
#phyto_shorter$organisms_per_ml_alt<-(phyto_shorter$Factor*phyto_shorter$`Unit Abundance`)
  
#create new column that calculates mean biovolume per cell
phyto_shorter<-phyto_shorter %>% 
  rowwise() %>% 
  mutate(mean_cell_biovolume = mean(c_across(`Biovolume 1`:`Biovolume 10`),na.rm=T))

#create a column that calculates biovolume per mL
#organisms per ml * cells per unit abundance * mean biovolume per cell
phyto_shorter$biovolume_per_ml<-phyto_shorter$organisms_per_ml * 
  phyto_shorter$"Number of cells per unit" * phyto_shorter$mean_cell_biovolume


#rename needed columns so they are simpler
names(phyto_shorter)

phyto<-phyto_shorter %>%
  rename(station = StationCode
         ,date = SampleDate 
         ,time = SampleTime2
         ,genus = Genus
         ,taxon = Taxon
         ,phyto_form =  "Colony/Filament/Individual Group Code"
  )

#reorder columns and subset again to just the needed columns
phy<-phyto[,c(
  "station"
  , "date"
  ,"time"
  ,"genus"
  ,"taxon"                              
  ,"phyto_form"           
  , "organisms_per_ml"
  ,"mean_cell_biovolume"  
  ,"biovolume_per_ml" 
)]

#Add higher level taxonomic information and habitat information-------------

#names(taxonomy)

#subset to just the needed columns
#will match genus name between the two data frames
taxon_high<-taxonomy[,c(
  "Kingdom"
  ,"Phylum"
  ,"Class"
  ,"Genus"
  ,"Algal Type" #confirmed that there is just one type per genus
)]
#this approach is very simple because it removes the need for exact matches in the taxon names
#part of the difficulty in matching taxon names is presence of "cf." for many taxa
#unfortunately by just matching by genus and not taxon, we loose the habitat type, and salinity range info
#also with species level data, there's a chance the taxonomy dataset doesn't include every species in the samples

#rename columns
taxon_high_rn<-taxon_high %>%
  rename(kingdom = Kingdom
         ,phylum = Phylum
         ,class = Class
         ,genus = Genus
         ,algal_type = 'Algal Type'
  )

#remove some combinations that are creating duplicates for genus
taxon_high_fx<-taxon_high_rn %>% 
  filter(!(genus == "Achnanthidium" & class =="Fragilariophyceae") & 
           !(genus == "Elakatothrix" & class =="Chlorophyceae") &
           !(genus == "Leptocylindrus" & algal_type =="Centric diatom")
  )

#condense taxonomy data set to just the unique combinations
taxon_high_cond<-unique(taxon_high_fx[,c('kingdom','phylum','class','genus','algal_type')])

#looks like some genera appear more than once in this taxonomy data set
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

#combine sample data and high level taxonomy
names(phy)
names(taxon_high_cond)
phyto_tax<-left_join(phy,taxon_high_cond)

#reorder columns once more for data frame export
phy_final<-phyto_tax[,c(1:3,10:13,6,4,5,7:9)]


#write the formatted data file to a csv
#write_csv(phy_final,"SMSCG_phytoplankton_formatted_2020.csv")



#exploring taxonomy data set--------------------

#how many genera in samples not in taxonomy data?
#look for NA in Class column
sum(is.na(phyto_tax$Class))
#none so no genera in these samples that aren't in taxonomy data


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



            
  
  
  
  
  
  
  
  
