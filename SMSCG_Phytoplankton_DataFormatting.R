#SMSCG
#Format raw data for data visualization and analysis

#required packages
library(tidyverse)
library(janitor)
library(readxl) #importing data from excel files
library(lubridate) #formatting dates


# 1. Import Data from SharePoint (under construction) ----------------------------------------------------------
# Dataset is on SharePoint site for the Seasonal Monitoring Report

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data/Phytoplankton"
  )
)  

#Create character vectors of all phytoplankton files
#includes six files of sample data and one file with higher level taxonomy info
phyto_files <- dir(sharepoint_path, pattern = "\\.xlsx", full.names = T)

#Separate the taxonomy file from the sample data files
samp_files <- phyto_files[!str_detect(phyto_files, "Taxonomy")] 
taxon_file <- phyto_files[str_detect(phyto_files, "Taxonomy")] 

#Read the taxonomy file
taxonomy <- map_dfr(taxon_file, read_excel)

#Combine all of the sample data files
#getting error that SampleTime isn't matching among files
#need to specify format of columns
columns<-c("guess","text","text",rep("guess",63)) #still need to work on this
phytoplankton <- map_dfr(samp_files, ~read_excel(.x, col_types = columns))
#succeeded in combining all the sample files
#but date and time are non-sense
#also the sampling depth column has three variations: "Depth (m)", "Depth (ft.)", "Depth (ft)"

phytoplankton <- map_dfr(samp_files, ~read_excel(.x, col_types = "text"))
glimpse(phytoplankton)

phytoplankton$SampleDate2<-as_date(as.numeric(phytoplankton$SampleDate))
phytoplankton$SampleTime2<-as_hms(as.numeric(phytoplankton$SampleTime)*60*60*24)

phytoplankton2 <- map_dfr(samp_files, ~read_excel(.x, col_types = cols(.default = "guess", SampleDate = "date")))


glimpse(phytoplankton)


#start formatting the data set------------

phyto_cleaner <- phytoplankton %>% 
  #subset to just the needed columns
  select("StationCode"
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
         , "Biovolume 1":"Biovolume 10") %>% 
  #remove empty rows created by linear cell measurement rows (length, width, depth)  
  remove_empty(which = "rows") %>% 
  #create new column that formats date as a date type instead of date-time
  mutate(Date = format(ymd(SampleDate), format = "%Y-%m-%d")) %>% 
  #create new column that calculates mean biovolume per cell
  rowwise() %>% 
  mutate(mean_cell_biovolume = mean(c_across(`Biovolume 1`:`Biovolume 10`),na.rm=T))



#look at data structure
glimpse(phyto_cleaner)
#everything looks fine except the time, which includes nonsense years
#we will want to keep the time info
#probably even want a date-time column

#look at station names
unique(phyto_cleaner$StationCode)
#n=14 statons; looks like the names were entered consistently; check to make sure they are all the correct stations

#format the time column
phyto_cleaner$SampleTime2<-format(phyto_cleaner$SampleTime, format = "%H:%M:%S")
#should add some code to detect any NAs in time

#create new column that calculates organisms per mL
phyto_cleaner$organisms_per_ml<-(phyto_cleaner$`Unit Abundance`*phyto_cleaner$`Slide/ Chamber Area (mm²)`)/
  (phyto_cleaner$`Volume Analyzed (mL)`*phyto_cleaner$`Field-of-view (mm²)`*phyto_cleaner$`Number of Fields Counted`)

#organisms per ml can also be calculated by simply multiplying factor by unit abundance
#Though it requires more calculations, I think I prefer to use the formula based on the more raw
#version of the data rather than the one based on the factor column which is a derived column
#phyto_cleaner$organisms_per_ml_alt<-(phyto_cleaner$Factor*phyto_cleaner$`Unit Abundance`)
  

#create a column that calculates biovolume per mL
#organisms per ml * cells per unit abundance * mean biovolume per cell
#units for biovolume both for individual cells and per mL is cubic microns
phyto_cleaner$biovolume_per_ml<-phyto_cleaner$organisms_per_ml * 
  phyto_cleaner$"Number of cells per unit" * phyto_cleaner$mean_cell_biovolume


#rename, subset, and reorder needed columns---------- 

phyto_cleanest<-phyto_cleaner %>%
  #simplify column names
  rename(station = StationCode
         ,date = Date 
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
         )


#Add higher level taxonomic information and habitat information-------------

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
#unfortunately by just matching by genus and not taxon, we loose the habitat type, and salinity range info
#also with species level data, there's a chance the taxonomy dataset doesn't include every species in the samples
  rename(kingdom = Kingdom
         ,phylum = Phylum
         ,class = Class
         ,genus = Genus
         ,algal_type = 'Algal Type')%>% 
#remove some (likely incorrect) combinations that are creating duplicates for genus
  filter(!(genus == "Achnanthidium" & class =="Fragilariophyceae") & 
           !(genus == "Elakatothrix" & class =="Chlorophyceae") &
           !(genus == "Leptocylindrus" & algal_type =="Centric diatom"))

#condense taxonomy data set to just the unique combinations
taxon_high_cond<-unique(taxon_high[,c('kingdom','phylum','class','genus','algal_type')])
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

#combine sample data and high level taxonomy----------
names(phyto_cleanest)
names(taxon_high_cond)
phyto_tax<-left_join(phyto_cleanest,taxon_high_cond)

#reorder columns once more for data frame export
phy_final<-phyto_tax[,c(1:3,9:12,6,4,5,7,8)]


#write the formatted data file to a csv
#figure out how to save to SharePoint site
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



            
  
  
  
  
  
  
  
  
