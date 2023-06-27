#SMSCG
#phytoplankton
#taxonomy
#algaeClassify package


#NOTE: this requires a key, which was provided for free for package review
#would need to buy one for future use

#load packages
library(algaeClassify)
library(viridis)
library(tidyverse)
library(janitor)

#added API key to environment variable
#file.edit("~/.Renviron")
#ALGAEBASE_APIKEY=yourKeyHere

#start with installing package and running example code-----------------

#library(devtools)
#install_github("vppatil/algaeClassifyV2",ref="main",dependencies=TRUE,force = T)
#should be algaeClassify v 2.0.0
#if asked to update packages, choose 3 (none) or hit enter to skip updates
#got a warning but ignored it
#WARNING: Rtools is required to build R packages, but is not currently installed.
#Please download and install Rtools 4.2 from https://cran.r-project.org/bin/windows/Rtools/ or https://www.r-project.org/nosvn/winutf8/ucrt3/.

#citation
#citation("algaeClassify")

#check out the package
#help(package="algaeClassify")

#View the new algaebase search functions:
#help("algaebase_species_search")
#help("algaebase_genus_search")
#help("algaebase_search_df")

#You can search for a single genus
algaebase_genus_search(genus="Anabaena")

algaebase_species_search("Anabaena","flos-aquae")

#There are several arguments for these functions.
#you can control whether to include higher taxonomy in the output
algaebase_genus_search(genus="Navicula",higher=TRUE)

#You can also choose to include the full species name with author and date in 
#output
algaebase_genus_search(genus="Navicula",higher=TRUE,long=TRUE)

#The default only returns exact matches and the most recent entry in algaebase,
#but you can override that behavior
algaebase_species_search(genus="Nitzschia",species="acicularis",
                         newest.only=FALSE,exact.matches.only=FALSE,long=TRUE)

algaebase_genus_search(genus="Cyclotella",higher=TRUE,print.full.json=TRUE)


data(lakegeneva) #load small example dataset
head(lakegeneva) #view example dataset

lakegeneva<-genus_species_extract(lakegeneva,phyto.name="phyto_name")
lakegeneva<-lakegeneva[!duplicated(lakegeneva$phyto_name),]

lakegeneva.algaebase<-algaebase_search_df(lakegeneva,higher=TRUE,
                                          genus.name="genus",species.name="species")

head(lakegeneva.algaebase)

#ITIS
genus_search_itis(genus="Mougeotia",higher=TRUE)

species_search_itis(genspp="Anabaena flos-aquae")

#GNR (Global Names Resolver)
#GNR/taxize use fuzzy/partial matching, and search multiple databases.
#If you do not find a hit in algaebase or ITIS, you can try searching for a 
#partial match via GNR

name<-"Anabaena flos-aquae"
gnr_simple(name=name,sourceid=3) #Search ITIS
gnr_simple(name=name,sourceid=NULL) #search for matches from any source

species_search_itis(genspp="Anabaena flosaquae") #check itis f0r gnr best match.

traits_to_csr(sav=0.2,msv=10,traitrange=traitranges)

traits_to_mfg(flagella = 1,size = "large",colonial = 1,filament = 0,
              centric = NA,gelatinous = 0,aerotopes = 0,
              class = "Euglenophyceae",order = "Euglenales")

data("csrTraits")
data("mfgTraits")
head(csrTraits)
head(mfgTraits)


data("species_mfg_library")
head(species_mfg_library)

species_to_mfg('Scenedesmus','bijuga')

data(lakegeneva)
lakegeneva<-genus_species_extract(lakegeneva,phyto.name='phyto_name')
lakegeneva.mfg<-species_to_mfg_df(lakegeneva)

head(lakegeneva.mfg)

data(mfg_csr_library)
mfg_csr_convert(mfg="11a-NakeChlor")


data(lakegeneva) #load the demonstration dataset

accum(lakegeneva,phyto_name='phyto_name',column='biovol_um3_ml',n=100,
      datename='date_dd_mm_yy',dateformat='%d-%m-%y')

#clean up binomial names and extract genus and species to new columns
lakegeneva<-genus_species_extract(lakegeneva,phyto.name='phyto_name')

#aggregate abundance data to genus level
lakegeneva.genus<-phyto_ts_aggregate(lakegeneva,SummaryType='abundance',
                                     AbundanceVar='biovol_um3_ml',
                                     GroupingVar1='genus')

#plot accumulation curve again, but at genus level
accum(lakegeneva.genus,phyto_name='genus',column='biovol_um3_ml',n=100,
      datename='date_dd_mm_yy',dateformat='%Y-%m-%d')

#classify taxa to CSR and visualize relative abundance of CSR groups by month
data(lakegeneva)

lakegeneva<-genus_species_extract(lakegeneva,phyto.name='phyto_name')
lakegeneva<-species_to_mfg_df(lakegeneva)
lakegeneva<-mfg_csr_convert_df(lakegeneva,mfg='MFG')
csrAbundance.by.month<-date_mat(lakegeneva,abundance.var='biovol_um3_ml',
                                taxa.name='CSR',time.agg='month')

#make a simple heatmap of mean daily csr group abundance by month
heatmap(csrAbundance.by.month,Rowv=NA,Colv=NA,col=viridis(10))


#test package with SMSCG phyto community data------------

#to do list
#should probably add columns to indicate if "cf." was present before genus or before species

#read in the SMSCG taxonomy file
taxonomy <- read_csv("./EDI/data_input/phytoplankton/PhytoplanktonTaxonomy_2022-02-09.csv") %>% 
  clean_names() %>% 
  glimpse()

#look at taxa with "unknown" for genus
#need to use an includes unknown type of filter
tax_unk <- taxonomy %>% 
  filter(str_detect(genus, 'Unknown|unknown'))
#all are "Unknown with capital "U"
#for now, just drop these taxa

#format df in prep for algaebase search
tax_format <- taxonomy %>% 
  #drop taxa with unknown in genus
  filter(str_detect(genus, ('Unknown|unknown'),negate=T)) %>% 
  mutate(
    #create new column for taxon that has sp., cf., var. and fo. removed
    #had to put "." in [], otherwise doesn't do replacement correctly (eg, any case of "sp" in strings is replaced with space)
    taxon_simple =str_replace_all(taxon, pattern = c('cf[.] ' = '',' cf[.]'="",' var[.]' = "", ' fo[.]' = ""," sp[.]"="","2"=""))
    ) %>% 
  #move new taxon column next to old one
  relocate(taxon_simple,.after=taxon) %>% 
  #create separate columns for genus and specific epithet (also variety and form)
  separate_wider_delim(cols = taxon_simple, delim =" ",names=c("genus2","species","var","form"),too_few="align_start",cols_remove = F) %>% 
  glimpse()

#does new genus column always equal original genus column?
tax_genus <- tax_format %>% 
  mutate(genus_check = case_when(genus==genus2~1,TRUE~0)) %>% 
  filter(genus_check==0)
#one case where there is probably a typo in original genus column
#the rest are cases where a genus was in the original genus column and taxon was left blank instead of usual "[Genus] sp."
#lets fix the one typo and then just use original genus column

#look at all taxa that have varieties and forms in name
tax_var <- tax_format %>% 
  filter(!is.na(var))
#n = 24

#create vector of genus and species for those with varieties
#we will search for these in the algaebase output file
tax_var_sp <- tax_var %>% 
  #combine genus and species
  unite(col = "input.name",c(genus,species),sep=" ",remove=F) %>% 
  #create vector of names
  pull(input.name)

#try algaebase search
#taxonomy_algaebase<-algaebase_search_df(tax_format,higher=TRUE, long=T, exact.matches.only = F
 #                                         ,genus.name="genus",species.name="species")
#took 52 minutes to complete
#found 1286 matches to the 1107 names so 179 extra matches

#write ouput file
#write_csv(taxonomy_algaebase,"./EDI/data_input/phytoplankton/phyto_taxonomy_algaebase_2023-06-27.csv")

#look at output file----------------

#look at options for taxonomic status
unique(taxonomy_algaebase$taxonomic.status)
#"currently accepted taxonomically"                     "homotypic or heterotypic synonym"                    
#"uncertain taxonomic status subjected to verification" "preliminary, not subjected to verification"          
# NA

#look at summary for taxonomic status
tax_status <- taxonomy_algaebase %>% 
  group_by(taxonomic.status) %>% 
  summarize(n = n(),.groups = 'drop') %>% 
  arrange(-n)
#most taxa are accepted, many are synonyms
#some are uncertain, a few preliminary
#two NA

#look closer at the two rarest groups for taxonomic status
tax_status_rare <- taxonomy_algaebase %>% 
  filter(taxonomic.status=="preliminary, not subjected to verification" | is.na(taxonomic.status)) %>% 
  arrange(taxonomic.status,input.name)
#so two taxa were not found at all; at least one is due to typo in genus

#look up the genus with typo corrected (Surirella, not Surirell)
algaebase_species_search("Surirella","suecica")
#yes, corrected name is in algaebase

#The other unfound taxon is due to the "ë"
algaebase_species_search("Terpsinoë","musica") #found
algaebase_species_search("Terpsinoe","musica") #not found

#look at taxa without higher level taxonomy
tax_nohigh <- taxonomy_algaebase %>% 
  filter(is.na(kingdom))
#oddly, most are accepted names but still lack higher level taxonomy info

#look at cases where input.match = 1 and currently.accepted=1
tax_perfect <- taxonomy_algaebase %>% 
  filter(input.match==1 & currently.accepted==1)
#661 of 1107 so 60%

#look at cases where input.match = 1 and currently.accepted=1
tax_input_name <- taxonomy_algaebase %>% 
  filter(input.match==0 & currently.accepted==1)

#I noticed that in some cases, I simplified a name from a variety to a species
#the algaebase search then indicated that I did not have the accepted name and provided the variety
#can we either include variety in the species column or have a third input column for variety?

algaebase_species_search("Fragilaria","vaucheriae var. capitellata") #no exact match found
algaebase_species_search("Fragilaria","vaucheriae") #finds match and says this is accepted name
algaebase_species_search("Fragilaria","capitellata") #no match

#look at set of taxa that had varieties that I dropped to do algaebase search
ab_var <- taxonomy_algaebase %>% 
  filter(input.name %in% tax_var_sp)




