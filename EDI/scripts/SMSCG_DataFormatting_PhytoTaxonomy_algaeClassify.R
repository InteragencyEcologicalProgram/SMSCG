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














