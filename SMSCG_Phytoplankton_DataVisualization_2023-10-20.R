#Suisun Marsh Salinity Control Gate Action
#Phyto community composition across space and time

#Nick Rasmussen
#2023-10-20

#to do list-------
#keep in mind that 2018-2019, don't have enhanced sampling; it's just EMP and they don't sample east marsh
#stacked bar plots of algal groups by date and station
#stacked bar plot of algal groups by month and region
#maybe drop a few samples based on the quality check column
#need to assign samples to survey numbers; especially important for deciding when EMP and DFW samples should be combined
#for now, just group samples by region and month
#look at AWCA plots to see how I did this

#Packages------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(ggvegan)
library(vegan)
library(ggpubr)

#read in data------------

#read in phyto abundance data from SMSCG data package on EDI
abund <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=8c283ea7d1823824ccfa2f05d8056027")

#read in phyto taxonomy data from SMSCG data package on EDI
taxon <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=4c514654c71a7b8b930bdd031701c359")

#read in phyto station metadata from SMSCG data package on EDI
region <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=08de2a97cf2a3743af06e3ff6e0e9b39")

#format the station metadata file----------------

#drop lat/long because those are already in abundance data set
region_format <- region %>% 
  select(station, region) %>% 
  glimpse()

#format taxonomy data set------------------

#there are three variations for this one taxon; remove the ones we don't want
taxon_format <- taxon %>% 
  filter(!(taxon == "Teleaulax amphioxeia" & species=="prolonga")
         & !(taxon == "Teleaulax amphioxeia" & is.na(taxon_original))
         )

#add the taxonomy and station metadata to abundance data----------

#add taxonomy
at <- left_join(abund,taxon_format)

#add regions
atr <- left_join(at,region_format) %>% 
  #add month and year columns
  mutate(
    year = year(date),
    month = month(date)
  ) %>% 
  glimpse()

#summarize biovolume by algal group---------------

alg_grp_biov <- atr %>% 
  group_by(region,year,month,algal_group) %>% 
  summarise(total_biovolume = sum(biovolume_per_ml)) %>% 
  arrange(year,month,region) %>% 
  glimpse()

#stacked barplots of biovolume by station and date-------------

#stacked bar plot
(plot_alg_grp_rm <- ggplot(alg_grp_biov, aes(x = month, y = total_biovolume, fill = algal_group))+
   geom_bar(position = "stack", stat = "identity") + 
   facet_wrap(year~region,ncol = 4)
   )
#needs work
#should use a survey number instead of date 
#should start by plotting just the stations in a given region
#then maybe group bars by year

#stacked bar plot: log transformed
(plot_alg_grp_rm <- ggplot(alg_grp_biov, aes(x = month, y = log(total_biovolume), fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)
  
#percent stacked bar plot
(plot_alg_grp_rm_perc <- ggplot(alg_grp_biov, aes(x = month, y = total_biovolume, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)

#summarize biovolume by genus and filter rare taxa-----------------
#quick summary for rare taxa removal
#if we keep only those present in at least 1% of samples, there are 55 taxa remaining of 105 total taxa
#if we keep only those that comprise at least 1% of total biovolume, there are 16 taxa remaining
#if we group the biovolume by algal group, only 5 of 9 groups comprise at least 1% of total biovolume

#first see how many unique genera are in the data set
genera <- unique(atr$genus) #105, probably too many to use in an NMDS analysis

#summarize biovolume by genus
genus_bv <- atr %>% 
  group_by(station,region,date,month,year,algal_group,genus) %>% 
  summarise(biovolume_gn = sum(biovolume_per_ml),.groups = 'drop')

#count the total number of samples
samples <- genus_bv %>% 
  distinct(station,date) %>% 
  count()
tot_samp <- as.numeric(samples[1,1])

#sum all the biovolume
tot_biovolume <-sum(genus_bv$biovolume_gn)

#count how many samples each genus occurs in
samples_genus <- genus_bv %>% 
  count(genus) %>% 
  mutate(sample_perc = (n/tot_samp)*100) %>% 
  arrange(-n)
#if we keep only those present in at least 1% of samples, there are 55 taxa

#sum biovolume by genus and look at what percent of biovolume each comprises
genus_rank <- genus_bv %>% 
  group_by(algal_group,genus) %>% 
  summarize(biovolume_gn_tot = sum(biovolume_gn)) %>% 
  arrange(-biovolume_gn_tot) %>% 
  mutate(biovolume_perc = (biovolume_gn_tot/tot_biovolume)*100)
#if we keep only those that comprise at least 1% of total biovolume, there are 16 taxa

#keep only genera that comprise at least 1% of total biovolume
genus_rare_bv1 <- genus_rank %>% 
  filter(biovolume_perc >= 1)

#keep only genera present in at least 1% of samples
genus_rare_ct1 <- samples_genus %>% 
  filter(sample_perc >= 1)

#now create list of remaining genera to use for filtering main dataset
genus_rare_ct1_list <- genus_rare_ct1 %>% 
  pull(genus)

#now use this list of taxa to filter main data set
genus_bv_ct1 <- genus_bv %>% 
  filter(genus %in% genus_rare_ct1_list)

#look at total biovolume by algal group
group_rank <- genus_bv %>% 
  group_by(algal_group) %>% 
  summarize(biovolume_grp_tot = sum(biovolume_gn)) %>% 
  mutate(biovolume_grp_perc = (biovolume_grp_tot/tot_biovolume)*100) %>% 
  arrange(-biovolume_grp_tot)
#only 5 of 9 groups comprise at least 1% of total biovolume
#most of biovolume is diatoms

#then try removing rare taxa (eg, present in fewer than 1% of samples)
#need total number of samples and number of samples each genus is present in
#could also do this by biovolume (ie, drop taxa that comprise less than 5% of total biovolume)
  
#NMDS plots: dropped genera in fewer than 1% of samples-----------------

#start with genus level data with taxa removed that are in fewer than 1% of samples
#this was the approach that retained the most taxa (n=55)

#first create properly formatted abundance data and predictors data frames
genus_ct1 <- genus_bv_ct1 %>% 
  #drop unneeded column 
  select(-algal_group) %>% 
  #convert long to wide; fill missing data with zeros
  pivot_wider(id_cols = c(station:year), names_from = genus, values_from = biovolume_gn,values_fill = 0)

#abundances
genus_ct1_abund <- genus_ct1 %>% 
  select(-(c(station:year)))

#predictors
genus_ct1_pred <- genus_ct1 %>% 
  select(c(station:year))
  
#transform community data with hellinger transformation; give more thought to best way of transforming data
genus_ct1_abund_hel <- decostand(genus_ct1_abund,method="hellinger")

#run the nmds
genus_ct1_nmds <- metaMDS(genus_ct1_abund_hel,autotransform = F)
#stress = 0.2704573 

#basic plot
ordiplot(genus_ct1_nmds)
ordiplot(genus_ct1_nmds,type="t") #adds labels for samples and genera

#this won't run
autoplot(genus_ct1_nmds)

#this won't run either
#full control with fortified ordination output
fort <- fortify(genus_ct1_nmds)
ggplot() +
  geom_point(data=subset(fort, Score == "sites"),
             mapping = aes(x=NMDS1, y=NMDS2),
             colour="black",
             alpha=0.5)+
  geom_segment(data=subset(fort,Score == "species"),
               mapping = aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               arrow=arrow(length=unit(0.015, "npc"),
                           type="closed"),
               colour="darkgrey",
               size=0.8)+
  geom_text(data=subset(fort,Score == "species"),
            mapping=aes(label=Label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept=0, slope=0, linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))



