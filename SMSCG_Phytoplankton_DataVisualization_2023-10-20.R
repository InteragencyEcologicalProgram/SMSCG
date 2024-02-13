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
library(ggordiplots) #use this to make nmds plots with centroids & ellipses
library(ggpubr)
library(scales) #log scale on plots
library(car) #anova tables for GLM
library(ggforce) #making matrix of correlation plots

#read in data------------

#read in phyto abundance data from SMSCG data package on EDI
abund <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=8c283ea7d1823824ccfa2f05d8056027")

#read in phyto taxonomy data from SMSCG data package on EDI
taxon <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=4c514654c71a7b8b930bdd031701c359")

#read in phyto station metadata from SMSCG data package on EDI
region <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=08de2a97cf2a3743af06e3ff6e0e9b39")

#read in LCEFA data
#Pulled from Galloway and Winder 2015
#Table 2: LCEFA mean as % cell dry mass
lcefa <- read_csv("./EDI/data_output/smscg_phytoplankton_lcefa.csv") %>% 
  select(-lcefa_group)

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

#look at unique algal_group
taxon_high_combos <- taxon_format %>% 
  distinct(algal_group,phylum)

#look at genera in Haptophytes, Euglenoids and Chrysophytes
#don't have good data on carbon mass (Menden-Deuer and Lessard 2000) 
#and/or LCEFA for these taxa (Galloway and Winder 2015; not even in Table S2)
taxon_no_lcefa <- taxon_format %>% 
  filter(algal_group=="Euglenoids" | algal_group=="Chrysophytes" | algal_group=="Haptophytes")
#we will just need to drop these taxa from analysis for LCEFA
#Euglenoids are rank 5 of 9 on total biovolume, so sucks to drop them
#Chrysophytes are rank 8 of 9 so not such a big deal
#Haptophytes are rank 9 of 9 so again not much of a loss


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

# Convert biovolume to biomass using relationships from Menden-Deuer and Lussard
# (2000) doi: 10.4319/lo.2000.45.3.0569
# Only relevant to group-level data
# note this study didn't include Euglenoids
# also very limited data for Chrysophytes and Chlorophytes and slope for Cryptophytes not different from zero
# Units of BV.Density are um^3 per L

#estimate biomass of carbon based on estimated biovolume
atr_mass <- atr %>% 
  mutate(
    #add mean cell biovolume (um^3/cell) column (biovolume_per_ml/cells_per_ml)
    mean_cell_biovolume = biovolume_per_ml/cells_per_ml,.before = gald_um
    #Calculate Biomass (pg-C per cell) from Biovolume (um^3 per cell)
    #Use one equation for diatoms and another equation for everything else based on Menden-Deuer & Lussard 2000
    ,biomass_pg_c = case_when((algal_group == "Centric Diatoms" | algal_group=="Pennate Diatoms") ~ 0.288 * (mean_cell_biovolume^0.811)
                                  ,!(algal_group == "Centric Diatoms" | algal_group=="Pennate Diatoms") ~ 0.216 * (mean_cell_biovolume^0.939))
    #now convert Convert pg to ug (ug-C per L)
    ,biomass_ug_c = biomass_pg_c / 10^6
    #Calculate Biomass Density (ug-C per mL)
    ,biomass_ug_c_ml = biomass_ug_c * cells_per_ml
    #convert from ug/ml to ug/l because final units for biomass and LCEFA will be per liter
    ,biomass_ug_c_l = biomass_ug_c_ml*1000
         ) %>% 
  #drop some unneeded columns
  select(-c(mean_cell_biovolume:biomass_ug_c_ml)) %>% 
  glimpse()

## Calculate LCEFA composition based on method in Galloway & Winder (2015) 
## doi: 10.1371/journal.pone.0130053
## values are LCEFA as % of algal dry weight 

#summarize sample data by algal group and add estimated LCEFA content
#note that we don't have values for chrysophytes or euglenoids; will need to drop those for LCEFA plots
alg_grp_biov_samp <- atr_mass %>% 
  group_by(region,station,year,month,date,algal_group) %>% 
  summarize(across(units_per_ml:biomass_ug_c_l, ~sum(.x, na.rm = TRUE)),.groups='drop') %>% 
  #add the LCEFA content info
  left_join(lcefa) %>% 
  mutate(
    #use the biomass estimates to estimate LCEFA content; divide by 100 because LCEFA values are percentages
    #not sure it was necessary to convert from ug/ml to ug/l
    lcefa_per_l = biomass_ug_c_l * lcefa_value / 100
  ) %>% 
  glimpse()

#summarize biovolume, biomass, and LCEFA by sample (across algal groups) 
#note that chrysophytes and euglenoids are excluded from LCEFA data so values for all of them are zero
alg_grp_biov_summary <- alg_grp_biov_samp %>% 
  group_by(region,station,year,month,date) %>% 
  summarise(across(c(biovolume_per_ml,biomass_ug_c_l,lcefa_per_l), ~sum(.x, na.rm = TRUE)),.groups='drop') %>% 
  arrange(year,month,region,station) %>% 
  glimpse()
  

#summarize biovolume, biomass, and LCEFA by region and month and algal group
#note that chrysophytes and euglenoids are excluded from LCEFA data so values for all of them are zero
alg_grp_biov <- alg_grp_biov_samp %>% 
  group_by(region,year,month,algal_group) %>% 
  summarise(across(c(biovolume_per_ml,biomass_ug_c_l,lcefa_per_l), ~sum(.x, na.rm = TRUE)),.groups='drop') %>% 
  #add a new column that lumps together less common algal groups into "other" category
  mutate(algal_group_adj = case_when(algal_group == "Chrysophytes" | algal_group == "Haptophytes" ~ "Other"
                                     ,TRUE ~ algal_group), .after = algal_group) %>% 
  rename(total_biovolume = biovolume_per_ml
         ,total_biomass = biomass_ug_c_l
         ,total_lcefa = lcefa_per_l
         ) %>% 
  arrange(year,month,region) %>% 
  glimpse()
unique(alg_grp_biov$algal_group_adj)

#look at cases of negative values when biomass is log transformed
alg_grp_biov_log <- alg_grp_biov %>% 
  mutate(total_biomass_log =log(total_biomass),.after = total_biomass) %>% 
  arrange(total_biomass_log)
#some values for biomass are > 1 so log is negative
#could change units so that doesn't happen
#eg, pg/l instead of ug/l

#plot correlations among biovolume, biomass,LCEFA
alg_grp_biov_summary %>% 
  select(biovolume_per_ml,biomass_ug_c_l,lcefa_per_l) %>% 
  ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_point(size = 1) +
  facet_matrix(
    vars(everything()), 
    layer.diag = FALSE
  ) +
  theme(axis.text.x = element_text(angle = 90))
#pretty strongly correlated overall; one outlier

#look at outlier 
phyto_outlier <- alg_grp_biov_summary %>% 
  filter(lcefa_per_l > 30)
#STN 602 July 2022

#look closer at composition of outlier
#high levels of Thalassiosira sp. during both surveys at this station in July 2022
#STN 602, FMWT 602, D7
#don't have June data included in this data set but could get it from EMP
phyto_outlier_comp <- atr_mass %>% 
  filter((station =="STN_602" | station=="FMWT_602" | station=="EMP_D7") 
         & year==2022 
         & genus=="Thalassiosira"
         )
#no evidence of this taxon in any other samples at that station in 2022

#stacked barplots of biovolume by algal group, region and month-------------

#nine is too many groups to show in stacked bar plot, lump some into "other"
#sum biovolume by algal group and order from high to low
biov_rank <- alg_grp_biov %>% 
  group_by(algal_group) %>% 
  summarise(grand_biovol = sum(total_biovolume)) %>% 
  arrange(grand_biovol)
#can lump Chrysophytes and Haptophytes into other
#all other groups have at least one case in which they were good chunk of biovolume in a given month/region
#so reduces from 9 to 8 groups

#pull algal_group levels in order from biov_rank
algal_group_rank <- biov_rank %>% 
  pull(algal_group)

#set order of algal groups based on contribution to biovolume
alg_grp_biov$algal_group <- factor(alg_grp_biov$algal_group, levels=algal_group_rank)


#sum biovolume by adjusted algal group and order from high to low
biov_rank_adj <- alg_grp_biov %>% 
  group_by(algal_group_adj) %>% 
  summarise(grand_biovol = sum(total_biovolume)) %>% 
  arrange(grand_biovol)

#pull algal_group_adj levels in order from biov_rank
algal_group_adj_rank <- biov_rank_adj %>% 
  pull(algal_group_adj)

#set order of adjusted algal groups based on contribution to biovolume
alg_grp_biov$algal_group_adj <- factor(alg_grp_biov$algal_group_adj, levels=algal_group_adj_rank)

#stacked bar plot of raw biovolume
(plot_alg_grp_rm <- ggplot(alg_grp_biov, aes(x = month, y = total_biovolume, fill = algal_group))+
   geom_bar(position = "stack", stat = "identity") + 
   facet_wrap(year~region,ncol = 4)
   )
#needs work
#should use a survey number instead of date 
#should start by plotting just the stations in a given region
#then maybe group bars by year

#stacked bar plot: log transformed biovolume, all regions and years
(plot_alg_grp_rm <- ggplot(alg_grp_biov, aes(x = month, y = log(total_biovolume), fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)

#stacked bar plot: log transformed biovolume, 2020-2022, no FLO
(plot_alg_grp_rm_recent <- alg_grp_biov %>% 
    filter(year>2019 & region!="FLO") %>% 
    ggplot(aes(x = month, y = log(total_biovolume), fill = algal_group_adj))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(year~region,ncol = 3)
)
#ggsave(plot=plot_alg_grp_rm_recent,"Plots/Phytoplankton/smscg_phyto_stacked_bar_tot.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

  
#percent stacked bar plot: all regions and years
(plot_alg_grp_rm_perc <- ggplot(alg_grp_biov, aes(x = month, y = total_biovolume, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)

#stacked bar plot: all algal groups, biovolume, 2020-2022, no FLO
(plot_alg_grp_rm_perc_recent <- alg_grp_biov %>% 
    filter(year>2019 & region!="FLO") %>% 
    ggplot(aes(x = month, y = total_biovolume, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity",color = "black") + 
    facet_wrap(year~region,ncol = 3)
)

#stacked bar plot: rare algal grouped lumped into "other", biovolume, 2020-2022, no FLO
(plot_alg_grp_rm_perc_recent_adj <- alg_grp_biov %>% 
    filter(year>2019 & region!="FLO") %>% 
    ggplot(aes(x = month, y = total_biovolume, fill = algal_group_adj))+
    geom_bar(position = "fill", stat = "identity",color = "black") + 
    facet_wrap(year~region,ncol = 3)+
    labs(x = "Month", y = "Proportion Biovolume")+
  scale_fill_discrete(name = "Algal Group")
)
#ggsave(plot=plot_alg_grp_rm_perc_recent_adj,"Plots/Phytoplankton/smscg_phyto_stacked_bar_perc.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#plot composition by region and year (not month)
alg_grp_biov_ry <- alg_grp_biov %>% 
  group_by(year,region,algal_group) %>% 
  summarize(tot_bvol = sum(total_biovolume),.groups = 'drop') %>% 
  filter(region!="FLO" & year>2019)

#percent stacked bar plot: all regions and years, grouped by region
(plot_alg_grp_ry_perc_r <- ggplot(alg_grp_biov_ry, aes(x = year, y = tot_bvol, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(region~.)
)

#percent stacked bar plot: all regions and years, grouped by year
(plot_alg_grp_ry_perc_y <- ggplot(alg_grp_biov_ry, aes(x = region, y = tot_bvol, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    labs(x = "Region", y = "Proportion of biovolume")+
    facet_wrap(year~.)
)
#ggsave(plot=plot_alg_grp_ry_perc_y,"Plots/Phytoplankton/smscg_phyto_stacked_bar_perc_bvol.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)


#total stacked bar plot: all regions and years
(plot_alg_grp_ry_tot_biov <- ggplot(alg_grp_biov_ry, aes(x = year, y = tot_bvol, fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(region~.)
)

#stacked barplots of biomass by algal group, region and month-------------
#fairly similar looking to biovolume overall
#diatoms have less carbon per unit volume so biomass is less dominated by diatoms so other taxa can be seen
#but biomass is an even rougher estimation than biovolume

#ten is too many groups to show in stacked bar plot, lump some into "other"
#sum biomass by algal group and order from high to low
biom_rank <- alg_grp_biov %>% 
  group_by(algal_group) %>% 
  summarise(grand_biomass = sum(total_biomass)) %>% 
  arrange(-grand_biomass)
#can lump Chrysophytes and Haptophytes into other

#stacked bar plot of raw biomass
(plot_alg_grp_rm_bm <- ggplot(alg_grp_biov, aes(x = month, y = total_biomass, fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)

#stacked bar plot: log transformed biomass, all regions and years
(plot_alg_grp_rm_bm_log <- ggplot(alg_grp_biov, aes(x = month, y = log(total_biomass), fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)
#some values are negative when log transformed because they are > 1

#stacked bar plot: log transformed biomass, 2020-2022, no FLO
(plot_alg_grp_rm_recent_bm_log <- alg_grp_biov %>% 
    filter(year>2019 & region!="FLO") %>% 
    ggplot(aes(x = month, y = log(total_biomass), fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(year~region,ncol = 3)
)
#ggsave(plot=plot_alg_grp_rm_recent,"Plots/Phytoplankton/smscg_phyto_stacked_bar_tot_biomass.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)


#percent stacked bar plot: all regions and years
(plot_alg_grp_rm_perc_bm <- ggplot(alg_grp_biov, aes(x = month, y = total_biomass, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)

#stacked bar plot: biomass, 2020-2022, no FLO
(plot_alg_grp_rm_perc_recent <- alg_grp_biov %>% 
    filter(year>2019 & region!="FLO") %>% 
    ggplot(aes(x = month, y = total_biomass, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(year~region,ncol = 3)
)
#ggsave(plot=plot_alg_grp_rm_perc_recent,"Plots/Phytoplankton/smscg_phyto_stacked_bar_perc_biomass.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#plot composition by region and year (not month)
alg_grp_biov_ry_bm <- alg_grp_biov %>% 
  group_by(year,region,algal_group) %>% 
  summarize(tot_bmass = sum(total_biomass),.groups = 'drop') %>% 
  filter(region!="FLO" & year>2019)

#percent stacked bar plot: all regions and years
(plot_alg_grp_ry_perc_bm <- ggplot(alg_grp_biov_ry_bm, aes(x = year, y = tot_bmass, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(region~.)
)


#stacked barplots of LCEFA by algal group, region and month-------------
#note that chrysophytes and euglenoids are excluded from LCEFA data so values for all of them are zero

#ten is too many groups to show in stacked bar plot, lump some into "other"
#sum LCEFA by algal group and order from high to low
biolcefa_rank <- alg_grp_biov %>% 
  group_by(algal_group) %>% 
  summarise(grand_lcefa = sum(total_lcefa)) %>% 
  arrange(-grand_lcefa)

#stacked bar plot of raw biomass
(plot_alg_grp_rm_fa <- ggplot(alg_grp_biov, aes(x = month, y = total_lcefa, fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)

#percent stacked bar plot: all regions and years
(plot_alg_grp_rm_perc_fa <- ggplot(alg_grp_biov, aes(x = month, y = total_lcefa, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(year~region,ncol = 4)
)

#stacked bar plot: biomass, 2020-2022, no FLO
(plot_alg_grp_rm_perc_recent_fa <- alg_grp_biov %>% 
    filter(year>2019 & region!="FLO") %>% 
    ggplot(aes(x = month, y = total_lcefa, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(year~region,ncol = 3)
)
#fairly similar plot to the one for biovolume
#obviously this one is missing the Euglenoids which are sometimes significant parts of biovolume
#main real difference is that cyanobacteria drops in importance in this plot because of low levels of LCEFA (eg, RIV 2021, Bay July 2020)

#plot composition by region and year (not month)
alg_grp_biov_ry_fa <- alg_grp_biov %>% 
  group_by(year,region,algal_group) %>% 
  summarize(tot_fa = sum(total_lcefa),.groups = 'drop') %>% 
  filter(region!="FLO" & year>2019)

#percent stacked bar plot: all regions and years
(plot_alg_grp_ry_perc_fa <- ggplot(alg_grp_biov_ry_fa, aes(x = year, y = tot_fa, fill = algal_group))+
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(region~.)
)

#total stacked bar plot: all regions and years
(plot_alg_grp_ry_tot_fa <- ggplot(alg_grp_biov_ry_fa, aes(x = year, y = tot_fa, fill = algal_group))+
    geom_bar(position = "stack", stat = "identity") + 
    facet_wrap(region~.)
)

#Boxplots of total phyto biovolume--------------

#sum biovolume across all taxa within sample
tot_biov <- atr %>% 
  group_by(region,station,year,month,date) %>% 
  #convert from cubic microns per mL to cubic mm per mL
  summarise(total_biovolume = sum(biovolume_per_ml)/1000000000) %>% 
  arrange(year,region,month) %>% 
  #lets drop some regions (FLO) and years (2018,2019)
  filter(region!="FLO" & year > 2019) %>%
  mutate(month = as.factor(month)
         ,year = as.factor(year)
         ,region = as.factor(region)
         ,station = as.factor(station)
         ) %>% 
  glimpse()

#count sample sizes by year and region
sample_totals_ry <- tot_biov %>% 
  group_by(year,region) %>% 
  count()
#most samples in marsh, a bit less in river, much less in bay (especially in 2020)

#box plots by month, year, region
plot_total_bvol_recent <- ggplot(data=tot_biov, aes(x = month, y = total_biovolume)) + 
  geom_boxplot(fill="darkolivegreen")+
  facet_grid(year~region)+
  labs(x = "Month"
       , y = bquote("Biovolume"~(mm^3~mL^-1) ) #convert to these different units above
  )+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(sides="l")+
  theme(legend.position="none"
        #, axis.text.x = element_text(angle = 90, hjust = 1)
  )
#ggsave(plot=plot_total_bvol_recent,"Plots/Phytoplankton/smscg_phyto_boxplot_total_biovolume.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#log biovolume: box plots by year, region
#need log transformation because of high outlier in Bay in July 2022
(plot_total_bvol_recent_ry <- ggplot(data=tot_biov, aes(x = year, y = total_biovolume,fill=region)) + 
  geom_boxplot()+
  labs(x = "Year"
       , y = bquote("Biovolume"~(mm^3~mL^-1) ) #convert to these different units above
  )+
  scale_fill_manual(values = c("#CCBFFF", "#B2FF8C", "#FFFF99"))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(sides="l")
  # theme(legend.position="none"
  #       #, axis.text.x = element_text(angle = 90, hjust = 1)
  # )
)
#ggsave(plot=plot_total_bvol_recent_ry,"Plots/Phytoplankton/smscg_phyto_boxplot_total_biovolume_year_region_allregions.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#untransformed data: box plots by year, region (Marsh and River only)
(plot_total_bvol_recent_ry_mr <- tot_biov %>% 
    filter(region!="BAY") %>% 
    ggplot(aes(x = year, y = total_biovolume,fill=region)) + 
    geom_boxplot()+
    scale_fill_manual(values = c("#B2FF8C", "#FFFF99"))+
    labs(x = "Year"
         , y = bquote("Biovolume"~(mm^3~mL^-1) ) #convert to these different units above
    )
    )
#ggsave(plot=plot_total_bvol_recent_ry_mr,"Plots/Phytoplankton/smscg_phyto_boxplot_total_biovolume_year_region_mar_riv.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)





#Boxplots of LCEFA--------------

#sum LCEFA across all taxa within sample
#note this doesn't include Euglenoids or Chrysophytes
tot_fa <- alg_grp_biov_samp %>% 
  group_by(region,station,year,month,date) %>% 
  summarise(total_lcefa = sum(lcefa_per_l,na.rm = T)) %>% 
  arrange(year,region,month) %>% 
  #lets drop some regions (FLO) and years (2018,2019)
  filter(region!="FLO" & year > 2019) %>%
  mutate(month = as.factor(month)
         ,year = as.factor(year)
         ,region = as.factor(region)
         ,station = as.factor(station)
  ) %>% 
  glimpse()


#box plots by month, year, region
(plot_total_fa_recent <- ggplot(data=tot_fa, aes(x = month, y = total_lcefa)) + 
  geom_boxplot(fill="darkolivegreen")+
  facet_grid(year~region)+
  labs(x = "Month"
       , y = bquote("LCEFA"~(ug~L^-1) )
  )+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+
  theme(legend.position="none"
        #, axis.text.x = element_text(angle = 90, hjust = 1)
  )
)
#ggsave(plot=plot_total_fa_recent,"Plots/Phytoplankton/smscg_phyto_boxplot_total_lcefa.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)
#NOTE: quite similar to total biovolume

#log biovolume: box plots by year, region
#need log transformation because of high outlier in Bay in July 2022
(plot_total_fa_recent_ry <- ggplot(data=tot_fa, aes(x = year, y = total_lcefa,fill=region)) + 
    geom_boxplot()+
    labs(x = "Year"
         , y = bquote("LCEFA"~(ug~L^-1)  ) #convert to these different units above
    )+
    scale_fill_manual(values = c("#CCBFFF", "#B2FF8C", "#FFFF99"))+
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) + 
    annotation_logticks(sides="l")
  # theme(legend.position="none"
  #       #, axis.text.x = element_text(angle = 90, hjust = 1)
  # )
)
#ggsave(plot=plot_total_fa_recent_ry,"Plots/Phytoplankton/smscg_phyto_boxplot_total_lcefa_year_region_allregions.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#untransformed data: box plots by year, region (Marsh and River only)
(plot_total_fa_recent_ry_mr <- tot_fa %>% 
    filter(region!="BAY") %>% 
    ggplot(aes(x = year, y = total_lcefa,fill=region)) + 
    geom_boxplot()+
    scale_fill_manual(values = c("#B2FF8C", "#FFFF99"))+
    labs(x = "Year"
         , y = bquote("LCEFA"~(ug~L^-1) ) #convert to these different units above
    )
)
#ggsave(plot=plot_total_fa_recent_ry_mr,"Plots/Phytoplankton/smscg_phyto_boxplot_total_lcefa_year_region_mar_riv.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)



#GLM of total biovolume----------------

#drop BAY from data set because mostly undersampled compared to other regions
tot_biov_nobay <- tot_biov %>% 
  filter(region!="BAY") %>% 
  glimpse()

#analysis with log transformed response
mod_tbiov_log_full <- glm(log(total_biovolume) ~ region * month* year, data = tot_biov_nobay)

#model checking plots
#plot(mod_tbiov_log_full)
#plots look pretty good

#look at model results
summary(mod_tbiov_log_full)
#Anova(mod_tbiov_log_full) #not working
#three way interaction not significant so drop it

#model with two way interactions
mod_tbiov_log_twoway = glm(log(total_biovolume) ~ region + month + year + region:month + region:year, data = tot_biov_nobay)
summary(mod_tbiov_log_twoway)
drop1(mod_tbiov_log_twoway, test="Chi")
#the two way interactions aren't significant

#additive model
mod_tbiov_log_oneway = glm(log(total_biovolume) ~ region + month + year, data = tot_biov_nobay)
#plot(mod_tbiov_log_oneway)
summary(mod_tbiov_log_oneway)
drop1(mod_tbiov_log_oneway, test="Chi")
#years are different but not month or region

#anova version of analysis
mod_tbiov_log_oneway_aov = aov(log(total_biovolume) ~ region + month + year, data = tot_biov_nobay)
Anova(mod_tbiov_log_oneway_aov)

#multiple comparison
#determine which years differ
TukeyHSD(mod_tbiov_log_oneway_aov)
#2020 vs 2021: p = 0.0001693
#2020 vs 2022: p = 0.0000009
#2021 vs 2022: p = 0.4269629

#generate effect sizes for total phyto by year
effsz<-tot_biov_nobay %>% 
  group_by(year) %>% 
  summarize(
    bvol_mean = mean(total_biovolume)
    ,bvol_sd = sd(total_biovolume)
    , .groups = 'drop')

#total phyto: compare 2020 vs 2021
effsz$bvol_mean[1]/effsz$bvol_mean[2] #2.043231

#total phyto: compare 2020 vs 2022
effsz$bvol_mean[1]/effsz$bvol_mean[3] #2.142461


#summarize biovolume by genus and filter rare taxa-----------------
#will use this to data set for NMDS plots

#quick summary for rare taxa removal
#if we keep only those present in at least 1% of samples, there are 51 taxa remaining of 91 total taxa
#if we keep only those that comprise at least 1% of total biovolume, there are 20 taxa remaining
#if we group the biovolume by algal group, only 5 of 9 groups comprise at least 1% of total biovolume

#subset to just keep 2020-2022 and marsh and river regions
#earlier years and other regions have much less data
atr_sub <- atr %>% 
  filter(year > 2019 & (region == "RIV" | region == "MAR")) %>% 
  glimpse()

#how many unique genera are in the data set
genera <- unique(atr_sub$genus) #91, probably too many to use in an NMDS analysis

#summarize biovolume by genus
genus_bv <- atr_sub %>% 
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
#if we keep only those present in at least 1% of samples, there are 51 taxa

#sum biovolume by genus and look at what percent of biovolume each comprises
genus_rank <- genus_bv %>% 
  group_by(algal_group,genus) %>% 
  summarize(biovolume_gn_tot = sum(biovolume_gn)) %>% 
  arrange(-biovolume_gn_tot) %>% 
  mutate(biovolume_perc = (biovolume_gn_tot/tot_biovolume)*100)
#if we keep only those that comprise at least 1% of total biovolume, there are 20 taxa

#create df with both count and biovolume rank info
all_rank <- left_join(genus_rank,samples_genus) %>% 
  select(-c(biovolume_gn_tot,n)) %>% 
  arrange(-biovolume_perc)
#write_csv(all_rank,"./Data/smscg_phyto_genera_rank.csv")

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

#NMDS plots: dropped genera in fewer than 1% of samples-----------------
#quick summary of results: region, month, year don't explain much variation; year x region do the best but still only explain 7% variation

#used the ggordiplots package for plots with ellipses for groups
#https://john-quensen.com/wp-content/uploads/2020/12/Ordiplots_with_ggplot.html

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
  select(c(station:year)) %>% 
  #add column with combo of year and region
  unite('year_region',c(year,region),remove = F) %>% 
  #make columns factors
  mutate(across(c(year_region:year),as.factor)) %>% 
  glimpse()

#transform community data with hellinger transformation; give more thought to best way of transforming data
genus_ct1_abund_hel <- decostand(genus_ct1_abund,method="hellinger")

#run the nmds
genus_ct1_nmds <- metaMDS(genus_ct1_abund_hel,autotransform = F)
#stress = 0.2752382 which is probably high 
#maybe try transforming the data; didn't make any difference to stress

#ggordiplot: ellipses for regions
gg_ordiplot(genus_ct1_nmds,groups = genus_ct1_pred$region, pt.size = 3)

#ggordiplot: ellipses for months
gg_ordiplot(genus_ct1_nmds,groups = genus_ct1_pred$month, pt.size = 3)

#ggordiplot: ellipses for year
gg_ordiplot(genus_ct1_nmds,groups = genus_ct1_pred$year, pt.size = 3)

#ggordiplot: ellipses for year x region
gg_ordiplot(genus_ct1_nmds,groups = genus_ct1_pred$year_region, pt.size = 3)

#basic plot with ggvegan
#ordiplot(genus_ct1_nmds)
#ordiplot(genus_ct1_nmds,type="t") #adds labels for samples and genera

#show nmds plot by region
#fortify() creates a data frame with nmds scores
fort <- fortify(genus_ct1_nmds)
ggplot() +
  geom_point(data=subset(fort, score == "sites"),
             mapping = aes(x=NMDS1, y=NMDS2, colour=genus_ct1_pred$region),
             alpha=0.5)+
  geom_segment(data=subset(fort,score == "species"),
               mapping = aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               arrow=arrow(length=unit(0.015, "npc"),
                           type="closed"),
               colour="darkgrey",
               size=0.8)+
  geom_text(data=subset(fort,score == "species"),
            mapping=aes(label=label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept=0, slope=0, linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))


#adonis by region
summary(genus_ct1_pred)
adonis2(genus_ct1_abund~region,data=genus_ct1_pred)
#regions did differ (p = 0.002)
#need to check dispersion which could also explain differences
#region doesn't explain much variation (R2 = 0.01087 or 1.1%)

#nmds by year
ggplot() +
  geom_point(data=subset(fort, score == "sites"),
             mapping = aes(x=NMDS1, y=NMDS2, colour=genus_ct1_pred$year),
             alpha=0.5)+
  geom_segment(data=subset(fort,score == "species"),
               mapping = aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               arrow=arrow(length=unit(0.015, "npc"),
                           type="closed"),
               colour="darkgrey",
               size=0.8)+
  geom_text(data=subset(fort,score == "species"),
            mapping=aes(label=label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept=0, slope=0, linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))

#adonis by year
adonis2(genus_ct1_abund~year,data=genus_ct1_pred)
#years did differ (p = 0.001)
#need to check dispersion which could also explain differences
#year doesn't explain much variation (R2 = 0.0422 or 4.2%)

#adonis by year x region
adonis2(genus_ct1_abund~year_region,data=genus_ct1_pred)
#year_region did differ (p = 0.001)
#doesn't explain much variation but better than any one predictor (R2 = 0.07 or 6.9%)

#nmds by month
ggplot() +
  geom_point(data=subset(fort, score == "sites"),
             mapping = aes(x=NMDS1, y=NMDS2, colour=genus_ct1_pred$month),
             alpha=0.5)+
  geom_segment(data=subset(fort,score == "species"),
               mapping = aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               arrow=arrow(length=unit(0.015, "npc"),
                           type="closed"),
               colour="darkgrey",
               size=0.8)+
  geom_text(data=subset(fort,score == "species"),
            mapping=aes(label=label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept=0, slope=0, linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))

#adonis by month
adonis2(genus_ct1_abund~month,data=genus_ct1_pred)
#months did not differ (p = 0.127)
#month doesn't explain much variation (R2 = 0.01433 or 1.4%)

