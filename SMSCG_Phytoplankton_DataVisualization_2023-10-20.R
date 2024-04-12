#Suisun Marsh Salinity Control Gate Action
#Phyto community composition across space and time

#Nick Rasmussen
#2024-04-05

#to do list-------
#should categorize EZ stations according to where they fall in SMSCG subregions rather than keeping them separate; probably wil
#always be in river region
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
#abund <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=8c283ea7d1823824ccfa2f05d8056027")
abund <- read_csv("./EDI/data_output/smscg_phytoplankton_samples_2020-2023.csv")

#read in phyto taxonomy data from SMSCG data package on EDI
#taxon <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=4c514654c71a7b8b930bdd031701c359")
taxon <- read_csv("./EDI/data_output/smscg_phytoplankton_taxonomy.csv")

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

#look at unique algal_group
taxon_high_combos <- taxon %>% 
  distinct(algal_group,phylum)

#look at genera in Haptophytes, Euglenoids and Chrysophytes
#don't have good data on carbon mass (Menden-Deuer and Lessard 2000) 
#and/or LCEFA for these taxa (Galloway and Winder 2015; not even in Table S2)
taxon_no_lcefa <- taxon %>% 
  filter(algal_group=="Euglenoids" | algal_group=="Chrysophytes" | algal_group=="Haptophytes")
#we will just need to drop these taxa from analysis for LCEFA
#Euglenoids are rank 5 of 9 on total biovolume, so sucks to drop them
#Chrysophytes are rank 8 of 9 so not such a big deal
#Haptophytes are rank 9 of 9 so again not much of a loss


#add the taxonomy and station metadata to abundance data----------

#add taxonomy
at <- left_join(abund,taxon) %>% 
  #drop one record of a ciliate because those aren't phyto
  filter(algal_group!="Ciliates")

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
    #Use one equation for diatoms and another equation for everything else based on Menden-Deuer & Lessard 2000
    ,biomass_pg_c = case_when((algal_group == "Centric Diatoms" | algal_group=="Pennate Diatoms") ~ 0.288 * (mean_cell_biovolume^0.811)
                                  ,!(algal_group == "Centric Diatoms" | algal_group=="Pennate Diatoms") ~ 0.216 * (mean_cell_biovolume^0.939))
    #now convert carbon pg to ug (ug-C per L)
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
alg_grp_biov_sample <- atr_mass %>% 
  group_by(region,station,year,month,date,algal_group) %>% 
  summarize(across(units_per_ml:biomass_ug_c_l, ~sum(.x, na.rm = TRUE)),.groups='drop') %>% 
  #add the LCEFA content info
  left_join(lcefa) %>% 
  mutate(
    #use the biomass estimates to estimate LCEFA content; divide by 100 because LCEFA values are percentages
    #not sure it was necessary to convert from ug/ml to ug/l
    lcefa_per_l = biomass_ug_c_l * lcefa_value / 100
  ) %>% 
  select(-c(phylum,lcefa_value)) %>% 
  glimpse()

#for next step, will need to calculate means across samples from things like biovolume
#to accurately calculate these means, need to add in zeros for all missing algal groups for all station dates
#make data frame with all existing station-dates and all possible combos of algal_group
alg_grp_biov_comb <- alg_grp_biov_sample %>% 
  expand(nesting(region,station,year,month,date),algal_group) %>% 
  arrange(date,station)

#next combine this new complete data frame with the data
alg_grp_biov_samp <- left_join(alg_grp_biov_comb,alg_grp_biov_sample)  %>% 
  #replace NAs with zeros for abundance metrics
  mutate(across(units_per_ml:lcefa_per_l, ~replace_na(.,0)))

#summarize biovolume, biomass, and LCEFA by sample (across algal groups) -------------------
#note that chrysophytes and euglenoids are excluded from LCEFA data so values for all of them are zero
alg_grp_biov_summary <- alg_grp_biov_samp %>% 
  group_by(region,station,year,month,date) %>% 
  summarise(across(c(biovolume_per_ml,biomass_ug_c_l,lcefa_per_l), ~sum(.x, na.rm = TRUE)),.groups='drop') %>% 
  arrange(year,month,region,station) %>% 
  glimpse()

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

#correlations
#biomass and LCEFA are estimated from biovolume so should be pretty linearly correlated

#check assumptions for biovolume
hist(alg_grp_biov_summary$biovolume_per_ml) #strongly clustered at low end with a few high values 
hist(log(alg_grp_biov_summary$biovolume_per_ml)) #looks much closer to normal 
shapiro.test(alg_grp_biov_summary$biovolume_per_ml) #W = 0.27126, p-value < 2.2e-16
shapiro.test(log(alg_grp_biov_summary$biovolume_per_ml)) #W = 0.99403, p-value = 0.04845
ggqqplot(log(alg_grp_biov_summary$biovolume_per_ml),ylab = "biovolume") #looks OKish

#check assumptions for biomass
hist(alg_grp_biov_summary$biomass_ug_c_l) #strongly clustered at low end with a few high values 
hist(log(alg_grp_biov_summary$biomass_ug_c_l)) #looks much closer to normal 
shapiro.test(alg_grp_biov_summary$biomass_ug_c_l) #W = 0.50416, p-value < 2.2e-16
shapiro.test(log(alg_grp_biov_summary$biomass_ug_c_l)) #W = 0.99626, p-value = 0.2967
ggqqplot(log(alg_grp_biov_summary$biomass_ug_c_l),ylab = "biovolume") #looks OKish

#check assumptions for LCEFA
hist(alg_grp_biov_summary$lcefa_per_l) #strongly clustered at low end with a few high values 
hist(log(alg_grp_biov_summary$lcefa_per_l)) #looks much closer to normal 
shapiro.test(alg_grp_biov_summary$lcefa_per_l) #W = 0.45061, p-value < 2.2e-16
shapiro.test(log(alg_grp_biov_summary$lcefa_per_l)) #W = 0.92851, p-value = 1.229e-14
ggqqplot(log(alg_grp_biov_summary$lcefa_per_l),ylab = "biovolume") #looks OKish

#correlation between biovolume and biomass
#use spearman because normality violated especially for LCEFA
cor.test(log(alg_grp_biov_summary$biovolume_per_ml),log(alg_grp_biov_summary$biomass_ug_c_l),method="spearman")
#corr = 0.9661786

cor.test(log(alg_grp_biov_summary$biovolume_per_ml),log(alg_grp_biov_summary$lcefa_per_l),method="spearman")
#corr = 0.9409912



#look at outlier 
# phyto_outlier <- alg_grp_biov_summary %>% 
#   filter(lcefa_per_l > 30)
#STN 602 July 2022

#look closer at composition of outlier
#high levels of Thalassiosira sp. during both surveys at this station in July 2022
#STN 602, FMWT 602, D7
#don't have June data included in this data set but could get it from EMP
# phyto_outlier_comp <- atr_mass %>% 
#   filter((station =="STN_602" | station=="FMWT_602" | station=="EMP_D7") 
#          & year==2022 
#          & genus=="Thalassiosira"
#          )
#no evidence of this taxon in any other samples at that station in 2022
  

#summarize biovolume, biomass, and LCEFA by region and month and algal group--------------------
#need to calculate means rather than just summing now that we are looking across samples
#note that chrysophytes and euglenoids are excluded from LCEFA data so values for all of them are zero
alg_grp_biov <- alg_grp_biov_samp %>% 
  group_by(region,year,month,algal_group) %>% 
  summarise(across(c(biovolume_per_ml,biomass_ug_c_l,lcefa_per_l), ~mean(.x, na.rm = TRUE)),.groups='drop') %>% 
  #add a new column that lumps together less common algal groups into "other" category
  rename(total_biovolume = biovolume_per_ml
         ,total_biomass = biomass_ug_c_l
         ,total_lcefa = lcefa_per_l
         ) %>% 
  arrange(year,month,region) %>% 
  glimpse()
#unique(alg_grp_biov$algal_group_adj)

#nine is too many groups to show in stacked bar plot, lump some into "other"
#look at total biovolume by algal group and order from high to low
biov_rank <- alg_grp_biov %>% 
  group_by(algal_group) %>% 
  summarise(grand_biovol = sum(total_biovolume)) %>% 
  arrange(grand_biovol)
#can lump Chrysophytes, Raphidophytes, and Haptophytes into other
#all other groups have at least one case in which they were good chunk of biovolume in a given month/region
#so reduces from 9 to 8 groups

#pull algal_group levels in order from biov_rank
algal_group_rank <- biov_rank %>% 
  pull(algal_group)

#set order of algal groups based on contribution to biovolume
alg_grp_biov$algal_group <- factor(alg_grp_biov$algal_group, levels=algal_group_rank)

#remake the df that summarizes algal group by region and month with rare groups combined into "Other"
alg_grp_biov_adj <- alg_grp_biov_samp %>% 
  #add a new column that lumps together less common algal groups into "other" category
  mutate(algal_group_adj = case_when(algal_group == "Chrysophytes" | algal_group == "Haptophytes" | 
                                       algal_group == "Raphidophytes" | algal_group == "Dinoflagellates" ~ "Other"
                                     ,TRUE ~ algal_group), .after = algal_group) %>% 
  group_by(region,year,month,algal_group_adj) %>% 
  summarise(across(c(biovolume_per_ml,biomass_ug_c_l,lcefa_per_l), ~mean(.x, na.rm = TRUE)),.groups='drop') %>% 
  rename(total_biovolume = biovolume_per_ml
         ,total_biomass = biomass_ug_c_l
         ,total_lcefa = lcefa_per_l
  ) %>% 
  arrange(year,month,region) %>% 
  glimpse()

#sum biovolume by adjusted algal group and order from high to low
biov_rank_adj <- alg_grp_biov_adj %>% 
  group_by(algal_group_adj) %>% 
  summarise(grand_biovol = sum(total_biovolume)) %>% 
  arrange(grand_biovol)

#pull algal_group_adj levels in order from biov_rank
algal_group_adj_rank <- biov_rank_adj %>% 
  pull(algal_group_adj)

#set order of adjusted algal groups based on contribution to biovolume
alg_grp_biov_adj$algal_group_adj <- factor(alg_grp_biov_adj$algal_group_adj, levels=algal_group_adj_rank)

#look at cases of negative values when biomass is log transformed
# alg_grp_biov_log <- alg_grp_biov %>% 
#   mutate(total_biomass_log =log(total_biomass),.after = total_biomass) %>% 
#   arrange(total_biomass_log)
#some values for biomass are less than 1 so log is negative
#could change units so that doesn't happen
#eg, pg/l instead of ug/l


#stacked barplots of biovolume by algal group, region and month-------------

#stacked bar plot of biovolume
# (plot_alg_grp_rm <- ggplot(alg_grp_biov, aes(x = month, y = total_biovolume, fill = algal_group))+
#    geom_bar(position = "stack", stat = "identity") + 
#    facet_wrap(year~region,ncol = 4)
#    )
#not very easy to look at, mostly because of high diatom abundance in July 2022 in bay

#stacked bar plot: log transformed biovolume, all regions and years
# (plot_alg_grp_rm <- ggplot(alg_grp_biov, aes(x = month, y = log(total_biovolume), fill = algal_group))+
#     geom_bar(position = "stack", stat = "identity") + 
#     facet_wrap(year~region,ncol = 4)
# )
#easier to see the various algal groups but hard to interpret

#stacked bar plot: log transformed biovolume, 2020-2023, no FLO
# (plot_alg_grp_rm_recent <- alg_grp_biov %>% 
#     filter(year>2019 & region!="FLO") %>% 
#     ggplot(aes(x = month, y = log(total_biovolume), fill = algal_group_adj))+
#     geom_bar(position = "stack", stat = "identity") + 
#     facet_wrap(year~region,ncol = 3)
# )
#ggsave(plot=plot_alg_grp_rm_recent,"Plots/Phytoplankton/smscg_phyto_stacked_bar_tot.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

  
#percent stacked bar plot: all regions and years
# (plot_alg_grp_rm_perc <- ggplot(alg_grp_biov, aes(x = month, y = total_biovolume, fill = algal_group))+
#     geom_bar(position = "fill", stat = "identity") + 
#     facet_wrap(year~region,ncol = 4)
# )

#stacked bar plot: all algal groups, biovolume, 2020-2023, no FLO
# (plot_alg_grp_rm_perc_recent <- alg_grp_biov %>% 
#     filter(year>2019 & region!="FLO") %>% 
#     ggplot(aes(x = month, y = total_biovolume, fill = algal_group))+
#     geom_bar(position = "fill", stat = "identity",color = "black") + 
#     facet_wrap(year~region,ncol = 3)
# )

#stacked bar plot: rare algal grouped lumped into "other", biovolume, 2020-2023, no FLO
(plot_alg_grp_rm_perc_recent_adj <- alg_grp_biov_adj %>% 
    filter(year>2019 & region!="FLO") %>% 
    ggplot(aes(x = month, y = total_biovolume, fill = algal_group_adj))+
    geom_bar(position = "fill", stat = "identity",color = "black") + 
    facet_wrap(year~region,ncol = 3)+
    labs(x = "Month", y = "Proportion Biovolume")+
  scale_fill_discrete(name = "Algal Group")
)
#ggsave(plot=plot_alg_grp_rm_perc_recent_adj,"Plots/Phytoplankton/smscg_phyto_stacked_bar_perc.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#summarize composition by region and year (not month)
alg_grp_biov_ry <- alg_grp_biov %>% 
  group_by(year,region,algal_group) %>% 
  summarise(across(c(total_biovolume:total_lcefa), ~mean(.x, na.rm = TRUE)),.groups='drop') %>% 
  filter(region!="FLO" & year>2019) %>% 
  arrange(year,region,algal_group)

#plot composition by region and year (not month); rare taxa lumped into "other"
alg_grp_biov_ry_adj <- alg_grp_biov_adj %>% 
  group_by(year,region,algal_group_adj) %>% 
  summarise(across(c(total_biovolume:total_lcefa), ~mean(.x, na.rm = TRUE)),.groups='drop') %>% 
  filter(region!="FLO" & year>2019) %>% 
  arrange(year,region,algal_group_adj)

#percent stacked bar plot: all regions and years, grouped by region
# (plot_alg_grp_ry_perc_r <- ggplot(alg_grp_biov_ry, aes(x = year, y = tot_bvol, fill = algal_group))+
#     geom_bar(position = "fill", stat = "identity") + 
#     facet_wrap(region~.)
# )

#percent stacked bar plot: all regions and years, grouped by year
# (plot_alg_grp_ry_perc_y <- ggplot(alg_grp_biov_ry, aes(x = region, y = tot_bvol, fill = algal_group))+
#     geom_bar(position = "fill", stat = "identity") + 
#     labs(x = "Region", y = "Proportion of biovolume")+
#     facet_wrap(year~.)
# )
#ggsave(plot=plot_alg_grp_ry_perc_y,"Plots/Phytoplankton/smscg_phyto_stacked_bar_perc_bvol.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)


#total stacked bar plot: all regions and years
# (plot_alg_grp_ry_tot_biov <- ggplot(alg_grp_biov_ry, aes(x = year, y = tot_bvol, fill = algal_group))+
#     geom_bar(position = "stack", stat = "identity", color="black") + 
#     facet_wrap(region~.)
# )

#percent stacked bar plot: all regions and years, grouped by region, rare taxa lumped into "other"
(plot_alg_grp_ry_prop_biov_adj <- ggplot(alg_grp_biov_ry_adj, aes(x = year, y = total_biovolume, fill = algal_group_adj))+
    geom_bar(position = "fill", stat = "identity", color="black") + 
    labs(x="Year",y = "Proportion biovolume" )+
    scale_fill_discrete(name = "Algal Group")+
    facet_wrap(.~region
               ,labeller = as_labeller(c("BAY"="Suisun Bay","MAR"="Suisun Marsh","RIV"="River"))
    )
)
#ggsave(plot=plot_alg_grp_ry_prop_biov_adj,"Plots/Phytoplankton/smscg_phyto_stacked_bar_prop_bvol.png",type ="cairo-png",width=8, height=6,units="in",dpi=300)

#total stacked bar plot: all regions and years; rare taxa lumped into "other"
#convert cubic microns to cubic mm to make y axis easier to read
(plot_alg_grp_ry_tot_biov_adj <- ggplot(alg_grp_biov_ry_adj, aes(x = year, y = total_biovolume/1000000000, fill = algal_group_adj))+
    geom_bar(position = "stack", stat = "identity", color="black") + 
    labs(x="Year",y = bquote("Biovolume"~(mm^3~mL^-1) ) )+
    scale_fill_discrete(name = "Algal Group")+
    facet_wrap(.~region
               ,labeller = as_labeller(c("BAY"="Suisun Bay","MAR"="Suisun Marsh","RIV"="River"))
               )
)
#ggsave(plot=plot_alg_grp_ry_tot_biov_adj,"Plots/Phytoplankton/smscg_phyto_stacked_bar_tot_bvol.png",type ="cairo-png",width=8, height=7,units="in",dpi=300)


#time series plot of eastern marsh stations before, during, after gate operations---------------
#specifically MONT, 609, 610
#gates operated August 15 to Oct 17
#because this comparison is within one year and only includes samples collected by DFW
#inconsistencies in how samples were enumerated don't apply here

#create vector of stations for filtering
stn_east <- c("STN_609","STN_610","STN_MONT")

#create subset of data
east23 <- alg_grp_biov_samp %>% 
  filter(year=="2023" & station %in% stn_east) %>% 
  #add column that assigns samples to pre vs post SMSCG ops starting
  mutate(timing = as.factor(case_when(date < "2023-08-15"~"Before"
                            ,date>="2023-08-15" ~"During"
                            ))
         #add a new column that lumps together less common algal groups into "other" category
         ,algal_group_adj = case_when(algal_group == "Chrysophytes" | algal_group == "Haptophytes" | 
                                        algal_group == "Raphidophytes" | algal_group == "Dinoflagellates" ~ "Other"
                                      ,TRUE ~ algal_group), .after = algal_group
         ) %>% 
  group_by(station,date,timing,algal_group_adj) %>% 
  summarise(across(c(biovolume_per_ml,biomass_ug_c_l,lcefa_per_l), ~sum(.x, na.rm = TRUE)),.groups='drop') %>% 
  glimpse()

#create list of unique station x date combos
east23sd <- east23 %>% 
  distinct(station,date)
#three sampling dates before gate operations started

#make stacked bar plot for each date with stations as facets
(plot_alg_grp_east <- ggplot(east23, aes(x = date, y = biovolume_per_ml, fill=algal_group_adj))+
  geom_bar(position = "stack", stat = "identity", color="black") + 
  labs(x="Year",y = "Biovolume" )+
  scale_fill_discrete(name = "Algal Group")+
  facet_grid(factor(station, levels=c("STN_609","STN_MONT","STN_610"))~.)
  )
#abundances seem to vary a lot among stations prior to gate operations starting so difficult
#to draw conclusions about impact of gate ops
#could make a plot that shows one bar for pre-SMSCG and one after start of SMSCG

#summarize data by pre vs post SMSCG
east23_gates<- east23 %>% 
  group_by(timing,algal_group_adj) %>% 
  summarise(total_biovolume = mean(biovolume_per_ml,na.rm=T),.groups='drop') %>% 
  arrange(desc(timing),algal_group_adj)

#sum phyto biovolume across algal groups by timing
east23_gates_sum <- east23_gates %>% 
  group_by(timing) %>% 
  summarize(grand_biovolume = sum(total_biovolume)) %>% 
  arrange(desc(timing))

#effect size of difference between before and after
east23_gates_sum[2,2]/east23_gates_sum[1,2] # 2.213091x more biovolume before than after

#cyanobacteria change
east23_gates[10,3]/east23_gates[3,3] #3.579333x higher before than after

#cyanobacteria change proportions
#before
east23_gates[10,3]/east23_gates_sum[2,2] # 0.2565589 of total composition
#after  
east23_gates[3,3]/east23_gates_sum[1,2] #0.1586296 of total composition


#diatom change
(east23_gates[8,3]+east23_gates[14,3])/(east23_gates[1,3]+east23_gates[7,3]) #1.673098x higher before than after
#before
(east23_gates[8,3]+east23_gates[14,3])/east23_gates_sum[2,2] #0.5056648
#after
(east23_gates[1,3]+east23_gates[7,3])/east23_gates_sum[1,2] #0.6688682

#set order of adjusted algal groups based on contribution to biovolume
east23_gates$algal_group_adj <- factor(east23_gates$algal_group_adj, levels=algal_group_adj_rank)

#make stacked bar plot showing before and after gate ops started
(plot_alg_grp_east_gates <- ggplot(east23_gates, aes(x = factor(timing,level=c("Before","During")), y = total_biovolume/1000000000, fill=algal_group_adj))+
    geom_bar(position = "stack", stat = "identity", color="black") + 
    labs(x="SMSCG operation",y = bquote("Biovolume"~(mm^3~mL^-1) ) )+
    scale_fill_discrete(name = "Algal Group")
)
#ggsave(plot=plot_alg_grp_east_gates,"Plots/Phytoplankton/smscg_phyto_stacked_bar_tot_bvol_2023_east_marsh.png",type ="cairo-png",width=5, height=6,units="in",dpi=300)

#make stacked bar plot showing before and after gate ops started
(plot_alg_grp_east_gates_prop <- ggplot(east23_gates, aes(x = factor(timing,level=c("Before","During")), y = total_biovolume, fill=algal_group_adj))+
    geom_bar(position = "fill", stat = "identity", color="black") + 
    labs(x="SMSCG operation",y = "Proportion biovolume" )+
    scale_fill_discrete(name = "Algal Group")
)

#stacked barplots of biomass by algal group, region and month-------------
#tried log transforming data but doesn't improve plots
#fairly similar looking to biovolume overall
#diatoms have less carbon per unit volume so biomass is less dominated by diatoms so other taxa can be seen
#but biomass is an even rougher estimation than biovolume

#ten is too many groups to show in stacked bar plot, lump some into "other"
#sum biomass by algal group and order from high to low
# biom_rank <- alg_grp_biov %>% 
#   group_by(algal_group) %>% 
#   summarise(grand_biomass = sum(total_biomass)) %>% 
#   arrange(grand_biomass)
#same result as biovolume so can use existing order of taxa

#stacked bar plot of raw biomass
# (plot_alg_grp_rm_bm <- ggplot(alg_grp_biov, aes(x = month, y = total_biomass, fill = algal_group))+
#     geom_bar(position = "stack", stat = "identity") + 
#     facet_wrap(year~region,ncol = 4)
# )

#percent stacked bar plot: all regions and years
# (plot_alg_grp_rm_perc_bm <- ggplot(alg_grp_biov, aes(x = month, y = total_biomass, fill = algal_group))+
#     geom_bar(position = "fill", stat = "identity") + 
#     facet_wrap(year~region,ncol = 4)
# )

#stacked bar plot: biomass, 2020-2022, no FLO
# (plot_alg_grp_rm_perc_recent <- alg_grp_biov %>% 
#     filter(year>2019 & region!="FLO") %>% 
#     ggplot(aes(x = month, y = total_biomass, fill = algal_group))+
#     geom_bar(position = "fill", stat = "identity", color="black") + 
#     facet_wrap(year~region,ncol = 3)
# )
#ggsave(plot=plot_alg_grp_rm_perc_recent,"Plots/Phytoplankton/smscg_phyto_stacked_bar_perc_biomass.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)

#plot composition by region and year (not month)
# alg_grp_biov_ry_bm <- alg_grp_biov %>% 
#   group_by(year,region,algal_group) %>% 
#   summarize(tot_bmass = sum(total_biomass),.groups = 'drop') %>% 
#   filter(region!="FLO" & year>2019)

#percent stacked bar plot: all regions and years
# (plot_alg_grp_ry_perc_bm <- ggplot(alg_grp_biov_ry_bm, aes(x = year, y = tot_bmass, fill = algal_group))+
#     geom_bar(position = "fill", stat = "identity") + 
#     facet_wrap(region~.)
# )

#percent stacked bar plot: all regions and years, grouped by region, rare taxa lumped into "other"
(plot_alg_grp_ry_prop_biom_adj <- ggplot(alg_grp_biov_ry_adj, aes(x = year, y = total_biomass, fill = algal_group_adj))+
   geom_bar(position = "fill", stat = "identity", color="black") + 
   labs(x="Year",y = "Proportion biomass" )+
   scale_fill_discrete(name = "Algal Group")+
   facet_wrap(.~region
              ,labeller = as_labeller(c("BAY"="Suisun Bay","MAR"="Suisun Marsh","RIV"="River"))
   )
)

#total stacked bar plot: all regions and years; rare taxa lumped into "other"
#convert cubic microns to cubic mm to make y axis easier to read
(plot_alg_grp_ry_tot_biom_adj <- ggplot(alg_grp_biov_ry_adj, aes(x = year, y = total_biomass, fill = algal_group_adj))+
    geom_bar(position = "stack", stat = "identity", color="black") + 
    labs(x="Year",y = "Biomass")+
    scale_fill_discrete(name = "Algal Group")+
    facet_wrap(.~region
               ,labeller = as_labeller(c("BAY"="Suisun Bay","MAR"="Suisun Marsh","RIV"="River"))
    )
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


#percent stacked bar plot: all regions and years, grouped by region, rare taxa lumped into "other"
(plot_alg_grp_ry_prop_fa_adj <- ggplot(alg_grp_biov_ry_adj, aes(x = year, y = total_lcefa, fill = algal_group_adj))+
    geom_bar(position = "fill", stat = "identity", color="black") + 
    labs(x="Year",y = "Proportion LCEFA" )+
    scale_fill_discrete(name = "Algal Group")+
    facet_wrap(.~region
               ,labeller = as_labeller(c("BAY"="Suisun Bay","MAR"="Suisun Marsh","RIV"="River"))
    )
)
#similar to biovolume and biomass except cyanobacteria drop out because virtually no LCEFA

#total stacked bar plot: all regions and years; rare taxa lumped into "other"
#convert cubic microns to cubic mm to make y axis easier to read
(plot_alg_grp_ry_tot_biom_adj <- ggplot(alg_grp_biov_ry_adj, aes(x = year, y = total_lcefa, fill = algal_group_adj))+
    geom_bar(position = "stack", stat = "identity", color="black") + 
    labs(x="Year",y = "LCEFA")+
    scale_fill_discrete(name = "Algal Group")+
    facet_wrap(.~region
               ,labeller = as_labeller(c("BAY"="Suisun Bay","MAR"="Suisun Marsh","RIV"="River"))
    )
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
(plot_total_bvol_recent <- ggplot(data=tot_biov, aes(x = month, y = total_biovolume)) + 
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
# mod_tbiov_log_full <- glm(log(total_biovolume) ~ region * month* year, data = tot_biov_nobay)
# 
# #model checking plots
# #plot(mod_tbiov_log_full)
# #plots look pretty good
# 
# #look at model results
# summary(mod_tbiov_log_full)
# Anova(mod_tbiov_log_full,type = "III") 
#three way interaction not significant so drop it

#model with two way interactions
# mod_tbiov_log_twoway = glm(log(total_biovolume) ~ region + month + year + region:month + region:year, data = tot_biov_nobay)
# summary(mod_tbiov_log_twoway)
# Anova(mod_tbiov_log_twoway,type = "III")
# drop1(mod_tbiov_log_twoway, test="Chi")
#the two way interactions aren't significant

#additive model
# mod_tbiov_log_oneway = glm(log(total_biovolume) ~ region + month + year, data = tot_biov_nobay)
# #plot(mod_tbiov_log_oneway)
# summary(mod_tbiov_log_oneway)
# Anova(mod_tbiov_log_oneway)
#drop1(mod_tbiov_log_oneway, test="Chi")
#years are different but not month or region

#anova version of analysis
mod_tbiov_log_oneway_aov = aov(log(total_biovolume) ~ region + month + year, data = tot_biov_nobay)
Anova(mod_tbiov_log_oneway_aov)
#tried dropping month completely but didn't change results

#multiple comparison
#determine which years differ
TukeyHSD(mod_tbiov_log_oneway_aov)
#2020 vs 2021: p = 0.0001725
#2020 vs 2022: p = 0.0000006
#2020 vs 2023: p = 0.0010388
#2021 vs 2022: p = 0.5697169
#2021 vs 2023: p = 0.9766188
#2022 vs 2023: p = 0.3353901
#so 2020 is higher than all other years but no other differences

#generate effect sizes for total phyto by month
effszm<-tot_biov_nobay %>% 
  group_by(month) %>% 
  summarize(
    bvol_mean = mean(total_biovolume)
    ,bvol_sd = sd(total_biovolume)
    , .groups = 'drop')

#generate effect sizes for total phyto by region
effszr<-tot_biov_nobay %>% 
  group_by(region) %>% 
  summarize(
    bvol_mean = mean(total_biovolume)
    ,bvol_sd = sd(total_biovolume)
    , .groups = 'drop')

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

#total phyto: compare 2020 vs 2023
effsz$bvol_mean[1]/effsz$bvol_mean[4] #2.218698

#GLM for before and during SMSCG for east marsh ------------------------------------

#create subset of data
east23_samp <- east23 %>% 
  group_by(station,date,timing) %>% 
  summarise(across(c(biovolume_per_ml,biomass_ug_c_l,lcefa_per_l), ~sum(.x, na.rm = TRUE)),.groups='drop') %>% 
  glimpse()

#analysis with log transformed response
mod_tbiov_log_east <- glm(biovolume_per_ml ~ timing, data = east23_samp)

#model checking plots
plot(mod_tbiov_log_east)
#the Q-Q plot doesn't look great

#look at model results
summary(mod_tbiov_log_east)
Anova(mod_tbiov_log_east) 
#timing not significant; p = 0.2951

#multiple comparison
#determine which years differ
TukeyHSD(mod_tbiov_log_oneway_aov)


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
  pivot_wider(id_cols = c(station:year), names_from = genus, values_from = biovolume_gn,values_fill = 0) %>% 
  glimpse()

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

#create another version of the dataset with 2020 removed to see how much this year affects results
#abundances
genus_ct1_abund2 <- genus_ct1 %>% 
  filter(year!=2020) %>% 
  select(-(c(station:year)))

#predictors
genus_ct1_pred2 <- genus_ct1 %>% 
  filter(year!=2020) %>% 
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
plot_nmds_yr<-gg_ordiplot(genus_ct1_nmds,groups = genus_ct1_pred$year_region, pt.size = 3)
ggsave(plot=plot_nmds_yr,"Plots/Phytoplankton/smscg_phyto_nmds_region-year.png",type ="cairo-png",width=8, height=5,units="in",dpi=300)


#basic plot with ggvegan
#ordiplot(genus_ct1_nmds)
#ordiplot(genus_ct1_nmds,type="t") #adds labels for samples and genera


#before doing adonis, need to make distance matrix of abundances
#needed for adonis and betadisper
dis <- vegdist(genus_ct1_abund)
dis2 <- vegdist(genus_ct1_abund2)


#adonis by region
summary(genus_ct1_pred)
adonis2(dis~genus_ct1_pred$region)
#regions did differ (p = 0.001); adding station as strata makes results not significant
#region doesn't explain much variation (R2 = 0.01002 or 1%)
mod<-betadisper(dis,genus_ct1_pred$region)
anova(mod) #p=0.58
#regions not significantly different (ie, homogeneity of variances is fine)
plot(mod, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse

#adonis by region without 2020
adonis2(dis2~genus_ct1_pred2$region) #still significant

#adonis by year
adonis2(dis~genus_ct1_pred$year,strata = genus_ct1_pred$station) #p=0.001
adonis2(dis2~genus_ct1_pred2$year,strata = genus_ct1_pred2$station) #still significant, p=0.001


#need to check dispersion which could also explain differences
#year doesn't explain much variation (R2 = 0.0422 or 4.2%)
mod2<-betadisper(dis,genus_ct1_pred$year)
anova(mod2) #p = 6.822e-07
#regions are significantly different (ie, homogeneity of variances violated)
plot(mod2, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse

#adonis by year x region
adonis2(dis~year_region,data=genus_ct1_pred)
adonis2(dis~genus_ct1_pred$region*genus_ct1_pred$year,strata = genus_ct1_pred$station) #interaction term is significant
#year_region did differ (p = 0.001)
#doesn't explain much variation but better than any one predictor (R2 = 0.07 or 6.9%)
mod3<-betadisper(dis,genus_ct1_pred$year_region)
anova(mod3) #p = 6.822e-07
#year_regions are significantly different (ie, homogeneity of variances violated)
plot(mod3, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse

#adonis by year x region (no 2020)
adonis2(dis~year_region,data=genus_ct1_pred)
adonis2(dis2~genus_ct1_pred2$region*genus_ct1_pred2$year,strata = genus_ct1_pred2$station) #interaction term is significant
#everything, including interaction term, still significant without 2020


#adonis by month
adonis2(dis~month,data=genus_ct1_pred)
#months did not differ (p = 0.127)
#month doesn't explain much variation (R2 = 0.01433 or 1.4%)

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


