#SMSCG
#phytoplantkon data
#data visualizations

#responses
#total organisms per ml
#total cells per ml
#total biovolume per ml

#To do list
#see if all the phyla are up to date; Tiffany's file is probably out of date
#make some plots to look at cells per mL which is a response I added to the source file
#compare DFW and EMP samples when they are collected close to same time
#maybe do nmds by region

#load packages
library(tidyverse) #variety of data science tools
library(lubridate) #format datesx
library(scales) #log scale axes in ggplot
#library(diathor) #diatom trait data

# 1. Read in the Data----------------------------------------------

#read in the aggregated and formatted phytoplankton data
phyto_vis<-read_csv("Data/phytoplankton/SMSCG_phytoplankton_formatted_2020-2021.csv")
#looks like column types are all correct, even date and time

#format data---------------

#EMP stations: Subset to just the five relevant to SMSCG
#D22, D4, NZ032, NZS42, D7

#create data frame that groups names of stations that represent the same general location and adds the three broad regions
#RV = Sacramento River, ME = East Suisun Marsh, MW = West Suisun Marsh, GB = Grizzly Bay
#Note: for 2022, changed from GZB to MtMouth
station_key <- data.frame(
  station = c("706", "D22","704","801", "802", "D4","609","MON","610","605", "606","NZ032","NZS42","GZB","602","D7"),
  station_comb = c(rep("706", 2), "704",rep("801", 3),"609","MON","610", "605", rep("606",2),"NZS42","GZB",rep("602",2)),
  region = c(rep("RV",6), rep("ME",3), rep("MW",4),rep("GB",3))
)
#glimpse(station_key)

#join the sample data and the new station/region data
#by using inner_join, only the SMSCG relevant stations are kept
phyto_gates<- inner_join(phyto_vis,station_key) %>% 
  #add a column for month by extracting month from date
  mutate(month = month(date)
         ,year = year(date)
         ) %>% 
  #remove the June samples from EMP because we don't have any other June samples
  filter(month!=6) 

#check for NAs
check_na <- phyto_gates[rowSums(is.na(phyto_gates)) > 0,]
#the only NAs are for the time for one sample

#export 2020 data as csv for publishing on EDI
edi <- phyto_gates %>% 
  filter(year=="2020") %>% 
  rename(biovolume_per_ml = biovolume_per_ml_new) %>% 
  select(
    station
    ,region
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
  #arrange(date,time)
  glimpse()
#write_csv(edi,"Data/phytoplankton/SMSCG_phytoplankton_formatted_2020_EDI_bvol_corr.csv")

#how many genera didn't match up with higher taxonomy?
#sum(is.na(phyto_gates$class))
#there are now zero rows that aren't matched with higher level taxonomy

#look at distinct genera and taxa
#taxon_na<-phyto_gates %>% 
#  filter(is.na(class)) %>% 
#  distinct(genus, taxon) %>% 
#  arrange(genus,taxon)
#5 genera and 6 taxa that weren't in higher taxonomy spreadsheet
#need to add them

#look at number of samples per station
s_samp_count<-phyto_gates %>% 
  distinct(station_comb, date) %>% 
  group_by(station_comb) %>% 
  summarize(count = n(), .groups = 'drop')  

#count total number of samples
sum(s_samp_count$count) #138

#look at number of samples per region
r_samp_count<-phyto_gates %>% 
  distinct(region, date) %>% 
  group_by(region) %>% 
  summarize(count = n(), .groups = 'drop')  
#sample size varies a lot among regions; river is most; bay is fewest

#look at number of samples per region and month
rm_samp_count<-phyto_gates %>% 
  distinct(region,station_comb, month,date) %>% 
  group_by(region, month) %>% 
  summarize(count = n(), .groups = 'drop') 
range(rm_samp_count$count)
#generally 8-14 samples per month x region

#look at number of samples per region and month and year
ry_samp_count<-phyto_gates %>% 
  distinct(region,station_comb, year, month,date) %>% 
  group_by(region, year,month) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  arrange(year,region)
range(ry_samp_count$count)

#look closer at Grizzly Bay samlpes
gzb <- phyto_gates %>% 
  filter(region=="GB")

#try to get diatom trait data using the diathor package--------------
#NOTE: so far, not having much luck; at least some of the examples worked

#first make a df with just the diatoms
#and format it into a matrix like this package needs
#diatoms_only<-phyto_gates %>% 
#  filter(phylum == "Ochrophyta") %>% 
  #for diaThorAll function, there must be a column called "species"
#  rename(species=taxon) %>% 
  #create combined station-date column
#  unite(station_date, station, date) %>% 
  #reduce to just needed columns
#  select(species
#         ,station_date
#         ,organisms_per_ml
#  )   %>% 
  #convert from long to wide
#  pivot_wider(names_from = station_date, values_from = organisms_per_ml) %>%  
  #convert all NAs to zeros
#  replace(is.na(.), 0) %>% 
  #make species a factor and everything else integers
  #NOTE that abundances are actually densities instead of counts but fine for now
#  mutate(across(c(species),as.factor)
#         ,across(c('STN 609_2020-08-12':'NZS42_2020-10-13'),as.integer)
#         ) %>% 
#  glimpse()
  

#data("diat_sampleData")
#glimpse(diat_sampleData)
#species is a factor and all other columns are integers


#Run diaThorAll to get all the outputs from the sample data with the default settings, and store
#the results into the “results” object, to also retain the output within R
#NOTE: this function runs a whole pipeline of functions and takes a while 
#results <- diaThorAll(diat_sampleData) #If the sample data was used

#I got their example to work
#loadedData <- diat_loadData(species_df=diat_sampleData,resultsPath=sharepoint_path) # load data with the diat_loadData() function
#results <- diat_ips(loadedData )

#this isn't working still
#loadedData <- diat_loadData(species_df=diatoms_only,resultsPath=sharepoint_path) 
#results <- diat_ips(loadedData )

#plot time series of density and biovolume by station-----------

#summarize density and biovolume data by sample (station x date combo)
#so sum all the taxon specific densities and biovolumes within a sample
s_phyto_sum<-phyto_gates %>% 
  group_by(region, station_comb, station, year, month, date, time) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol_old = sum(biovolume_per_ml_old)
    ,tot_bvol_new = sum(biovolume_per_ml_new) 
    ,.groups = 'drop'
  ) %>% 
  #make month and year factors
    mutate_at(vars(year,month), factor)
#write_csv(s_phyto_sum,file = paste0(sharepoint_path,"./Plots/SMSCG_phytoplankton_sample_summary_2020.csv"))

#reorder region factor levels for plotting
s_phyto_sum$region <- factor(s_phyto_sum$region, levels=c('GB','MW','ME','RV'))


#plot total phytoplankton density by station 
(plot_st_tot_den <-ggplot(s_phyto_sum, aes(x=date, y=tot_den, group=year))+ #specified what to plot on the x and y axes
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
  )

#plot total phytoplankton biovolume OLD by station
(plot_st_tot_bvol <-ggplot(s_phyto_sum, aes(x=date, y=tot_bvol_old, group=year))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
)

#plot total phytoplankton biovolume NEW by station
(plot_st_tot_bvol <-ggplot(s_phyto_sum, aes(x=date, y=tot_bvol_new, group=year))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
)

#plot time series of density and biovolume by region and month-----------

#function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

#so calculate means for densities and biovolumes 
#across stations within regions and across dates within months
r_phyto_sum<-s_phyto_sum %>% 
  group_by(region, month) %>% 
  summarize(
    tot_den_avg = mean(tot_den),
    tot_den_se = se(tot_den),
    tot_bvol_old_avg = mean(tot_bvol_old),
    tot_bvol_old_se = se(tot_bvol_old),
    tot_bvol_new_avg = mean(tot_bvol_new),
    tot_bvol_new_se = se(tot_bvol_new)
    ,.groups = 'drop'
  )
#glimpse(r_phyto_sum)

#reorder region factor levels for plotting
r_phyto_sum$region <- factor(r_phyto_sum$region, levels=c('GB','MW','ME','RV'))


#plot mean total phytoplankton density by region
(plot_rg_tot_den <-ggplot(r_phyto_sum, aes(x=month, y=tot_den_avg))+ 
    geom_line() + 
    geom_point() + 
    geom_errorbar(aes(ymin=tot_den_avg-tot_den_se, ymax=tot_den_avg+tot_den_se), width = 0.2) +
    facet_wrap(~region)
)

#plot mean total phytoplankton biovolume OLD by region
(plot_rg_tot_bvol_old <-ggplot(r_phyto_sum, aes(x=month, y=tot_bvol_old_avg))+ 
    geom_line() + 
    geom_point() + 
    geom_errorbar(aes(ymin=tot_bvol_old_avg-tot_bvol_old_se, ymax=tot_bvol_old_avg+tot_bvol_old_se), width = 0.2) +
    facet_wrap(~region)+
    labs(x="Month", y="Total Biovolume (cubic microns per mL)")
  
)

#plot mean total phytoplankton biovolume NEW by region
(plot_rg_tot_bvol_new <-ggplot(r_phyto_sum, aes(x=month, y=tot_bvol_new_avg))+ 
    geom_line() + 
    geom_point() + 
    geom_errorbar(aes(ymin=tot_bvol_new_avg-tot_bvol_new_se, ymax=tot_bvol_new_avg+tot_bvol_new_se), width = 0.2) +
    facet_wrap(~region)+
    labs(x="Month", y="Total Biovolume (cubic microns per mL)")
  
)
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_LinePlot_TotalBiovolume_RegionMonth.png"),type ="cairo-png",width=8, height=5,units="in",dpi=300)


#boxplots of total density and biovolume by region-------

(plot_rg_tot_den_bx<-ggplot(data=s_phyto_sum, aes(x = region, y = tot_den)) + 
   geom_boxplot()+
   geom_jitter() #adds all points to plot, not just outliers
)

#OLD
(plot_rg_tot_bvol_old_bx<-ggplot(data=s_phyto_sum, aes(x = region, y = tot_bvol_old)) + 
    geom_boxplot()+
    #geom_jitter()+ #adds all points to plot, not just outliers; need to remove default outlier points to avoid duplication
    labs(x = "Region", y = "Phytoplankton Biovolume (cubic microns per mL)")+
    scale_x_discrete(labels = c('Grizzly Bay','West Suisun Marsh','East Suisun Marsh','Lower Sacramento'))
)

#NEW
(plot_rg_tot_bvol_new_bx<-ggplot(data=s_phyto_sum, aes(x = region, y = tot_bvol_new)) + 
    geom_boxplot()+
    #geom_jitter()+ #adds all points to plot, not just outliers; need to remove default outlier points to avoid duplication
    labs(x = "Region", y = "Phytoplankton Biovolume (cubic microns per mL)")+
    scale_x_discrete(labels = c('Grizzly Bay','West Suisun Marsh','East Suisun Marsh','Lower Sacramento'))
)
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_BoxPlot_TotalBiovolume_Region_Report.png"),type ="cairo-png",width=8, height=5,units="in",dpi=300)


#plot number of genera by station and region-------

#determine how many species typically within each genera
#if generally one, then easier to just use genus for plots/analysis
ssp_count<-phyto_gates %>% 
  distinct(genus, taxon) %>% 
  group_by(genus) %>% 
  summarize(count = n(), .groups = 'drop')  

#use histogram to visualize distribution of species per genera
(plot_spp_per_genus_hist<-ggplot(data=ssp_count, aes(x = count)) + 
    geom_histogram()
)
#most genera contain 1-2 species but some contain up to 15

#look closer at genera with many species
genera_rich<-phyto_gates %>% 
  distinct(genus, taxon) %>% 
  filter(genus=="Nitzschia" | genus =="Navicula") %>% 
  arrange(genus)
#not just unique because of variants of same name (though there are variants of genus sp.)
#actually quite a few different species within these genera present

#create df that counts genera by sample
phyto_genera_sum<-phyto_gates %>% 
  distinct(region, station_comb,date, month,genus) %>% 
  group_by(region, station_comb,date, month) %>% 
  summarize(genera = n(), .groups = 'drop')

#use histogram to visualize distribution of genera per sample
(plot_genera_per_sample_hist<-ggplot(data=phyto_genera_sum, aes(x = genera)) + 
    geom_histogram()
)
#generally 1-7 but some up to 15 genera per sample

#plot time series of number of genera by station
(plot_st_genera <-ggplot(phyto_genera_sum, aes(x=date, y=genera))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb)
)

#number of genera per region
phyto_genera_sum_rg<-phyto_gates %>% 
  distinct(region,date, month,genus) %>% 
  group_by(region,date, month) %>% 
  summarize(genera = n(), .groups = 'drop')

#plot time series of number of genera by region
(plot_st_genera <-ggplot(phyto_genera_sum_rg, aes(x=date, y=genera))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~region)
)

#stacked bar plot showing density and biovolume by phyla and station-------

#summarize density and biovolume data by sample and phylum
#so sum densities and biovolumes within a sample by phylum
s_phyto_phylum_sum<-phyto_gates %>% 
  group_by(region, station_comb, station, date, year,month,phylum) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol_old = sum(biovolume_per_ml_old) 
    ,tot_bvol_new = sum(biovolume_per_ml_new)
    ,.groups = 'drop'
  )

#stacked bar plot time series of density by phylum and station
(plot_st_den_per_phylum <-ggplot(s_phyto_phylum_sum
      , aes(x=date, y= tot_den,  fill = phylum))+
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Phytoplankton Density (Organisms / mL)") + xlab("Date") + 
  #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
  #scale_fill_manual(name = "Phylum",
   #                 values=phylum_colors,
    #                breaks=phylum_names,
     #               labels = phylum_names)+ 
  facet_grid(~station_comb
             #,labeller = labeller(island = island_label) 
))
#cyanobacteria dominates the community numerically at all stations and times
#figure out why there is "NA" category for phylum

#stacked bar plot time series of biovolume OLD by phylum and station
(plot_st_bvol_old_per_phylum <-ggplot(s_phyto_phylum_sum, aes(x=date, y= tot_bvol_old,  fill = phylum))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Phytoplankton Biovolume") + xlab("Date") + 
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 
    facet_grid(~station_comb
               #,labeller = labeller(island = island_label) 
    ))
#mostly cyanobactera as well as diatoms and allies

#stacked bar plot time series of biovolume NEW by phylum and station
(plot_st_bvol_new_per_phylum <-ggplot(s_phyto_phylum_sum, aes(x=date, y= tot_bvol_new,  fill = phylum))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Phytoplankton Biovolume") + xlab("Date") + 
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 
    facet_grid(~station_comb
               #,labeller = labeller(island = island_label) 
    ))


#stacked bar plot showing phyla by region and month-------

#summarize density and biovolume data by region, month and phylum
#so calculate mean densities and biovolumes within a region and date by phylum
r_phyto_phylum_sum<-s_phyto_phylum_sum %>% 
  group_by(region, month, phylum) %>% 
  summarize(
    tot_den_avg = mean(tot_den),
    tot_den_se = se(tot_den),
    tot_bvol_old_avg = mean(tot_bvol_old),
    tot_bvol_old_se = se(tot_bvol_old),
    tot_bvol_new_avg = mean(tot_bvol_new),
    tot_bvol_new_se = se(tot_bvol_new), 
    .groups = 'drop'
      )
#NOTE: calculating the means probably isn't this simple
#probably need to create data frame with all combos of samples x taxa
#then include zeros for cases in which no individuals of a taxon where found
#to make sure those are part of the mean calculation
#the main thing is to get the number of samples in calculation correct (ie, denominator)

#stacked bar plot time series of density by phylum and station
(plot_rg_den_per_phylum <-ggplot(r_phyto_phylum_sum
                                 , aes(x=month, y= tot_den_avg,  fill = phylum))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Phytoplankton Density (Organisms / mL)") + xlab("Date") + 
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 
    facet_grid(~region
               #,labeller = labeller(island = island_label) 
    ))
#cyanobacteria dominates the community numerically at all stations and times
#figure out why there is "NA" category for phylum

#stacked bar plot time series of biovolume OLD by phylum and station
(plot_rg_bvol_old_per_phylum <-ggplot(r_phyto_phylum_sum
                                 , aes(x=month, y= tot_bvol_old_avg,  fill = phylum))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Phytoplankton Biovolume") + xlab("Month") + 
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 
    facet_grid(~region
               #,labeller = labeller(island = island_label) 
    ))
#mostly cyanobactera as well as diatoms and allies

#stacked bar plot time series of biovolume NEW by phylum and station
(plot_rg_bvol_new_per_phylum <-ggplot(r_phyto_phylum_sum
                                  , aes(x=month, y= tot_bvol_new_avg,  fill = phylum))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Phytoplankton Biovolume") + xlab("Month") + 
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 
    facet_grid(~region
               #,labeller = labeller(island = island_label) 
    ))
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_StackedBar_TotalBiovolume_AllPhyto_Region.png"),type ="cairo-png",width=8, height=5,units="in",dpi=300)


#stacked bar plot showing phyla by region-------

#summarize density and biovolume data by region only
r_phyto_phylum_sum_rg<-phyto_gates %>% 
  #sum densities and biovolumes within a sample by phylum
  group_by(region, station_comb, date, month,phylum) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol_old = sum(biovolume_per_ml_old)
    ,tot_bvol_new = sum(biovolume_per_ml_new)
    , .groups = 'drop') %>% 
  #calculate mean densities and biovolumes within regions by phylum
  group_by(region, phylum) %>% 
  summarize(
    tot_den_avg = mean(tot_den),
    tot_bvol_old_avg = mean(tot_bvol_old),
    tot_bvol_new_avg = mean(tot_bvol_new)
    , .groups = 'drop'
  )

#reorder region factor levels for plotting
r_phyto_phylum_sum_rg$region <- factor(r_phyto_phylum_sum_rg$region, levels=c('GB','MW','ME','RV'))


#stacked bar plot time series of diatom biovolume OLD by region, phylum
ggplot(r_phyto_phylum_sum_rg, aes(x=region, y= tot_bvol_old_avg,  fill = phylum))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Biovolume (cubic microns per mL)") + xlab("Region")  
  #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
  #scale_fill_manual(name = "Phylum",
  #                 values=phylum_colors,
  #                breaks=phylum_names,
  #               labels = phylum_names) 

#stacked bar plot time series of diatom biovolume NEW by region, phylum
ggplot(r_phyto_phylum_sum_rg, aes(x=region, y= tot_bvol_new_avg,  fill = phylum))+
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Biovolume (cubic microns per mL)") + xlab("Region")  
#scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
#scale_fill_manual(name = "Phylum",
#                 values=phylum_colors,
#                breaks=phylum_names,
#               labels = phylum_names) 

#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_StackedBar_AllPhyto_Region.png"),type ="cairo-png",width=6, height=5,units="in",dpi=300)



#stacked barplots for diatoms-------------

#summarize diatom density and biovolume data by sample (station x date combo)
#so sum all the taxon specific densities and biovolumes within a sample
s_diatom_sum<-phyto_gates %>% 
  #subset data to just the phylum with diatoms
  filter(phylum == "Ochrophyta") %>% 
  group_by(region, station_comb, station,year, month, date, time) %>% 
  summarize(
    d_tot_den = sum(organisms_per_ml)
    ,d_tot_bvol_old = sum(biovolume_per_ml_old)
    ,d_tot_bvol_new = sum(biovolume_per_ml_new)
    , .groups = 'drop'
  )  %>% 
  #make month a factor
  mutate_at(vars(month,year), factor)
#there are 7 samples without diatoms
#combine diatom data with total phyto data

#join diatom data set and all phyto data set
glimpse(s_phyto_sum)
glimpse(s_diatom_sum)
s_pd_sum <- left_join(s_phyto_sum,s_diatom_sum)

#replace NAs with zeros for density and biovolume
s_pd_sum_z <- s_pd_sum %>% 
    replace_na(list(d_tot_den = 0, d_tot_bvol_old = 0, d_tot_bvol_new = 0))

#summarize diatom density and biovolume data by class, region, and month
diatom_sum<-phyto_gates %>% 
  #subset data to just the phylum with diatoms
  filter(phylum == "Ochrophyta") %>% 
  #sum densities and biovolumes within a sample by class
  group_by(region, station_comb, date, month,class) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol_old = sum(biovolume_per_ml_old)
    ,tot_bvol_new = sum(biovolume_per_ml_new)
    , .groups = 'drop') %>% 
  #calculate mean densities and biovolumes within regions and months by class
    group_by(region, month, class) %>% 
    summarize(
      tot_den_avg = mean(tot_den),
      tot_den_se = se(tot_den),
      tot_bvol_old_avg = mean(tot_bvol_old),
      tot_bvol_old_se = se(tot_bvol_old),
      tot_bvol_new_avg = mean(tot_bvol_new),
      tot_bvol_new_se = se(tot_bvol_new)
      , .groups = 'drop'
    )

#reorder region factor levels for plotting
diatom_sum$region <- factor(diatom_sum$region, levels=c('GB','MW','ME','RV'))

  
#stacked bar plot time series of diatom biovolume OLD by region, month, class
ggplot(diatom_sum, aes(x=month, y= tot_bvol_old_avg,  fill = class))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Phytoplankton Biovolume") + xlab("Date") + 
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 
    facet_grid(~region
               #,labeller = labeller(island = island_label) 
    )

#stacked bar plot time series of diatom biovolume NEW by region, month, class
ggplot(diatom_sum, aes(x=month, y= tot_bvol_new_avg,  fill = class))+
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Phytoplankton Biovolume") + xlab("Date") + 
  #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
  #scale_fill_manual(name = "Phylum",
  #                 values=phylum_colors,
  #                breaks=phylum_names,
  #               labels = phylum_names)+ 
  facet_grid(~region
             #,labeller = labeller(island = island_label) 
  )
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_StackedBar_Diatom_RegionMonth.png"),type ="cairo-png",width=9, height=5,units="in",dpi=300)


#similar stacked bar plot but averaged over months too
#facilitate comparisons among regions

#summarize density and biovolume data by region only
diatom_sum_rg<-phyto_gates %>% 
  #subset data to just the phylum with diatoms
  filter(phylum == "Ochrophyta") %>% 
  #sum densities and biovolumes within a sample by class
  group_by(region, station_comb, date, month,class) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol_old = sum(biovolume_per_ml_old)
    ,tot_bvol_new = sum(biovolume_per_ml_new)
    , .groups = 'drop') %>% 
  #calculate mean densities and biovolumes within regions by class
  group_by(region, class) %>% 
  summarize(
    tot_den_avg = mean(tot_den),
    tot_den_se = se(tot_den),
    tot_bvol_old_avg = mean(tot_bvol_old),
    tot_bvol_old_se = se(tot_bvol_old),
    tot_bvol_new_avg = mean(tot_bvol_new),
    tot_bvol_new_se = se(tot_bvol_new)
    , .groups = 'drop'
  )

#reorder region factor levels for plotting
diatom_sum_rg$region <- factor(diatom_sum_rg$region, levels=c('GB','MW','ME','RV'))


#stacked bar plot time series of diatom biovolume OLD by region, class
ggplot(diatom_sum_rg, aes(x=region, y= tot_bvol_old_avg,  fill = class))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Biovolume (cubic microns per mL)") + xlab("Region")  
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 


#stacked bar plot time series of diatom biovolume NEW by region, class
ggplot(diatom_sum_rg, aes(x=region, y= tot_bvol_new_avg,  fill = class))+
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Biovolume (cubic microns per mL)") + xlab("Region")  
#scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
#scale_fill_manual(name = "Phylum",
#                 values=phylum_colors,
#                breaks=phylum_names,
#               labels = phylum_names)+ 

#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_StackedBar_Diatom_Region.png"),type ="cairo-png",width=6, height=5,units="in",dpi=300)

#boxplot of total phyto biovolume and diatom biovolume by region and month---------

#first reshape data frame
#use version with zeros for samples without diatoms (instead of NAs)
s_pd_sum_l<- s_pd_sum_z %>% 
  #reduce data frame to just needed columns
  select("region","station_comb","year","month","date","tot_bvol_old","tot_bvol_new","d_tot_bvol_old","d_tot_bvol_new") %>% 
  #convert wide to long
  pivot_longer(c("tot_bvol_old","tot_bvol_new","d_tot_bvol_old","d_tot_bvol_new"), names_to = "type", values_to = "tot_bvol") %>% 
  #converts some columns from character to factor
  mutate_at(vars(region,type), factor) %>% 
  #reorder factor levels for plotting
  mutate(region = factor(region, levels=c('RV','ME','MW','GB'))
         ,type = factor(type, levels=c("tot_bvol_old","tot_bvol_new","d_tot_bvol_old","d_tot_bvol_new"))
  #create new columns with better names for labeling facets
         ,type2 = recode(
           type, "tot_bvol_old" = "All Phytoplankton OLD","tot_bvol_new" = "All Phytoplankton NEW","d_tot_bvol_old" = "Diatoms OLD","d_tot_bvol_new" = "Diatoms NEW")
        ,region2 = recode(
    region, "RV" = "Lower Sac", "ME" = "E Suisun Marsh", "MW" = "W Suisun Marsh", "GB"="Grizzly Bay")
    ,month2 = recode(
    month, "7"="July" ,"8"="August", "9"="September", "10"="October")
  ) %>% 
  #convert from cubic microns per mL to cubic mm per mL
  mutate(tot_bvol2 = tot_bvol/1000000000)
glimpse(s_pd_sum_l)

#plot: col: phyto and diatoms, row: region, x-axis: month
(plot_rm_pd_bvol_bx<-ggplot(data=s_pd_sum_l
                            , aes(x = month
                                  , y = tot_bvol2
                                  #, y = log(tot_bvol2)
                                  )) + 
    geom_boxplot(fill="darkolivegreen4")+
    #ylim(0,0.05)+ #drops a high end outlier for all phyto, july, sac river
    facet_grid(region2~type2)+
        labs(x = "Month"
             , y = bquote("Biovolume"~(mm^3~mL^-1) )
             #, y = bquote("Biovolume"~LN(mm^3~mL^-1) )
                          )+
    scale_x_discrete(labels = c("July","August", "September", "October"))
)
#log transforming data means we lose 7 data points for the diatoms plot because they are zeros
#could switch month and region to better emphasize region comparisons but either month nor region differ

#create separate panels for 2020 and 2021
#this is necessary in part because Grizzly Bay samples are new in 2021
#also just use the new and correct calculations

#2020
s_pd_sum_l_n20 <- s_pd_sum_l %>% 
  #just keep the new calculations
  filter(grepl("new",type) & date < "2021-01-01") %>% 
  #create new columns with better names for labeling facets
  mutate(type2 = recode(
    type, "tot_bvol_new" = "2020 All Phytoplankton","d_tot_bvol_new" = "2020 Diatoms"
    )    )
unique(s_pd_sum_l_n20$type) #tot_bvol_new   d_tot_bvol_new
range(s_pd_sum_l_n20$date) #"2020-07-14" "2020-10-22"

#2021
s_pd_sum_l_n21 <- s_pd_sum_l %>% 
  #just keep the new calculations
  filter(grepl("new",type) & date > "2021-01-01") %>% 
  mutate(type2 = recode(
    type, "tot_bvol_new" = "2021 All Phytoplankton","d_tot_bvol_new" = "2021 Diatoms"
  ))
unique(s_pd_sum_l_n21$type) #tot_bvol_new   d_tot_bvol_new
range(s_pd_sum_l_n21$date) #"2020-07-14" "2020-10-22"

#2020 plot: col: phyto and diatoms, row: region, x-axis: month

group.colors20 <- c('2020 All Phytoplankton' = "darkolivegreen", "2020 Diatoms"= "darkolivegreen2")

ggplot(data=s_pd_sum_l_n20, aes(x = region2, y = tot_bvol2, fill=type2
                                  #, y = log(tot_bvol2)
                            )) + 
    geom_boxplot(
       #fill="darkolivegreen4"
      )+
    #ylim(0,0.05)+ #drops a high end outlier for all phyto, july, sac river
    facet_grid(month2~type2)+
    labs(x = "Region"
         , y = bquote("Biovolume"~(mm^3~mL^-1) )
         #, y = bquote("Biovolume"~LN(mm^3~mL^-1) )
    )+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+
  scale_fill_manual(values=group.colors20)+
  theme(legend.position="none"
        , axis.text.x = element_text(angle = 90, hjust = 1)
  )
#ggsave(file = "Data/phytoplankton/SMSCG_Phyto_Boxplot_Phyto&Diatom_Region&Month_2020.png",type ="cairo-png",scale=0.9,width=8.5, height=6,units="in",dpi=300)

#2021 plot: col: phyto and diatoms, row: region, x-axis: month

group.colors21 <- c('2021 All Phytoplankton' = "darkolivegreen", "2021 Diatoms"= "darkolivegreen2")

ggplot(data=s_pd_sum_l_n21, aes(x = region2, y = tot_bvol2, fill=type2
                                #, y = log(tot_bvol2)
)) + 
  geom_boxplot()+
  #ylim(0,0.05)+ #drops a high end outlier for all phyto, july, sac river
  facet_grid(month2~type2)+
  labs(x = "Region"
       , y = bquote("Biovolume"~(mm^3~mL^-1) )
       #, y = bquote("Biovolume"~LN(mm^3~mL^-1) )
  )+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(sides="l")+
  scale_fill_manual(values=group.colors21)+
  theme(legend.position="none"
        , axis.text.x = element_text(angle = 90, hjust = 1)
  )
#ggsave(file = "Data/phytoplankton/SMSCG_Phyto_Boxplot_Phyto&Diatom_Region&Month_2021.png",type ="cairo-png",scale=0.9,width=8.5, height=8,units="in",dpi=300)


#generate effect sizes for total phyto and diatoms across regions and months
#needed for USBR report
effsz<-s_pd_sum_l %>% 
  filter(region!="GB" & grepl("new",type)) %>% 
  group_by(type,year) %>% 
  summarize(
    bvol_mean = mean(tot_bvol2)
    ,bvol_sd = sd(tot_bvol2)
    , .groups = 'drop')

#total phyto: compare 2020 vs 2021
effsz$bvol_mean[1]/effsz$bvol_mean[2] #2.122569

#diatoms: compare 2020 vs 2021
effsz$bvol_mean[3]/effsz$bvol_mean[4] #1.654778

#proportion of diatom biovolume in samples
#2020
effsz$bvol_mean[3]/effsz$bvol_mean[1] #0.5478529
#2021
effsz$bvol_mean[4]/effsz$bvol_mean[2] #0.7027262


#compare regions
effszr<-s_pd_sum_l %>% 
  filter(region!="GB" & grepl("new",type)) %>% 
  group_by(type,region) %>% 
  summarize(
    bvol_mean = mean(tot_bvol2)
    ,bvol_sd = sd(tot_bvol2)
    , .groups = 'drop')

#diatoms: compare RV vs. MW
effszr$bvol_mean[6]/effszr$bvol_mean[4] #2.240234


#Statistics: total phyto biovolume--------

#glimpse(s_phyto_sum)

#create subset without Grizzly Bay samples
#samples sizes are too small for this region in 2021 and it is missing from 2020
s_phyto_sum_g <- s_phyto_sum %>% 
  filter(region!="GB")

#predictors to include: region, month, year
#could include station as a random effect. there are only 9 though

#build model
tbmod_new = glm(tot_bvol_new ~ region * month* year, data = s_phyto_sum_g)

#model checking plots
#plot(tbmod_new)
#plots aren't great
#residuals vs fitted: spread gets larger with higher values 
#Q-Q plot: points stray pretty far off diagonal at upper end 
#scale-location: line drifts up with higher values
#residuals vs leverage: looks OK

#redo analysis with log transformed response
tblmod_new_lg = glm(log(tot_bvol_new) ~ region * month* year, data = s_phyto_sum_g)

#model checking plots
#plot(tblmod_new_lg)
#looks like log transformation did the trick
#could also try a different error distribution like gamma

#look at model results
summary(tblmod_new_lg)
drop1(tblmod_new_lg, test="Chi")
#three-way interaction term isn't significant; p=0.2615

tblmod_new_lg2 = glm(log(tot_bvol_new) ~ region + month + year + region:month + region:year, data = s_phyto_sum_g)

#look at model results
summary(tblmod_new_lg2)
drop1(tblmod_new_lg2, test="Chi")
#neither two-way interaction is significant

#redo model with log transformed response and without interaction term
tblmod_new_lg3 = glm(log(tot_bvol_new) ~ region  + month + year, data = s_phyto_sum_g)
#plot(tblmod_new_lg3)

#look at model results
summary(tblmod_new_lg3)
drop1(tblmod_new_lg3, test="Chi")
#neither region (p=0.6708) nor month (p=0.5262) are significant but year is (p=1.387e-05)

#Statistics: total diatom biovolume--------

#predictors to include: region, month, maybe salinity
#could include station as a random effect. there are only 9 though

#Note: there are 7 zeros that are currently excluded from the data set used in these models
#s_pd_sum: these 27 samples are NAs
#s_pd_sum_z: these 27 samples are zeros
zeros <-filter(s_pd_sum_z, d_tot_bvol_new == 0)
#RV = 13, ME = 6, MW = 8

#create subset without Grizzly Bay samples
#samples sizes are too small for this region in 2021 and it is missing from 2020
s_diatom_sum_g <- s_diatom_sum %>% 
  filter(region!="GB") %>% 
  mutate(year=factor(year)
         ,region=factor(region)) %>% 
  glimpse()

#predictors to include: region, month, year
#could include station as a random effect. there are only 9 though

#build model
tbmod_new = glm(d_tot_bvol_new ~ region * month* year, data = s_diatom_sum_g)

#model checking plots
#plot(tbmod_new)
#plots aren't great
#residuals vs fitted: spread gets larger with higher values 
#Q-Q plot: points stray pretty far off diagonal at upper end 
#scale-location: line drifts up with higher values
#residuals vs leverage: looks OK

#redo analysis with log transformed response
tblmod_new_lg = glm(log(d_tot_bvol_new) ~ region * month* year, data = s_diatom_sum_g)

#model checking plots
#plot(tblmod_new_lg)
#looks like log transformation did the trick
#could also try a different error distribution like gamma

#look at model results
summary(tblmod_new_lg)
drop1(tblmod_new_lg, test="Chi")
#three-way interaction term isn't significant; p=0.5824

tblmod_new_lg2 = glm(log(d_tot_bvol_new) ~ region + month + year + region:month + region:year, data = s_diatom_sum_g)

#look at model results
summary(tblmod_new_lg2)
drop1(tblmod_new_lg2, test="Chi")
#neither two-way interaction is significant, though region:month is somewhat close (p=0.09659)

#redo model with log transformed response and without interaction term
tblmod_new_lg3 = glm(log(d_tot_bvol_new) ~ region  + month + year, data = s_diatom_sum_g)
#plot(tblmod_new_lg3)

#look at model results
summary(tblmod_new_lg3)
drop1(tblmod_new_lg3, test="Chi")
#region (p=0.03468)
#month (p=0.65924) 
#year is (p=3.835e-05)

#pairwise comparisons among the three regions
tblmod_new_lg4 = aov(log(d_tot_bvol_new) ~ region  + month + year, data = s_diatom_sum_g)
TukeyHSD(tblmod_new_lg4,"region")
names(s_diatom_sum_g)

#look at Grizzly Bay stations----------

#s_samp_count shows sample sizes
#602 = 13, GZB = 2

#look closer at Grizzly bay samples
gzb_sum <- gzb %>% 
  distinct(station_comb, station, date) %>% 
  arrange(date,station_comb,station)

#create a subset of 2021 GB samples with density and biovolume summed by sample
gb_sum <- s_phyto_sum %>% 
  filter(region=="GB" & year=="2021")

#plot time series of biovolume and density by station
ggplot(gb_sum, aes(x=date, y=tot_den, group=station,col=station))+ #specified what to plot on the x and y axes
    geom_line() + 
    geom_point()  


#plot total phytoplankton biovolume NEW by station
ggplot(gb_sum, aes(x=date, y=tot_bvol_new, group=station,col=station))+ 
    geom_line() + 
    geom_point() 

#next look closer at taxonomic composition

gb_phylum_sum <- s_phyto_phylum_sum %>% 
  filter(region=="GB" & year=="2021")


#stacked bar plot time series of density by phylum and station
ggplot(gb_phylum_sum, aes(x=station, y= tot_den,fill = phylum))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Phytoplankton Density (Organisms / mL)") + xlab("Station")+
  facet_grid(~date)

#stacked bar plot time series of biovolume NEW by phylum and station
ggplot(gb_phylum_sum, aes(x=station, y= tot_bvol_new,fill = phylum))+
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Phytoplankton Biovolume (cubic microns per mL)") + xlab("Station")+
  facet_grid(~date)



