#SMSCG
#phytoplantkon data
#data visualizations

#Note: refer to DSRS plotting code
#also compare DFW and EMP samples where possible

#total organisms per ml
#total biovolume per ml
#both responses by major taxonomic group (likely phylum)
#maybe nmds by region

#NOTE: don't forget to compare data with notes about some samples not looking not well preserved
#also note that EMP data starts in June but DFW data starts in July

#load packages
library(tidyverse) #variety of data science tools
library(ggplot2) #making plots
library(lubridate) #format dates
library(diathor) #diatom trait data

# 1. Read in the Data----------------------------------------------
# Dataset is on SharePoint site for the SMSCG action

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/SMSCG - Summer Action - Data"
  )
)  

#read in the aggregated and formatted phytoplankton data
phyto_vis<-read_csv(file = paste0(sharepoint_path,"./Phytoplankton/SMSCG_phytoplankton_formatted_2020.csv"))
#looks like column types are all correct, even date and time

#format data---------------

#look at station names
#sort(unique(phyto_vis$station))
#names are mostly, but not completely, unique
#fix this one because it's needed: "STN 610" vs "STN610"
#the rest will be filtered out: 
#"C3A" "C3A-Hood" "C3A-HOOD"
#"D16"  "D16-Twitchell"
#"E26" vs "EZ6" 


#remove spaces from all station names, which will remove the redundant versions of 610

phyto_vis$station2<-str_replace_all(phyto_vis$station, " ", "")

#EMP stations: Subset to just the four relevant to SMSCG
#Two different naming systems for stations: EMP (DFW)
#D22 (NZ064), D4 (NZ060), NZ032 (NZ032), NZS42 (NZS42)

#create data frame that groups names of stations that represent the same general location and adds the three broad regions
#RV = Sacramento River, ME = East Suisun Marsh, MW = West Suisun Marsh
station_key <- data.frame(
  station2 = c("STN706", "FMWT706", "NZ064", "D22","STN704","FMWT704","STN801", "FMWT802", "NZ060", "D4","STN609","MONT","STN610","FMWT605", "STN606","FMWT606","NZ032","NZS42"),
  station_comb = c(rep("PHY706", 4), rep("PHY704",2), rep("PHY801", 4),"STN609","MONT","STN610", "FMWT605", rep("PHY606",3),"NZS42"),
  region = c(rep("RV",10), rep("ME",3), rep("MW",5))
)
#glimpse(station_key)

#join the sample data and the new station/region data
#by using inner_join, only the SMSCG relevant stations are kept
phyto_gates<- inner_join(phyto_vis,station_key) %>% 
  #add a column for month by extracting month from date
  mutate(month = month(date)) %>% 
  #remove the June samples from EMP because we don't have any other June samples
  filter(month!=6) 

#check for NAs
check_na <- phyto_gates[rowSums(is.na(phyto_gates)) > 0,]
#no NAs now that I've updated the taxonomy dataset

#export data as csv for publishing on EDI
edi<-phyto_gates[,c(1,14,2:11)]
#write_csv(edi,file = paste0(sharepoint_path,"./Phytoplankton/SMSCG_phytoplankton_EDI_2020.csv"))

#how many genera didn't match up with higher taxonomy?
#sum(is.na(phyto_gates$class))
#there are 10 rows that aren't matched with higher level taxonomy

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
#generally 6-8 as expected 

#count total number of samples
sum(s_samp_count$count) #70

#look at number of samples per region
r_samp_count<-phyto_gates %>% 
  distinct(region, date) %>% 
  group_by(region) %>% 
  summarize(count = n(), .groups = 'drop')  
#twice as many samples in river as east marsh
#west marsh intermediate in sample number

#look at number of samples per region and month
rm_samp_count<-phyto_gates %>% 
  distinct(region,station_comb, month,date) %>% 
  group_by(region, month) %>% 
  summarize(count = n(), .groups = 'drop') 
range(rm_samp_count$count)
#4-7 samples per month x region


#try to get diatom trait data using the diathor package--------------
#NOTE: so far, not having much luck; at least some of the examples worked

#first make a df with just the diatoms
#and format it into a matrix like this package needs
diatoms_only<-phyto_gates %>% 
  filter(phylum == "Ochrophyta") %>% 
  #for diaThorAll function, there must be a column called "species"
  rename(species=taxon) %>% 
  #create combined station-date column
  unite(station_date, station, date) %>% 
  #reduce to just needed columns
  select(species
         ,station_date
         ,organisms_per_ml
  )   %>% 
  #convert from long to wide
  pivot_wider(names_from = station_date, values_from = organisms_per_ml) %>%  
  #convert all NAs to zeros
  replace(is.na(.), 0) %>% 
  #make species a factor and everything else integers
  #NOTE that abundances are actually densities instead of counts but fine for now
  mutate(across(c(species),as.factor)
         ,across(c('STN 609_2020-08-12':'NZS42_2020-10-13'),as.integer)
         ) %>% 
  glimpse()
  

data("diat_sampleData")
glimpse(diat_sampleData)
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
  group_by(region, station_comb, station2, month, date, time) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol = sum(biovolume_per_ml), 
    .groups = 'drop'
  ) %>% 
  #make month a factor
    mutate_at(vars(month), factor)
#write_csv(s_phyto_sum,file = paste0(sharepoint_path,"./Plots/SMSCG_phytoplankton_sample_summary_2020.csv"))

#reorder region factor levels for plotting
s_phyto_sum$region <- factor(s_phyto_sum$region, levels=c('MW','ME','RV'))


#plot total phytoplankton density by station 
(plot_st_tot_den <-ggplot(s_phyto_sum, aes(x=date, y=tot_den))+ #specified what to plot on the x and y axes
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
  )

#plot total phytoplankton biovolume by station
(plot_st_tot_bvol <-ggplot(s_phyto_sum, aes(x=date, y=tot_bvol))+ 
    geom_line() + 
    geom_point() + 
    facet_wrap(~station_comb, nrow=2)
)

#plot time series of density and biovolume by region-----------

#function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

#so calculate means for densities and biovolumes 
#across stations within regions and across dates within months
r_phyto_sum<-s_phyto_sum %>% 
  group_by(region, month) %>% 
  summarize(
    tot_den_avg = mean(tot_den),
    tot_den_se = se(tot_den),
    tot_bvol_avg = mean(tot_bvol),
    tot_bvol_se = se(tot_bvol), 
    .groups = 'drop'
  )
#glimpse(r_phyto_sum)

#reorder region factor levels for plotting
r_phyto_sum$region <- factor(r_phyto_sum$region, levels=c('MW','ME','RV'))


#plot mean total phytoplankton density by region
(plot_rg_tot_den <-ggplot(r_phyto_sum, aes(x=month, y=tot_den_avg))+ 
    geom_line() + 
    geom_point() + 
    geom_errorbar(aes(ymin=tot_den_avg-tot_den_se, ymax=tot_den_avg+tot_den_se), width = 0.2) +
    facet_wrap(~region)
)

#plot mean total phytoplankton biovolume by region
(plot_rg_tot_bvol <-ggplot(r_phyto_sum, aes(x=month, y=tot_bvol_avg))+ 
    geom_line() + 
    geom_point() + 
    geom_errorbar(aes(ymin=tot_bvol_avg-tot_bvol_se, ymax=tot_bvol_avg+tot_bvol_se), width = 0.2) +
    facet_wrap(~region)+
    labs(x="Month", y="Total Biovolume (cubic microns per mL)")
  
)
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_LinePlot_TotalBiovolume_RegionMonth.png"),type ="cairo-png",width=8, height=5,units="in",dpi=300)


#boxplots of total density and biovolume by region-------

(plot_rg_tot_den_bx<-ggplot(data=s_phyto_sum, aes(x = region, y = tot_den)) + 
   geom_boxplot()+
   geom_jitter() #adds all points to plot, not just outliers
)

(plot_rg_tot_bvol_bx<-ggplot(data=s_phyto_sum, aes(x = region, y = tot_bvol)) + 
    geom_boxplot()+
    #geom_jitter()+ #adds all points to plot, not just outliers; need to remove default outlier points to avoid duplication
    labs(x = "Region", y = "Phytoplankton Biovolume (cubic microns per mL)")+
    scale_x_discrete(labels = c('West Suisun Marsh','East Suisun Marsh','Lower Sacramento'))
)
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_BoxPlot_TotalBiovolume_Region_Report.png"),type ="cairo-png",width=8, height=5,units="in",dpi=300)

#look at high end outliers
out<-filter(s_phyto_sum, tot_bvol>40000000)

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
#not just unique because of variants of same name
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
  group_by(region, station_comb, date, month,phylum) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol = sum(biovolume_per_ml), 
    .groups = 'drop'
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

#stacked bar plot time series of density by phylum and station
(plot_st_bvol_per_phylum <-ggplot(s_phyto_phylum_sum, aes(x=date, y= tot_bvol,  fill = phylum))+
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


#stacked bar plot showing phyla by region and month-------

#summarize density and biovolume data by region, month and phylum
#so calculate mean densities and biovolumes within a region and date by phylum
r_phyto_phylum_sum<-s_phyto_phylum_sum %>% 
  group_by(region, month, phylum) %>% 
  summarize(
    tot_den_avg = mean(tot_den),
    tot_den_se = se(tot_den),
    tot_bvol_avg = mean(tot_bvol),
    tot_bvol_se = se(tot_bvol), 
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

#stacked bar plot time series of biovolume by phylum and station
(plot_rg_bvol_per_phylum <-ggplot(r_phyto_phylum_sum
                                 , aes(x=month, y= tot_bvol_avg,  fill = phylum))+
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
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_StackedBar_TotalBiovolume_AllPhyto_Region.png"),type ="cairo-png",width=8, height=5,units="in",dpi=300)


#stacked bar plot showing phyla by region-------

#summarize density and biovolume data by region only
r_phyto_phylum_sum_rg<-phyto_gates %>% 
  #sum densities and biovolumes within a sample by phylum
  group_by(region, station_comb, date, month,phylum) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol = sum(biovolume_per_ml)
    , .groups = 'drop') %>% 
  #calculate mean densities and biovolumes within regions by phylum
  group_by(region, phylum) %>% 
  summarize(
    tot_den_avg = mean(tot_den),
    tot_bvol_avg = mean(tot_bvol)
    , .groups = 'drop'
  )

#reorder region factor levels for plotting
r_phyto_phylum_sum_rg$region <- factor(r_phyto_phylum_sum_rg$region, levels=c('MW','ME','RV'))


#stacked bar plot time series of diatom biovolume by region, class
(plot_all_phyto_rg_only_bvol_per_phylum <-ggplot(r_phyto_phylum_sum_rg
                                             , aes(x=region, y= tot_bvol_avg,  fill = phylum))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Biovolume (cubic microns per mL)") + xlab("Region")  
  #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
  #scale_fill_manual(name = "Phylum",
  #                 values=phylum_colors,
  #                breaks=phylum_names,
  #               labels = phylum_names)+ 
)
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_StackedBar_AllPhyto_Region.png"),type ="cairo-png",width=6, height=5,units="in",dpi=300)

#stacked barplots for diatoms-------------

#summarize diatom density and biovolume data by sample (station x date combo)
#so sum all the taxon specific densities and biovolumes within a sample
s_diatom_sum<-phyto_gates %>% 
  #subset data to just the phylum with diatoms
  filter(phylum == "Ochrophyta") %>% 
  group_by(region, station_comb, station2, month, date, time) %>% 
  summarize(
    d_tot_den = sum(organisms_per_ml)
    ,d_tot_bvol = sum(biovolume_per_ml)
    , .groups = 'drop'
  )  %>% 
  #make month a factor
  mutate_at(vars(month), factor)
#there are 7 samples without diatoms
#combine diatom data with total phyto data

#join diatom data set and all phyto data set
s_pd_sum <- left_join(s_phyto_sum,s_diatom_sum)

#replace NAs with zeros for density and biovolume
s_pd_sum_z <- s_pd_sum %>% 
    replace_na(list(d_tot_den = 0, d_tot_bvol = 0))

#summarize diatom density and biovolume data by class, region, and month
diatom_sum<-phyto_gates %>% 
  #subset data to just the phylum with diatoms
  filter(phylum == "Ochrophyta") %>% 
  #sum densities and biovolumes within a sample by class
  group_by(region, station_comb, date, month,class) %>% 
  summarize(
    tot_den = sum(organisms_per_ml)
    ,tot_bvol = sum(biovolume_per_ml)
    , .groups = 'drop') %>% 
  #calculate mean densities and biovolumes within regions and months by class
    group_by(region, month, class) %>% 
    summarize(
      tot_den_avg = mean(tot_den),
      tot_den_se = se(tot_den),
      tot_bvol_avg = mean(tot_bvol),
      tot_bvol_se = se(tot_bvol)
      , .groups = 'drop'
    )

#reorder region factor levels for plotting
diatom_sum$region <- factor(diatom_sum$region, levels=c('MW','ME','RV'))

  
#stacked bar plot time series of diatom biovolume by region, month, class
(plot_diatom_rg_bvol_per_class <-ggplot(diatom_sum
                                  , aes(x=month, y= tot_bvol_avg,  fill = class))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Phytoplankton Biovolume") + xlab("Date") + 
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 
    facet_grid(~region
               #,labeller = labeller(island = island_label) 
    ))
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
    ,tot_bvol = sum(biovolume_per_ml)
    , .groups = 'drop') %>% 
  #calculate mean densities and biovolumes within regions by class
  group_by(region, class) %>% 
  summarize(
    tot_den_avg = mean(tot_den),
    tot_den_se = se(tot_den),
    tot_bvol_avg = mean(tot_bvol),
    tot_bvol_se = se(tot_bvol)
    , .groups = 'drop'
  )

#reorder region factor levels for plotting
diatom_sum_rg$region <- factor(diatom_sum_rg$region, levels=c('MW','ME','RV'))


#stacked bar plot time series of diatom biovolume by region, class
(plot_diatom_rg_only_bvol_per_class <-ggplot(diatom_sum_rg
                                        , aes(x=region, y= tot_bvol_avg,  fill = class))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Biovolume (cubic microns per mL)") + xlab("Region")  
    #scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
    #scale_fill_manual(name = "Phylum",
    #                 values=phylum_colors,
    #                breaks=phylum_names,
    #               labels = phylum_names)+ 
)
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_StackedBar_Diatom_Region.png"),type ="cairo-png",width=6, height=5,units="in",dpi=300)

#boxplot of total phyto biovolume and diatom biovolume by region and month---------

#first reshape data frame
#use version with zeros for samples without diatoms (instead of NAs)
s_pd_sum_l<- s_pd_sum_z %>% 
  #reduce data frame to just needed columns
  select("region","station_comb","month","date","tot_bvol","d_tot_bvol") %>% 
  #convert wide to long
  pivot_longer(c("tot_bvol","d_tot_bvol"), names_to = "type", values_to = "tot_bvol") %>% 
  #converts some columns from character to factor
  mutate_at(vars(region,type), factor) %>% 
  #reorder factor levels for plotting
  mutate(region = factor(region, levels=c('RV','ME','MW'))
         ,type = factor(type, levels=c("tot_bvol","d_tot_bvol"))
  #create new columns with better names for labeling facets
         ,type2 = recode(
           type, "tot_bvol" = "All Phytoplankton","d_tot_bvol" = "Diatoms")
        ,region2 = recode(
    region, "RV" = "Lower Sacramento", "ME" = "East Suisun Marsh", "MW" = "West Suisun Marsh")) %>% 
  #convert from cubic microns per mL to cubic mm per mL
  mutate(tot_bvol2 = tot_bvol/1000000000)
glimpse(s_pd_sum_l)

#generate effect sizes for total phyto and diatoms across regions and months
#needed for USBR report
#start with means and SD for total phyto and diatoms
effsz<-s_pd_sum_l %>% 
  group_by(type) %>% 
  summarize(
    bvol_mean = mean(tot_bvol2)
    ,bvol_sd = sd(tot_bvol2)
    , .groups = 'drop')

#calculate % of phyto biovolume comprised of diatoms
(sum(s_pd_sum_z$d_tot_bvol)/sum(s_pd_sum_z$tot_bvol))*100

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
#ggsave(file = paste0(sharepoint_path,"./Plots/SMSCG_Phyto_Boxplot_Phyto&Diatom_Region&Month.png"),type ="cairo-png",width=6, height=5,units="in",dpi=300)

#Statistics: total phyto biovolume--------

#predictors to include: region, month, maybe salinity
#could include station as a random effect. there are only 9 though

#build model
tbmod = glm(tot_bvol ~ region * month, data = s_phyto_sum)

#model checking plots
#plot(tbmod)
#plots aren't great
#residuals vs fitted: spread gets larger with higher values 
#Q-Q plot: points stray pretty far off diagonal at upper end 
#scale-location: line drifts up with higher values
#residuals vs leverage: looks OK

#redo analysis with log transformed response
tblmod = glm(log(tot_bvol) ~ region * month, data = s_phyto_sum)

#model checking plots
#plot(tblmod)
#looks like log transformation did the trick
#could also try a different error distribution like gamma

#look at model results
summary(tblmod)
drop1(tblmod, test="Chi")
#interaction term isn't significant 

#redo model with log transformed response and without interaction term
tblmod2 = glm(log(tot_bvol) ~ region  + month, data = s_phyto_sum)

#look at model results
summary(tblmod2)
drop1(tblmod2, test="Chi")
#neither region nor month is significant
#Df Deviance    AIC scaled dev. Pr(>Chi)
#<none>      19.120 122.34                     
#region  2   19.645 120.27      1.9235   0.3822
#month   3   19.894 119.16      2.8168   0.4207


#Statistics: total diatom biovolume--------

#predictors to include: region, month, maybe salinity
#could include station as a random effect. there are only 9 though

#Note: there are 7 zeros that are currently excluded from the data set used in these models
#s_pd_sum: these 7 samples are NAs
#s_pd_sum_z: these 7 samples are zeros
zeros <-filter(s_pd_sum_z, d_tot_bvol == 0)
#RV = 3, ME = 1, MW = 3

#build model
dbmod = glm(d_tot_bvol ~ region * month, data = s_pd_sum)

#model checking plots
#plot(dbmod)
#as with full data set, should do log transform

#redo analysis with log transformed response
dblmod = glm(log(d_tot_bvol) ~ region * month, data = s_pd_sum)

#model checking plots
#plot(dblmod)
#looks like log transformation did the trick
#could also try a different error distribution like gamma

#look at model results
summary(dblmod)
drop1(dblmod, test="Chi")
#interaction term isn't significant 

#redo model with log transformed response and without interaction term
dblmod2 = glm(log(d_tot_bvol) ~ region  + month, data = s_pd_sum)

#look at model results
summary(dblmod2)
drop1(dblmod2, test="Chi")
#neither region nor month is significant
#       Df Deviance    AIC scaled dev. Pr(>Chi)
#<none>      183.02 262.87                     
#region  2   184.86 259.51     0.64225   0.7253
#month   3   189.84 259.21     2.34374   0.5042


#NMDS plots---------------
#next step is use these plots to see how much regions overlap in composition
#the large number of rare taxa may make it difficult to generate these plots

#first create histogram of genera per sample




