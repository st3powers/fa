# ============================================================== #
## ========================= OVERVIEW ========================= ##
# ============================================================== #

# Looking at iceon/iceoff community FA profiles
# Only using Lakes Mendota and Monona from LTER,
# as they have equivalent iceon/iceoff paired data
# Using AG PLoS paper FA dataset, with some aggregation/crosswalking
# to match LTER data.

# ============================================================== #
## =================== DATA WRANGLING STEPS =================== ##
# ============================================================== #

# 1) LTER data - read in, format dates, keep only ME/MO
# 2) read in iceon/iceoff data (ecology letters paper)
# 3) tag LTER data with season, keep only iceon/iceoff
# 4) aggregate LTER samples to month (mean taxa biomass by lake/year/month)
# 5) identify genera <5% of any sample - remove these genera
# 6) calculate dry weight (20% of wet weight)
# 7) read in FA PLoS data - keep freshwater, proportion data
# --> 7.5) fix FA/LTER group/division matches (see crosswalk.xlsx)
# 8) aggregate FA to genus and division level profiles
# 9) aggregate biomass to genus level
# 10) tag LTER data with whether has matching division/Group or genus in FA data
# 11) merge LTER with FA data (genus, division, NA matches)
# 12) weight FA data by biomass dry weight / total biomass 
#     (genus-level FA, independent of biomass)
# 13) aggregated weighted to community level 



# ============================================================== #
## ====================== SCRIPT BEGINS ======================= ##
# ============================================================== #

#load packages
library(dplyr)
library(reshape2)
library(tidyr)
library(lubridate)
library(vegan)
library(RColorBrewer)
library(ggplot2)

# ============================================================================
# ----> read in LTER phyto data

#read in, format dates so can stack ok 

madison <- read.csv("../Data/ntl88_v7.csv", stringsAsFactors = FALSE) %>% 
  mutate(date = as.Date(sampledate, format = "%m/%d/%Y")) %>% 
  select(-sampledate)

#only madison lakes have matching iceon/iceoff data
lter_lakes <- madison %>% 
  filter(lakeid %in% c("MO", "ME"))

#keep only necessary files
lter_lakes <- lter_lakes %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  select(lakeid, year, month, date, sta, depth_range, 
         division, genus, taxa_name, biomass_conc)

#=============================================================================
# ----> read in under ice data, for ice on/off timing
seasons_orig <- read.csv("../Data/under_ice_data.csv", stringsAsFactors = FALSE)

#keep only wisconsin, cols of interest, make start and end full dates
seasons_wisc <- seasons_orig %>% 
  filter(lakeregloc == "Wisconsin") %>% 
  select(year, season, lakename, lakeregloc, 
         startday, startmonth, startyear, 
         endday, endmonth, endyear) %>% #, 
  #iceduration, periodn, photicdepth, 
  #icedepth, snowdepth, airtemp) %>% 
  mutate(startmonum = match(startmonth, month.abb),
         endmonum = match(endmonth, month.abb)) %>% 
  mutate(startdate = paste(startyear, startmonum, startday, sep = "-"),
         enddate = paste(endyear, endmonum, endday, sep = "-"))

#unique(seasons_wisc$lakename)
#unique(lter_lakes$lakeid) %>% sort()

#make lakeid to match
seasons_lakes <- seasons_wisc %>% 
  mutate(lakeid = ifelse(lakename == "Lake Mendota", "ME", NA),
         lakeid = ifelse(lakename == "Lake Monona", "MO", lakeid)) %>% 
  select(season, lakeid, year, startdate, enddate) %>% 
  filter(lakeid %in% c("ME", "MO"))

# Note: we are losing the 2014/2015/2016 data by using this to subset
# we may want to go back to raw LTER ice data to get 2014/2015/2016 iceon/iceoff dates

#=============================================================================
# ----> LTER ice duration data (for seasons)

#this gets the ice, but we don't have the summer stratified period
#(which is problematic...)

#read in LTER ice duration data
duration <- read.csv("../Data/ntl33_v4.csv", stringsAsFactors = FALSE)

#look like what we need
head(duration)

#keep only relevant info
duration_relevant <- duration %>%
  #only interested in Mendota, Monona
  filter(lakeid %in% c("ME", "MO")) %>% 
  #can narrow down year range
  filter(year4 >= 1990)

#not sure what to do about this

#=============================================================================
# ----> tag data with season

lter_dates_only <- lter_lakes %>% 
  select(lakeid, year, date) %>% 
  unique() %>% 
  rename(sample_year = year) %>% 
  arrange(lakeid, sample_year)

#make seasons wide format to can have column "start", "sample", and "end" dates
seasons_lakes_wide <- seasons_lakes %>% 
  melt(id.vars = c("lakeid", "year", "season"), variable.name = "category", value.name = "date") %>% 
  mutate(seasons_category = paste(season, category, sep = "_")) %>% 
  select(-season, -category) %>% 
  dcast(lakeid + year ~ seasons_category, value.var = "date") %>% 
  rename(winter_yr = year) %>% 
  select(lakeid, winter_yr, iceon_startdate, iceon_enddate, iceoff_startdate, iceoff_enddate)

#we're only interested in iceon/iceoff
#(looking at ice vs stratified patterns in FA)

lter_seasons_pre <- merge(lter_dates_only, seasons_lakes_wide,
                      by = c("lakeid"), all.x = TRUE) %>% 
  #tag seasons with iceon/iceoff (NA if not fall within start/end dates)
  mutate(season = ifelse(date >= iceon_startdate & date <= iceon_enddate, "iceon", NA),
         season = ifelse(date >= iceoff_startdate & date <= iceoff_enddate, "iceoff", season)) %>% 
  #keep only samples that fall within iceon or iceoff
  filter(!is.na(season))

#merge back to full lter data
lter_seasons <- lter_seasons_pre %>% 
  #group things by *winter* year
  select(lakeid, date, winter_yr, season) %>% 
  merge(lter_lakes, by = c("lakeid", "date")) %>% 
  select(-year)

#=============================================================================
# ----> aggregate samples to month

#aggregate to month within *winter* year and lake (ignore station and depth for now)
#since ignoring "other" seasons, shouldn't have issue of sample dates across seasons

#using mean since some dates have multiple samples per month, while others have one or none
#using averages to account for multiple within-month samples, make comparable

lter_month <- lter_seasons %>% 
  #monthly taxa mean biomass (across stations and depth)
  group_by(lakeid, winter_yr, season, month, division, genus, taxa_name) %>% 
  summarize(biomass_ww_m = mean(biomass_conc, na.rm = TRUE)) %>% 
  as.data.frame()

#check out number of years
# lter_month %>% 
#   select(lakeid, winter_yr, season) %>% 
#   group_by(lakeid, season) %>% 
#   summarize(n = n_distinct(winter_yr))


#========================================================================
# ----> identify <5% of samples

#identify contributors (genus-level) <5% of any sample any date (within lake)
#use wet weight (shouldn't matter)

#find contributions to biomass, in terms of %
lter_contribs <- lter_month %>% 
  #for each lake/year/month, find total biomass
  group_by(lakeid, winter_yr, month) %>% 
  mutate(total_bm = sum(biomass_ww_m)) %>% 
  ungroup() %>% 
  #for lake/date, find genus biomass
  group_by(lakeid, winter_yr, month, genus) %>% 
  mutate(genus_bm = sum(biomass_ww_m)) %>% 
  as.data.frame() %>% 
  select(-taxa_name, -biomass_ww_m) %>% 
  #find percent genus biomass of total biomass
  mutate(perc_genus_bm = genus_bm / total_bm * 100) %>% 
  ungroup()  

#find genera that are ever over 5% of a sample BY LAKE
lter_high_contrib <- lter_contribs %>% 
  filter(perc_genus_bm >= 0.05)

lter_high_contrib_genera <- lter_high_contrib %>% 
  select(lakeid, genus) %>% 
  unique() %>% 
  arrange(lakeid, genus) %>% 
  mutate(high_p = "present")

#find genera that are NOT ever over 5% of a sample
lter_low_contrib <- lter_contribs %>% 
  filter(perc_genus_bm < 0.05) 

lter_low_contrib_genera <- lter_low_contrib %>% 
  select(lakeid, genus) %>% 
  unique() %>% 
  arrange(lakeid, genus) %>% 
  mutate(low_p = "present")

#merge, keep ones that are present in low and not in high
#(i.e., those that are never >=5% of a sample)
lter_remove_5 <- merge(lter_high_contrib_genera, lter_low_contrib_genera,
                       by = c("lakeid", "genus"), all = TRUE) %>% 
  #these are ones that are never >=5% of a lake sample
  filter(low_p == "present" & is.na(high_p))


#========================================================================
# ----> LTER after removals of low contributors

#by lake/date - total genus biomass AFTER remove low contribs
lter_after_remove <- lter_month %>% 
  #remove low contributors by lake and genus
  filter( !((lakeid %in% lter_remove_5$lakeid) & (genus %in% lter_remove_5$genus)) ) 

#add a dry weight biomass column
lter_use <- lter_after_remove %>% 
  mutate(biomass_dw = 0.20 * biomass_ww_m)


#========================================================================
# ----> read in fatty acid profiles

# read in PLoS fatty acid dataset
fa <- read.csv("../Data/PLoS_supp/S1_Dataset.csv", stringsAsFactors = FALSE)

#only want freshwater
#fa %>% group_by(Salinity) %>% summarize(n = length(Salinity))
fa_fresh <- filter(fa, Salinity == 0)

#want only proportion data
fa_fresh_prop <- filter(fa_fresh, Data == "prop")

#for now, we don't care about the experimental info
fa_fresh_prop_small <- fa_fresh_prop %>% 
  select(Group, Class, Order, Genus, species,
         sumSAFA, sumMUFA, sumPUFA,
         c18.2w6, c18.3w6, c18.3w3,
         c18.4w3, c18.5w3, 
         c20.4w6, c20.5w3, c22.6w3)

#========================================================================
# ----> fix FA/LTER group/division matches

#existing matches: Chlorophyta, Cryptophyta, Rhodophyta

#make match at group/division level: 
#   Cyanobacteria (FA) = Cyanophyta (LTER)
#   Dinophyta (FA) = Pyrrhophyta (LTER)
#   Euglenozoa (FA) = Euglenophyta (LTER)

#match division to non-group
#   Chrysophyta (LTER) = class Chrysophyceae (FA)
#   Xanthophyta (LTER) = class Xanthophyceae (FA)
#   Haptophyta (LTER) = genus Chromulina (FA)
#   Bacillariophyta (LTER) = average of classes 
#                 Bacillariophyceae, Fragilariophyceae, Coscinodiscophyceae (FA)
#       [Bacillariophyta is multiple classes - for these 3 classes, find average FA profile]

fa_fix <- fa_fresh_prop_small %>% 
  #first, match at group level
  mutate(GroupDiv = ifelse(Group == "Cyanobacteria", "Cyanophyta", Group),
         GroupDiv = ifelse(Group == "Dinophyta", "Pyrrhophyta", GroupDiv),
         GroupDiv = ifelse(Group == "Euglenozoa", "Euglenophyta", GroupDiv),
         #match at single class level
         GroupDiv = ifelse(Class == "Chrysophyceae", "Chrysophyta", GroupDiv),
         GroupDiv = ifelse(Class == "Xanthophyceae", "Xanthophyta", GroupDiv),
         #match at genus level
         GroupDiv = ifelse(Genus == "Chromulina", "Haptophyta", GroupDiv),
         #Bacillariophyta is multiple classes
         GroupDiv = ifelse(Class %in% c("Bacillariophyceae", "Fragilariophyceae", "Coscinodiscophyceae"),
                           "Bacillariophyta", GroupDiv))

# fa_fix %>% select(Group, GroupDiv) %>% unique()
# fa_fix %>% filter(GroupDiv %in% c("Bacillariophyta", "Xanthophyta")) %>% select(Group, Class, GroupDiv) %>% unique()
# fa_fix %>% filter(GroupDiv == "Haptophyta") %>% select(Group, Class, Genus, GroupDiv) %>% unique()
# #yes, works as it should


#========================================================================
# ----> tag LTER data with whether matching genus/division in FA data

lter_fa_tag <- lter_use %>%
  #tag at which level it matches
  #not that groupdiv level is a combination of groups/classes/genus
  mutate(cat = ifelse(division %in% fa_fix$GroupDiv, "groupdiv", NA)) %>%
  mutate(cat = ifelse(genus %in% fa_fix$Genus, "genus", cat))

tags <- lter_fa_tag %>% select(division, genus, cat) %>% unique()

# unique(fa_fix$GroupDiv) %>% sort()
# unique(lter_use$division) %>% sort()


#========================================================================
# ----> find FA genus and division-level profiles

#within genus, what's the average FA profile
fa_genus <- fa_fix %>% 
  group_by(GroupDiv, Genus) %>% 
  summarize(gen_avg_sumSAFA = mean(sumSAFA),
            gen_avg_sumMUFA = mean(sumMUFA),
            gen_avg_sumPUFA = mean(sumPUFA),
            gen_avg_c18.2w6 = mean(c18.2w6),
            gen_avg_c18.3w6 = mean(c18.3w6),
            gen_avg_c18.3w3 = mean(c18.3w3),
            gen_avg_c18.4w3 = mean(c18.4w3),
            gen_avg_c18.5w3 = mean(c18.5w3),
            gen_avg_c20.4w6 = mean(c20.4w6),
            gen_avg_c20.5w3 = mean(c20.5w3),
            gen_avg_c22.6w3 = mean(c22.6w3)) %>% 
  as.data.frame()

#within group/div (FA/LTER fixed croswalk), what's the average FA profile
fa_groupdiv <- fa_fix %>% 
  group_by(GroupDiv) %>% 
  summarize(grpdiv_avg_sumSAFA = mean(sumSAFA),
            grpdiv_avg_sumMUFA = mean(sumMUFA),
            grpdiv_avg_sumPUFA = mean(sumPUFA),
            grpdiv_avg_c18.2w6 = mean(c18.2w6),
            grpdiv_avg_c18.3w6 = mean(c18.3w6),
            grpdiv_avg_c18.3w3 = mean(c18.3w3),
            grpdiv_avg_c18.4w3 = mean(c18.4w3),
            grpdiv_avg_c18.5w3 = mean(c18.5w3),
            grpdiv_avg_c20.4w6 = mean(c20.4w6),
            grpdiv_avg_c20.5w3 = mean(c20.5w3),
            grpdiv_avg_c22.6w3 = mean(c22.6w3)) %>% 
  as.data.frame()


#========================================================================
# ----> aggregate biomass to GENUS
# when missing genus, stick with GENUS biomass but sub in DIVISION fatty acid profile

#aggregate biomass to genus
lter_level_bm <- lter_use %>% 
  group_by(lakeid, winter_yr, season, month, division, genus) %>% 
  summarize(bm_ww_g = sum(biomass_ww_m, na.rm = TRUE),
         bm_drw_g = sum(biomass_dw, na.rm = TRUE)) %>% 
  as.data.frame() 

#merge with tag info
lter_level_bm_tag <- merge(lter_level_bm, tags, by = c("division", "genus"), all = TRUE)

# lter_level_bm_tag %>%
#   select(division, genus, cat) %>%
#   unique() %>%
#   group_by(cat) %>%
#   summarize(n = length(cat)) %>%
#   as.data.frame()

#        cat  n
# 1    genus 35
# 2 groupdiv 95
# 3     <NA>  1

#========================================================================
# ----> merge with FA profiles

# --> genus level FA - matches genus-level biomass
# merges GENUS level biomass with GENUS level FA profiles
tags_fa_genus <- lter_level_bm_tag %>% 
  #only looking at genus-level matches
  filter(cat == "genus") %>% 
  #rename for downstream stacking
  rename(biomass_wet_weight = bm_ww_g,
         biomass_dry_weight = bm_drw_g) %>% 
  #merge with FA genus level data
  merge(fa_genus,  
        by.x = "genus", by.y = "Genus",
        all.x = TRUE) %>% 
  #organize columns
  select(GroupDiv, genus, cat, lakeid, winter_yr, month, season,
         biomass_wet_weight, biomass_dry_weight,
         gen_avg_sumSAFA,
         gen_avg_sumMUFA,
         gen_avg_sumPUFA,
         gen_avg_c18.2w6,
         gen_avg_c18.3w6,
         gen_avg_c18.3w3,
         gen_avg_c18.4w3,
         gen_avg_c18.5w3,
         gen_avg_c20.4w6,
         gen_avg_c20.5w3,
         gen_avg_c22.6w3)

#remove labels in headers
names(tags_fa_genus) <- gsub("gen_avg_", "", names(tags_fa_genus))

# --> division level FA - matches division level biomass
# merges GENUS level biomass with DIVISION level FA profiles
# (missing genera in FA get division level FA)
tags_fa_groupdiv <- lter_level_bm_tag %>% 
  #keep only those matching at groupdiv level
  filter(cat == "groupdiv") %>% 
  #rename for downstream stacking
  rename(biomass_wet_weight = bm_ww_g,
         biomass_dry_weight = bm_drw_g) %>% 
  #merge with FA division/group level data
  #merge with FA genus level data
  merge(fa_groupdiv,  
        by.x = "division", by.y = "GroupDiv",
        all.x = TRUE) %>% 
  rename(GroupDiv = division) %>% 
  #organize columns
  select(GroupDiv, genus, cat, lakeid, winter_yr, month, season,
         biomass_wet_weight, biomass_dry_weight,
         grpdiv_avg_sumSAFA,
         grpdiv_avg_sumMUFA,
         grpdiv_avg_sumPUFA,
         grpdiv_avg_c18.2w6,
         grpdiv_avg_c18.3w6,
         grpdiv_avg_c18.3w3,
         grpdiv_avg_c18.4w3,
         grpdiv_avg_c18.5w3,
         grpdiv_avg_c20.4w6,
         grpdiv_avg_c20.5w3,
         grpdiv_avg_c22.6w3)

#remove labels in headers
names(tags_fa_groupdiv) <- gsub("grpdiv_avg_", "", names(tags_fa_groupdiv))

# --> does NOT have associated FA
#leaving biomass at genus level
tags_fa_NA <- lter_level_bm_tag %>% 
  filter(is.na(cat)) %>% 
  rename(biomass_wet_weight = bm_ww_g,
         biomass_dry_weight = bm_drw_g) %>% 
  rename(GroupDiv = division) %>% 
  select(GroupDiv, genus, cat, lakeid, winter_yr, month, season,
         biomass_wet_weight, biomass_dry_weight) %>% 
  #add NA columns
  mutate(          sumSAFA = NA,
                   sumMUFA = NA,
                   sumPUFA = NA,
                   c18.2w6 = NA,
                   c18.3w6 = NA,
                   c18.3w3 = NA,
                   c18.4w3 = NA,
                   c18.5w3 = NA,
                   c20.4w6 = NA,
                   c20.5w3 = NA,
                   c22.6w3 = NA)

# --> merge together
full_dat <- rbind(tags_fa_genus, tags_fa_groupdiv, tags_fa_NA)


#========================================================================
# ----> weight FA profiles by biomass

# Data == "prop" is FA % of dry weight
# so it's a fatty acid proportion of dry weight (of genus or division, by cat)

# What we want is "what are the average proportions of SAFA, MUFA, PUFA 
# present in the community, irrespective of total biomass?" 
# (i.e. we know there is higher biomass in summer - the Circus Circus buffet)
# SH: pretty sure that what we want then is:(biomass_dry_weight * sumSAFA)/total dry weight

full_dat_weighted <- full_dat %>% 
  #find total dry weight biomass for each *sample point*
  group_by(lakeid, winter_yr, season, month) %>% 
  mutate(sample_date_dry_weight_total = sum(biomass_dry_weight)) %>% 
  ungroup() %>% 
  #weight FA by biomass - per row (genus within sample date)
  #if FA is % dry weight, then FA * dry weight = biomass of that FA
  #here, sample_date_dry_weight_total is community-level biomass
  #FA props are genus-level
  mutate(sumSAFA_prop = (biomass_dry_weight * sumSAFA)/sample_date_dry_weight_total,
         sumMUFA_prop = (biomass_dry_weight * sumMUFA)/sample_date_dry_weight_total,
         sumPUFA_prop = (biomass_dry_weight * sumPUFA)/sample_date_dry_weight_total,
         c18.2w6_prop = (biomass_dry_weight * c18.2w6)/sample_date_dry_weight_total,
         c18.3w6_prop = (biomass_dry_weight * c18.3w6)/sample_date_dry_weight_total,
         c18.3w3_prop = (biomass_dry_weight * c18.3w3)/sample_date_dry_weight_total,
         c18.4w3_prop = (biomass_dry_weight * c18.4w3)/sample_date_dry_weight_total,
         c18.5w3_prop = (biomass_dry_weight * c18.5w3)/sample_date_dry_weight_total,
         c20.4w6_prop = (biomass_dry_weight * c20.4w6)/sample_date_dry_weight_total,
         c20.5w3_prop = (biomass_dry_weight * c20.5w3)/sample_date_dry_weight_total,
         c22.6w3_prop = (biomass_dry_weight * c22.6w3)/sample_date_dry_weight_total) %>% 
  as.data.frame()

#filter(full_dat_weighted, lakeid == "MO" & winter_yr == 2002 & month == 8)

#keep only columns of interest
full_dat_weighted_small <- full_dat_weighted %>% 
  select(GroupDiv, genus, cat, lakeid, winter_yr, month, season, 
         sumSAFA_prop, sumMUFA_prop, sumPUFA_prop,
         c18.2w6_prop, c18.3w6_prop, c18.3w3_prop,
         c18.4w3_prop, c18.5w3_prop,
         c20.4w6_prop, c20.5w3_prop, c22.6w3_prop)

#write to csv
write.csv(full_dat_weighted_small, "../Data/LTER_madison_FA_weighted_groupdiv.csv", row.names = FALSE)

#this is basically saying:
#for each time point, a genus has some proportion FAs
#we want to aggregate to community, then to season

#ADD UP all MUFA/PUFA/SAFA within a community
#to get community-level amount
#I have proved to myself using pen, paper, and basic algebra that this will work

full_dat_weighted_date_FA <- full_dat_weighted_small %>% 
  #get community-level FA props for each time point
  group_by(lakeid, winter_yr, month, season) %>% 
  summarize(MUFA_perc = sum(sumMUFA_prop, na.rm = TRUE),
            PUFA_perc = sum(sumPUFA_prop, na.rm = TRUE),
            SAFA_perc = sum(sumSAFA_prop, na.rm = TRUE),
            c18.2w6_perc = sum(c18.2w6_prop, na.rm = TRUE),
            c18.3w6_perc = sum(c18.3w6_prop, na.rm = TRUE),
            c18.3w3_perc = sum(c18.3w3_prop, na.rm = TRUE),
            c18.4w3_perc = sum(c18.4w3_prop, na.rm = TRUE),
            c18.5w3_perc = sum(c18.5w3_prop, na.rm = TRUE),
            c20.4w6_perc = sum(c20.4w6_prop, na.rm = TRUE),
            c20.5w3_perc = sum(c20.5w3_prop, na.rm = TRUE),
            c22.6w3_perc = sum(c22.6w3_prop, na.rm = TRUE)) %>% 
  as.data.frame()

# summary(full_dat_weighted_date_FA) 

# #now aggregate to season - find average MUFA/PUFA/SAFA profile for each season (all years, boths lakes)
# full_dat_weighted_date_FA %>% 
#   group_by(season) %>% 
#   summarize(MUFA_perc_avg = mean(MUFA_perc),
#             PUFA_perc_avg = mean(PUFA_perc),
#             SAFA_perc_avg = mean(SAFA_perc)) %>% 
#   as.data.frame()
# #doesn't quite add up to 100%, more like 95%, but not unexpected since on average

#aggregate to lake/month/year/season
full_dat_weighted_month_season_FA <- full_dat_weighted_date_FA %>% 
  #aggregate across months within season, find average FAs
  group_by(lakeid, winter_yr, season, month) %>% 
  summarize(MUFA_perc_avg = mean(MUFA_perc, na.rm = TRUE),
            PUFA_perc_avg = mean(PUFA_perc, na.rm = TRUE),
            SAFA_perc_avg = mean(SAFA_perc, na.rm = TRUE),
            c18.2w6_perc_avg = mean(c18.2w6_perc, na.rm = TRUE),
            c18.3w6_perc_avg = mean(c18.3w6_perc, na.rm = TRUE),
            c18.3w3_perc_avg = mean(c18.3w3_perc, na.rm = TRUE),
            c18.4w3_perc_avg = mean(c18.4w3_perc, na.rm = TRUE),
            c18.5w3_perc_avg = mean(c18.5w3_perc, na.rm = TRUE),
            c20.4w6_perc_avg = mean(c20.4w6_perc, na.rm = TRUE),
            c20.5w3_perc_avg = mean(c20.5w3_perc, na.rm = TRUE),
            c22.6w3_perc_avg = mean(c22.6w3_perc, na.rm = TRUE)) %>% 
  as.data.frame()

#aggregate to lake/year/season
full_dat_weighted_yr_season_FA <- full_dat_weighted_date_FA %>% 
  #aggregate across months within season, find average FAs
  group_by(lakeid, winter_yr, season) %>% 
  summarize(MUFA_perc_avg = mean(MUFA_perc, na.rm = TRUE),
            PUFA_perc_avg = mean(PUFA_perc, na.rm = TRUE),
            SAFA_perc_avg = mean(SAFA_perc, na.rm = TRUE),
            c18.2w6_perc_avg = mean(c18.2w6_perc, na.rm = TRUE),
            c18.3w6_perc_avg = mean(c18.3w6_perc, na.rm = TRUE),
            c18.3w3_perc_avg = mean(c18.3w3_perc, na.rm = TRUE),
            c18.4w3_perc_avg = mean(c18.4w3_perc, na.rm = TRUE),
            c18.5w3_perc_avg = mean(c18.5w3_perc, na.rm = TRUE),
            c20.4w6_perc_avg = mean(c20.4w6_perc, na.rm = TRUE),
            c20.5w3_perc_avg = mean(c20.5w3_perc, na.rm = TRUE),
            c22.6w3_perc_avg = mean(c22.6w3_perc, na.rm = TRUE)) %>% 
  as.data.frame()
#note: doesn't add up to quite 100% (working with averages)
#also, data only goes to 2013 (may need to adjust iceon/iceoff dates from raw LTER snow/ice data)

summary(full_dat_weighted_yr_season_FA)
#MUFA: 11-36
#PUFA: 24-61
#SAFA: 22-47

#write to csv
write.csv(full_dat_weighted_yr_season_FA, "../Data/LTER_Madison_weighted_year_season_FA.csv", row.names = FALSE)

# ============================================================== #
## ========= END OF DATA WRANGLING - ANALYSIS BEGINS ========== ##
# ============================================================== #


LTER_Madison_weighted_year_season_FA<-read.csv("../Data/LTER_Madison_weighted_year_season_FA.csv")


#MRB edits below:

View(full_dat_weighted_yr_season_FA)


#Figure 3:

full_dat_weighted_yr_season_FA_long <- melt(data = full_dat_weighted_yr_season_FA, 
                                       measure.vars = c("MUFA_perc_avg","PUFA_perc_avg","SAFA_perc_avg"),
                                       id.vars = c("lakeid","season"))

full_dat_weighted_yr_season_FA_long$variable<-as.character(full_dat_weighted_yr_season_FA_long$variable)
full_dat_weighted_yr_season_FA_long$variable[which(full_dat_weighted_yr_season_FA_long$variable=="MUFA_perc_avg")]<-"% MUFA"
full_dat_weighted_yr_season_FA_long$variable[which(full_dat_weighted_yr_season_FA_long$variable=="PUFA_perc_avg")]<-"% PUFA"
full_dat_weighted_yr_season_FA_long$variable[which(full_dat_weighted_yr_season_FA_long$variable=="SAFA_perc_avg")]<-"% SAFA"

madison_FA <- ggplot(data = full_dat_weighted_yr_season_FA_long, aes(x = season, y = value)) + 
  geom_boxplot(outlier.shape="") +
  geom_jitter(aes(color = lakeid), width = 0.1,size=0.7) +
 # geom_point(aes(color = lakeid)) + 
  facet_wrap(~variable) + 
  xlab("Season") + ylab("% of Total FA")  +
  scale_color_manual(name = "Lake ID", values = c("royalblue3","green3"))+
  theme_bw()


#png(filename = "../Figures/Madison_FA_plot.png",width = 6, height = 6, units = "in", res = 500)
png(filename = "../Figures/Madison_FA_plot.png",width = 4, height = 2.5, units = "in", res = 500)
madison_FA
dev.off()


#Figure 4: 

#Taken from old version of script

omegas_chains <- full_dat_weighted_yr_season_FA %>% 
  #total omega 3 prop of PUFAs
  mutate(total_omega3 = c18.3w3_perc_avg +  
           c18.4w3_perc_avg +  
           c18.5w3_perc_avg +  
           c20.5w3_perc_avg +  
           c22.6w3_perc_avg,
         #total omega 6 prop of PUFAs
         total_omega6 = c18.2w6_perc_avg +  
           c18.3w6_perc_avg +  
           c20.4w6_perc_avg,
         #total short chain PUFAs (<20 C)
         total_short_chain = c18.2w6_perc_avg + 
           c18.3w6_perc_avg + 
           c18.3w3_perc_avg + 
           c18.4w3_perc_avg + 
           c18.5w3_perc_avg,
         #total long chain PUFAs (AG defined as >= 20 C)
         total_long_chain = c20.4w6_perc_avg + 
           c20.5w3_perc_avg + 
           c22.6w3_perc_avg) %>% 
  mutate(omega6_omega3_ratio = total_omega6/total_omega3,
         #may want just long chain PUFA down the road (%)
         longchainPUFA = (total_long_chain * PUFA_perc_avg)/100,
         #total long chain is % of PUFAs that are long chain
         #divide by PUFAs prop to get ratio with SAFA
         #(i.e., proportion of ALL FATTY ACIDS that are long chain PUFAs compared to SAFA prop)
         longchainPUFA_SAFA = longchainPUFA/ SAFA_perc_avg, 
         omega3_omega6_ratio = total_omega3/total_omega6) #added by MRB on 6/4/18

#plot
#madison_omega_ratio_plot_6.3 <- ggplot(omegas_chains, aes(season, omega6_omega3_ratio)) +
#  geom_boxplot(outlier.shape="") +
#  geom_jitter(aes(color = lakeid), width = 0.1, size=0.6) +
#  ylab("Omega 6:Omega 3 Ratio") + xlab("Season") + 
#  scale_color_manual(name = "Lake ID", values = c("royalblue3","green3")) +
#  theme_bw()

madison_omega_ratio_plot_3.6 <- ggplot(omegas_chains, aes(season, omega3_omega6_ratio)) +
  geom_boxplot(outlier.shape="") +
  geom_jitter(aes(color = lakeid), width = 0.1, size=1.5) +
  ylab("Omega 3:Omega 6 Ratio") + xlab("Season") + 
  scale_color_manual(name = "Lake ID", values = c("royalblue3","green3")) +
  theme_bw()+
  theme(axis.title.y = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) #must be used after theme_bw()

  
#png(filename = "../Figures/madison_omega_ratio_plot3v6.png",width = 6, height = 6, units = "in", res = 500)
png(filename = "../Figures/madison_omega_ratio_plot_3v6.png",width = 3.75, height = 4, units = "in", res = 500)
madison_omega_ratio_plot_3.6
dev.off()


######################
# fa ~ secchi 

fa_secchi <- read.csv("../Data/fa_secchi_data.csv", stringsAsFactors = FALSE)
fa_secchi$winter_yr2<-substr(fa_secchi$winter_yr,3,4)
fa_secchi$winter_yr2[which(fa_secchi$season=="iceoff")]<-""
fa_secchi$fill<-"Y"
fa_secchi$fill[which(fa_secchi$season=="iceoff")]<-"N"
#fa_secchi$shape<-24
#fa_secchi$shape[which(fa_secchi$season=="iceoff")]<-20

#seasonal patterns
fasecchi_MEMO_oneseason_onevalue<-ggplot(fa_secchi, aes(x=mean_secnview, y=(MUFA_perc_avg + PUFA_perc_avg))) +
  geom_point(size=3, aes(shape = season , colour= lakeid)) +
  ylab("MUFA + PUFA (% of Total FA)")+
  xlab("Secchi Depth (m)")+
  scale_shape_manual(name = "Season", values=c(0,16))+
  scale_color_manual(name = "Lake ID", values = c("royalblue3","green3"))+
  theme_bw()

png(filename = "../Figures/fasecchi_MEMO_oneseason_onevalue.png",width = 4, height = 3, units = "in", res = 500)
fasecchi_MEMO_oneseason_onevalue
dev.off()

