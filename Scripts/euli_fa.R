# Script to look at fatty acids profile under ice vs summer stratification
# uses Hampton et al. ecology under lake ice dataset (ecology letters)
# and Galloway et al PLoS one paper fatty acid dataset

#libraries
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)

#===========================================================================
# ----> read in ecology under lake ice dataset and FA dataset

euli_orig <- read.csv("../Data/under_ice_data.csv", stringsAsFactors = FALSE)

fa_orig <- read.csv("../Data/PLoS_supp/S1_Dataset.csv", stringsAsFactors = FALSE)

#===========================================================================
# ----> wrangle ice phyto data

euli <- euli_orig %>% 
  #keep only columns of potential interest
  select(year, season, researcher, lakename, lakeregloc, lakecountry,
         startday, startmonth, startyear, endday, endmonth, endyear,
         iceduration, fadata, avesecchidepth, avechla, 
         phytomethod, avephytomass, avephytocount,
         propchloro, propcrypto, propcyano, propdiatom, propdiatom, propotherphyto) %>% 
  #only interested when *not* all NA for phyto props
  filter(!(is.na(propchloro) & is.na(propcrypto) & is.na(propcyano) & 
           is.na(propdiatom) & is.na(propdiatom) & is.na(propotherphyto)))

euli_phytos <- euli %>% 
  #start by looking just at pytos, can circle back to incorporate snow/ice/secchi etc.
  select(year, season, lakename, 
         propchloro, propcrypto, propcyano, propdiatom, propdiatom, propotherphyto)

summary(euli_phytos) #40 NA in crypto
str(euli_phytos) #211

#going to start by using only lakes with *NO NAs* for phytos
euli_phytos_complete <- euli_phytos %>% 
  filter(!(is.na(propchloro) | is.na(propcrypto) | is.na(propcyano) | 
             is.na(propdiatom) | is.na(propdiatom) | is.na(propotherphyto)))
#164 obvs - only "complete" phyto profiles

#24 lakes in total
unique(euli_phytos_complete$lakename) 

#what's the temporal coverage?
#only want ones with matching iceon/iceoff
euli_phytos_complete %>% 
  group_by(lakename) %>% 
  summarize(seasons = n_distinct(season)) %>% 
  as.data.frame() %>% 
  arrange(seasons)

#keep only those with multiple seasons
multi_seasons <- euli_phytos_complete %>% 
  group_by(lakename) %>% 
  summarize(seasons = n_distinct(season)) %>% 
  as.data.frame() %>% 
  arrange(seasons) %>% 
  filter(seasons > 1)

euli_phytos_complete_matching <- euli_phytos_complete %>% 
  filter(lakename %in% multi_seasons$lakename)

#check out remaining
str(euli_phytos_complete_matching)
str(unique(euli_phytos_complete_matching$lakename)) 
#16 lakes

#temporal coverage
euli_phytos_complete_matching %>% 
  group_by(lakename) %>% 
  summarize(yrs = n_distinct(year)) %>% 
  arrange(yrs) %>% 
  as.data.frame()
#                 lakename yrs
# 1  Blackstrap Reservoir   1
# 2   Broderick Reservoir   1
# 3      Lake Diefenbaker   1
# 4   Lake Santo Parmense   1
# 5           Simoncouche   1
# 6      St. Denis Pond 1   1
# 7     St. Denis Pond 90   1
# 8  St. Denis Pond S5338   1
# 9   Lake Valkea-Kotinen   2
# 10    Lake Vanajanselka   2
# 11             Lake 227   3
# 12           Saanajarvi   3
# 13             Lake 239   6
# 14      Scharmuetzelsee   9
# 15          Lake Monona  18
# 16         Lake Mendota  19

#make long to be equivalent to FA data
phytos_long <- euli_phytos_complete_matching %>% 
  melt(id.vars = c("lakename", "year", "season")) %>% 
  rename(phyto_group = variable, prop = value)

#remove prop from group name
phytos_long$phyto_group <- gsub("prop", "", phytos_long$phyto_group)

unique(phytos_long$phyto_group)
# "chloro"     "crypto"     "cyano"      "diatom"     "otherphyto"

#===========================================================================
# ----> wrangle FA data

fa <- fa_orig %>% 
  #only fresh water
  filter(Salinity == 0) %>% 
  #only proportional data
  filter(Data == "prop") %>% 
  #for now, don't care about experimental info
  select(Group, Class, Order, Genus, species,
         sumSAFA, sumMUFA, sumPUFA,
         c18.2w6, c18.3w6, c18.3w3,
         c18.4w3, c18.5w3, 
         c20.4w6, c20.5w3, c22.6w3)

unique(fa$Group)
# "Cyanobacteria" "Chlorophyta"   "Ochrophyta"    "Cryptophyta"   "Dinophyta"     "Euglenozoa"    "Rhodophyta"  

#crosswalk to match euli to FA
fa_equiv <- fa %>% 
  #crypto, chloro, cyano are direct matches
  mutate(fa_group = ifelse(Group == "Cryptophyta", "crypto", NA),
         fa_group = ifelse(Group == "Chlorophyta", "chloro", fa_group),
         fa_group = ifelse(Group == "Cyanobacteria", "cyano", fa_group),
         #diatoms are everything in these 3 classes in FA data
         fa_group = ifelse(Class %in% c("Bacillariophyceae", "Fragilariophyceae", "Coscinodiscophyceae"),
                           "diatom", fa_group),
         #other phyto = average of all else in FA data
         fa_group = ifelse(is.na(fa_group), "otherphyto", fa_group))

# fa_equiv %>% 
#   select(Group, fa_group) %>% 
#   unique()
# 
# fa_equiv %>% 
#   select(Group, Class, fa_group) %>% 
#   unique()


#===========================================================================
# ----> aggregate FA data

#aggregate to level of fa_group (average)

fa_group_avgs <- fa_equiv %>% 
  group_by(fa_group) %>% 
  summarize(grp_avg_sumSAFA = mean(sumSAFA, na.rm = TRUE),
            grp_avg_sumMUFA = mean(sumMUFA, na.rm = TRUE),
            grp_avg_sumPUFA = mean(sumPUFA, na.rm = TRUE),
            grp_avg_c18.2w6 = mean(c18.2w6, na.rm = TRUE),
            grp_avg_c18.3w6 = mean(c18.3w6, na.rm = TRUE),
            grp_avg_c18.3w3 = mean(c18.3w3, na.rm = TRUE),
            grp_avg_c18.4w3 = mean(c18.4w3, na.rm = TRUE),
            grp_avg_c18.5w3 = mean(c18.5w3, na.rm = TRUE),
            grp_avg_c20.4w6 = mean(c20.4w6, na.rm = TRUE),
            grp_avg_c20.5w3 = mean(c20.5w3, na.rm = TRUE),
            grp_avg_c22.6w3 = mean(c22.6w3, na.rm = TRUE)) %>% 
  as.data.frame()

fa_group_avgs

#===========================================================================
# ----> combine EULI and FA datasets

head(phytos_long) #lake, year, season, group, prop
head(fa_group_avgs) #group, FAs

#combine euli and FA - have group, proportion, and FA data
full_dat <- merge(phytos_long, fa_group_avgs, 
                  by.x = "phyto_group", by. = "fa_group", 
                  all.x = TRUE)

# From LTER FA script:
# What we want is "what are the average proportions of SAFA, MUFA, PUFA 
# present in the community, irrespective of total biomass?" 
# (i.e. we know there is higher biomass in summer - the Circus Circus buffet)
# SH: pretty sure that what we want then is:(biomass_dry_weight * sumSAFA)/total dry weight
# This is already a proportion (biomass/total) so can do prop * FA

#weight data accordingly
full_dat_weighted <- full_dat %>% 
  #prop phyto (already biomass/total biomass) * prop FA
  mutate(sumSAFA_prop = grp_avg_sumSAFA * prop,
         sumMUFA_prop = grp_avg_sumMUFA * prop, 
         sumPUFA_prop = grp_avg_sumPUFA * prop) %>% 
  #keep only columns of interest
  #for now, only SAFA, MUFA, PUFA
  select(lakename, year, season, phyto_group, 
         sumSAFA_prop, sumMUFA_prop, sumPUFA_prop)

#sum within lake/time point (get community-level FA)
full_dat_weighted_comm <- full_dat_weighted %>% 
  group_by(lakename, year, season) %>% 
  #sum across phyto groups
  summarize(MUFA_perc = sum(sumMUFA_prop, na.rm = TRUE),
            PUFA_perc = sum(sumPUFA_prop, na.rm = TRUE),
            SAFA_perc = sum(sumSAFA_prop, na.rm = TRUE)) %>% 
  as.data.frame()

#this format matches LTER_FA_20180217 
#(detailed look at lakes ME/MO)

head(full_dat_weighted_comm)

#make a verion with one point per lake/season (aggregate across years)
#include standard deviation as measure of variance, as well as count
full_dat_weighted_comm_agg <- full_dat_weighted_comm %>% 
  group_by(lakename, season) %>% 
  #using __ so can separate later
  summarize(n_years = n_distinct(year),
            seasonal_avg__MUFA_perc = mean(MUFA_perc, na.rm = TRUE),
            sd__MUFA_perc = sd(MUFA_perc, na.rm = TRUE),
            seasonal_avg__PUFA_perc = mean(PUFA_perc, na.rm = TRUE),
            sd__PUFA_perc = sd(PUFA_perc, na.rm = TRUE),
            seasonal_avg__SAFA_perc = mean(SAFA_perc, na.rm = TRUE),
            sd__SAFA_perc = sd(SAFA_perc, na.rm = TRUE)) %>% 
  as.data.frame()

#===========================================================================
# ----> look at all lakes/years/seasons

#make long for plotting
dat_long_full_points <- full_dat_weighted_comm %>% 
  melt(id.vars = c("lakename", "year", "season")) %>% 
  rename(FA_type = variable, FA_perc = value)

#matching graphs to ME/MO
ggplot(dat_long_full_points, aes(season, FA_perc)) +
  geom_boxplot() +
  geom_point(aes(color = lakename)) +
  facet_wrap(~FA_type)

#hmmm definitely not as extreme as ME/MO

#check just using those for sanity check
ggplot(filter(dat_long_full_points, lakename %in% c("Lake Mendota", "Lake Monona")), 
       aes(season, FA_perc)) +
  geom_boxplot() +
  geom_jitter(aes(color = lakename)) +
  facet_wrap(~FA_type)

#yep, see the same trends in just ME/MO
#so it's accurate
#(other has wider spread since it's actual data, not avg proportions)

#have some definite outliers in full dat
ggplot(dat_long_full_points, aes(season, FA_perc)) +
  geom_boxplot() +
  geom_jitter(width = 0.1,aes(color = season), alpha = 0.4) +
  facet_wrap(~FA_type)

#what do anovas say?

mufa1 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "MUFA_perc"))
summary(mufa1) #p=0.463

pufa1 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "PUFA_perc"))
summary(pufa1) #p=0.377

safa1 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "SAFA_perc"))
summary(safa1) #p=0.0102

#===========================================================================
# ----> look at lakes aggregated to seasons across years

#make long
dat_long_seasonal_points <- full_dat_weighted_comm_agg %>% 
  #make long
  melt(id.vars = c("lakename", "season", "n_years")) %>% 
  #split variable column name
  separate(variable, into = c("agg_type", "FA_type"), sep = "__") %>% 
  #make wide so have column for avg and sd
  dcast(lakename + season + n_years + FA_type ~ agg_type, value.var = "value")

#plot
ggplot(dat_long_seasonal_points, aes(season, seasonal_avg)) +
  geom_boxplot() +
  geom_point(aes(color = n_years, group = lakename), position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(x = season, ymin = seasonal_avg - sd, ymax = seasonal_avg + sd,
                    color = n_years, group = lakename),
                position = position_dodge(width = 0.2)) +
  # geom_pointrange(aes(x = season, y = seasonal_avg, 
  #                     ymin = seasonal_avg - sd, ymax = seasonal_avg + sd)) +
  facet_wrap(~FA_type)
  
#plot
ggplot(dat_long_seasonal_points, aes(season, seasonal_avg)) +
  #ignore outlier sofr boxplots only
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = lakename, group = lakename), position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(x = season, ymin = seasonal_avg - sd, ymax = seasonal_avg + sd,
                    color = lakename, group = lakename),
                position = position_dodge(width = 0.2)) +
  # geom_pointrange(aes(x = season, y = seasonal_avg, 
  #                     ymin = seasonal_avg - sd, ymax = seasonal_avg + sd)) +
  facet_wrap(~FA_type)









