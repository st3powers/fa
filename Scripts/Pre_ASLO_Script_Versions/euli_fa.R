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
#===========================================================================

euli_orig <- read.csv("../Data/under_ice_data.csv", stringsAsFactors = FALSE)

fa_orig <- read.csv("../Data/PLoS_supp/S1_Dataset.csv", stringsAsFactors = FALSE)

#===========================================================================
# ----> wrangle ice phyto data
#===========================================================================

euli <- euli_orig %>% 
  #keep only columns of potential interest
  select(year, season, researcher, lakename, stationname, poolstation, lakeregloc, lakecountry,
         startday, startmonth, startyear, endday, endmonth, endyear,
         iceduration, fadata, avesecchidepth, avechla, 
         phytomethod, avephytomass, avephytocount,
         propchloro, propcrypto, propcyano, propdiatom, propdiatom, propotherphyto) %>% 
  #only interested when *not* all NA for phyto props
  filter(!(is.na(propchloro) & is.na(propcrypto) & is.na(propcyano) & 
           is.na(propdiatom) & is.na(propdiatom) & is.na(propotherphyto)))

#some lakes have multiple stations within lake
euli %>% 
  group_by(lakename) %>% 
  summarize(n = n_distinct(stationname)) %>% 
  filter(n > 1) %>% 
  as.data.frame()
#               lakename n
# 1 Blackstrap Reservoir 2
# 2     Lake Diefenbaker 3

euli %>% 
  group_by(lakename) %>% 
  summarize(n = n_distinct(poolstation)) %>% 
  as.data.frame() %>% 
  filter(n > 1)

#1:1 match lakename:poolstation
# filter(euli, lakename %in% c("Blackstrap Reservoir", "Lake Diefenbaker")) %>% select(lakename, stationname, poolstation)

#aggregate to lake/poolstation
euli_lakes_phytos <- euli %>% 
  #start by looking just at pytos, can circle back to incorporate snow/ice/secchi etc.
  select(year, season, lakename, poolstation, 
         propchloro, propcrypto, propcyano, propdiatom, propotherphyto) %>% 
  #group and aggregate
  group_by(lakename, poolstation, year, season) %>% 
  summarize(prop_chloro = mean(propchloro, na.rm = TRUE),
            prop_crypto = mean(propcrypto, na.rm = TRUE),
            prop_cyano = mean(propcyano, na.rm = TRUE),
            prop_diatom = mean(propdiatom, na.rm = TRUE),
            prop_other = mean(propotherphyto, na.rm = TRUE)) %>% 
  ungroup() %>% 
  as.data.frame()
  
summary(euli_lakes_phytos) #40 NA in crypto
str(euli_lakes_phytos) #205

#going to start by using only lakes with *NO NAs* for phytos
euli_phytos_complete <- euli_lakes_phytos %>% 
  filter(!(is.na(prop_chloro) | is.na(prop_crypto) | is.na(prop_cyano) | 
             is.na(prop_diatom) | is.na(prop_other)))
#158 obvs - only "complete" phyto profiles

#24 lakes in total
unique(euli_phytos_complete$lakename) 

#what's the temporal coverage?
#only want ones with matching iceon/iceoff
euli_phytos_complete %>% 
  group_by(lakename) %>% 
  summarize(seasons = n_distinct(season)) %>% 
  as.data.frame() %>% 
  arrange(seasons)

#identity lakes with both seasons per year
multi_seasons <- euli_phytos_complete %>% 
  group_by(lakename, year) %>% 
  summarize(seasons = n_distinct(season)) %>% 
  as.data.frame() %>% 
  arrange(seasons) %>% 
  filter(seasons > 1) %>% 
  mutate(lakeyear = paste(lakename, year, sep = "_"))

#keep only those lake/year/seasons
euli_phytos_complete_matching <- euli_phytos_complete %>% 
  mutate(lakeyear = paste(lakename, year, sep = "_")) %>% 
  filter(lakeyear %in% multi_seasons$lakeyear)

#make sure 1:1 match (i.e., iceon/iceoff in single year for lake - no missing pairs singles)
# euli_phytos_complete_matching %>% 
#   group_by(lakename, year) %>% 
#   summarize(n = n_distinct(season)) %>% 
#   filter(n != 2)
#correct

# filter(euli_phytos_complete, lakename == "Saanajarvi")
# filter(euli, lakename == "Saanajarvi")
#yep...being true to the data, we ignore Saanajarvi
#I think it's actually a goof in the euli data, 
#but moving forward without it...

#check out remaining
str(euli_phytos_complete_matching)
str(unique(euli_phytos_complete_matching$lakename)) 
#15 lakes

#temporal coverage
lake_years <- euli_phytos_complete_matching %>% 
  group_by(lakename) %>% 
  summarize(yrs = n_distinct(year)) %>% 
  arrange(yrs) %>% 
  as.data.frame()
#                lakename yrs
# 1  Blackstrap Reservoir   1
# 2   Broderick Reservoir   1
# 3      Lake Diefenbaker   1
# 4   Lake Santo Parmense   1
# 5     Lake Vanajanselka   1
# 6           Simoncouche   1
# 7      St. Denis Pond 1   1
# 8     St. Denis Pond 90   1
# 9  St. Denis Pond S5338   1
# 10  Lake Valkea-Kotinen   2
# 11             Lake 227   3
# 12             Lake 239   6
# 13      Scharmuetzelsee   9
# 14          Lake Monona  12
# 15         Lake Mendota  16

#may be interested in lakes with >= 3 years down the road
lake_years_3more <- filter(lake_years, yrs >= 3)

#make long to be equivalent to FA data
phytos_long <- euli_phytos_complete_matching %>% 
  select(-poolstation, -lakeyear) %>% 
  melt(id.vars = c("lakename", "year", "season")) %>% 
  rename(phyto_group = variable, prop = value)

#remove prop from group name
phytos_long$phyto_group <- gsub("prop_", "", phytos_long$phyto_group)

unique(phytos_long$phyto_group)
# "chloro"     "crypto"     "cyano"      "diatom"     "other"


#===========================================================================
# ----> wrangle FA data
#===========================================================================

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
         fa_group = ifelse(is.na(fa_group), "other", fa_group))

# fa_equiv %>% 
#   select(Group, fa_group) %>% 
#   unique()
# 
# fa_equiv %>% 
#   select(Group, Class, fa_group) %>% 
#   unique()
#
# fa_equiv %>% group_by(fa_group) %>% summarize(n = n_distinct(Genus)) %>% as.data.frame()
#   fa_group  n
# 1   chloro 13
# 2   crypto  2
# 3    cyano 22
# 4   diatom  8
# 5    other 11


#===========================================================================
# ----> aggregate FA data
#===========================================================================

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
#===========================================================================

head(phytos_long) #lake, year, season, group, prop
head(fa_group_avgs) #group, FAs

#combine euli and FA - have group, proportion, and FA data
full_dat <- merge(phytos_long, fa_group_avgs, 
                  by.x = "phyto_group", by.y = "fa_group", 
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
         sumPUFA_prop = grp_avg_sumPUFA * prop,
         prop_c18.2w6 = grp_avg_c18.2w6 * prop,
         prop_c18.3w6 = grp_avg_c18.3w6 * prop,
         prop_c18.3w3 = grp_avg_c18.3w3 * prop,
         prop_c18.4w3 = grp_avg_c18.4w3 * prop,
         prop_c18.5w3 = grp_avg_c18.5w3 * prop,
         prop_c20.4w6 = grp_avg_c20.4w6 * prop,
         prop_c20.5w3 = grp_avg_c20.5w3 * prop,
         prop_c22.6w3 = grp_avg_c22.6w3 * prop) %>% 
  #keep only columns of interest
  #for now, only SAFA, MUFA, PUFA
  select(lakename, year, season, phyto_group, 
         sumSAFA_prop, sumMUFA_prop, sumPUFA_prop,
         prop_c18.2w6,prop_c18.3w6,  prop_c18.3w3,
         prop_c18.4w3, prop_c18.5w3,
         prop_c20.4w6, prop_c20.5w3, 
         prop_c22.6w3)

#==============================================================================================

full_dat_weighted %>% 
  group_by(lakename, year, season) %>% 
  summarize(totSAFA = sum(sumSAFA_prop, na.rm = TRUE),
            totMUFA = sum(sumMUFA_prop, na.rm = TRUE),
            totPUFA = sum(sumPUFA_prop, na.rm = TRUE),
            total = totSAFA + totMUFA + totPUFA) %>% 
  as.data.frame() %>% 
  filter(total < 90) %>% 
  arrange(desc(total))

#pretty low totals...
#because high "other" category which means FA for other is kind of arbitrary?
#perhaps better to find total of non-other, recalculate %, then move forward

#==============================================================================================


#sum within lake/time point (get community-level FA)
full_dat_weighted_comm <- full_dat_weighted %>% 
  group_by(lakename, year, season) %>% 
  #sum across phyto groups
  summarize(MUFA_perc = sum(sumMUFA_prop, na.rm = TRUE),
            PUFA_perc = sum(sumPUFA_prop, na.rm = TRUE),
            SAFA_perc = sum(sumSAFA_prop, na.rm = TRUE),
            perc_c18.2w6 = sum(prop_c18.2w6, na.rm = TRUE),
            perc_c18.3w6 = sum(prop_c18.3w6, na.rm = TRUE),
            perc_c18.3w3 = sum(prop_c18.3w3, na.rm = TRUE),
            perc_c18.4w3 = sum(prop_c18.4w3, na.rm = TRUE),
            perc_c18.5w3 = sum(prop_c18.5w3, na.rm = TRUE),
            perc_c20.4w6 = sum(prop_c20.4w6, na.rm = TRUE),
            perc_c20.5w3 = sum(prop_c20.5w3, na.rm = TRUE),
            perc_c22.6w3 = sum(prop_c22.6w3, na.rm = TRUE)) %>% 
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
            sd__SAFA_perc = sd(SAFA_perc, na.rm = TRUE),
            
            seasonal_avg__c18.2w6 = mean(perc_c18.2w6, na.rm = TRUE),
            seasonal_avg__c18.3w6 = mean(perc_c18.3w6, na.rm = TRUE),
            seasonal_avg__c18.3w3 = mean(perc_c18.3w3, na.rm = TRUE),
            seasonal_avg__c18.4w3 = mean(perc_c18.4w3, na.rm = TRUE),
            seasonal_avg__c18.5w3 = mean(perc_c18.5w3, na.rm = TRUE),
            seasonal_avg__c20.4w6 = mean(perc_c20.4w6, na.rm = TRUE),
            seasonal_avg__c20.5w3 = mean(perc_c20.5w3, na.rm = TRUE),
            seasonal_avg__c22.6w3 = mean(perc_c22.6w3, na.rm = TRUE),
            
            sd__c18.2w6 = sd(perc_c18.2w6, na.rm = TRUE),
            sd__c18.3w6 = sd(perc_c18.3w6, na.rm = TRUE),
            sd__c18.3w3 = sd(perc_c18.3w3, na.rm = TRUE),
            sd__c18.4w3 = sd(perc_c18.4w3, na.rm = TRUE),
            sd__c18.5w3 = sd(perc_c18.5w3, na.rm = TRUE),
            sd__c20.4w6 = sd(perc_c20.4w6, na.rm = TRUE),
            sd__c20.5w3 = sd(perc_c20.5w3, na.rm = TRUE),
            sd__c22.6w3 = sd(perc_c22.6w3, na.rm = TRUE)) %>% 
  as.data.frame()

#===========================================================================
# ----> look at ALL lakes/years/seasons (MUFA/PUFA/SAFA)
#===========================================================================

#make long for plotting
dat_long_full_points <- full_dat_weighted_comm %>% 
  melt(id.vars = c("lakename", "year", "season")) %>% 
  rename(FA_type = variable, FA_perc = value)

# ----> plot for all lakes
ggplot(filter(dat_long_full_points, FA_type %in% c("MUFA_perc", "PUFA_perc", "SAFA_perc")),
              aes(season, FA_perc)) +
  geom_boxplot() +
  #geom_jitter(aes(color = lakename), width = 0.1, size = 1) +
  facet_wrap(~FA_type)

#what do anovas say for all lakes?
mufa1 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "MUFA_perc"))
summary(mufa1) #p=0.417

pufa1 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "PUFA_perc"))
summary(pufa1) #p=0.962

safa1 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "SAFA_perc"))
summary(safa1) #p=0.0255 *
#SAFA signif higher in summer


# ----> check just ME/MO 
ggplot(filter(dat_long_full_points, lakename %in% c("Lake Mendota", "Lake Monona") &
                FA_type %in% c("MUFA_perc", "PUFA_perc", "SAFA_perc")), 
       aes(season, FA_perc)) +
  geom_boxplot() +
  geom_jitter(aes(color = lakename), width = 0.1) +
  facet_wrap(~FA_type)
#SAFA more extreme


# ----> removing ME/MO
ggplot(filter(dat_long_full_points, !(lakename %in% c("Lake Mendota", "Lake Monona")) &
                FA_type %in% c("MUFA_perc", "PUFA_perc", "SAFA_perc")), 
       aes(season, FA_perc)) +
  geom_boxplot() +
  geom_jitter(aes(color = lakename), width = 0.1) +
  facet_wrap(~FA_type)
#pretty wide spread

#what do anovas say when remove ME/MO?
mufa2 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "MUFA_perc" &
                                              !(lakename %in% c("Lake Mendota", "Lake Monona"))))
summary(mufa2) #p=0.324

pufa2 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "PUFA_perc" &
                                              !(lakename %in% c("Lake Mendota", "Lake Monona"))))
summary(pufa2) #p=0.962

safa2 <- aov(FA_perc ~ season, dat = filter(dat_long_full_points, FA_type == "SAFA_perc" &
                                              !(lakename %in% c("Lake Mendota", "Lake Monona"))))
summary(safa2) #p=0.0859
#none signif


#===========================================================================
# ----> look at lakes aggregated to seasons across years (MUFA/PUFA/SAFA)
#===========================================================================

#make long
dat_long_seasonal_points <- full_dat_weighted_comm_agg %>% 
  #make long
  melt(id.vars = c("lakename", "season", "n_years")) %>% 
  #split variable column name
  separate(variable, into = c("agg_type", "FA_type"), sep = "__") %>% 
  #make wide so have column for avg and sd
  dcast(lakename + season + n_years + FA_type ~ agg_type, value.var = "value")
  
# ----> plot for all lakes
ggplot(filter(dat_long_seasonal_points, FA_type %in% c("MUFA_perc", "PUFA_perc", "SAFA_perc")),
              aes(season, seasonal_avg)) +
  #ignore outlier sofr boxplots only
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(group = lakename, color = lakename), position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(x = season, ymin = seasonal_avg - sd, ymax = seasonal_avg + sd,
                    color = lakename, 
                    group = lakename),
                position = position_dodge(width = 0.2)) +
  # geom_pointrange(aes(x = season, y = seasonal_avg, 
  #                     ymin = seasonal_avg - sd, ymax = seasonal_avg + sd)) +
  facet_wrap(~FA_type) +
  ggtitle("all lakes (aggregated to lake/season)")

#anova for all lakes, aggregate to one point per lake/season across years
mufa3 <- aov(seasonal_avg ~ season, dat = filter(dat_long_seasonal_points, FA_type == "MUFA_perc"))
summary(mufa3) #p=0.949

pufa3 <- aov(seasonal_avg ~ season, dat = filter(dat_long_seasonal_points, FA_type == "PUFA_perc"))
summary(pufa3) #p=0.709

safa3 <- aov(seasonal_avg ~ season, dat = filter(dat_long_seasonal_points, FA_type == "SAFA_perc"))
summary(safa3) #p=0.374
#none signif

# ----> ignore mendota/monona
ggplot(filter(dat_long_seasonal_points, 
              FA_type %in% c("MUFA_perc", "PUFA_perc", "SAFA_perc") &
                !(lakename %in% c("Lake Mendota", "Lake Monona"))), 
              aes(season, seasonal_avg)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = lakename, group = lakename), position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(x = season, ymin = seasonal_avg - sd, ymax = seasonal_avg + sd,
                    color = lakename, group = lakename),
                position = position_dodge(width = 0.2)) +
  facet_wrap(~FA_type) +
  ggtitle("excluding Madison lakes (aggregated to lake/season)")

mufa4 <- aov(seasonal_avg ~ season, dat = filter(dat_long_seasonal_points, FA_type == "MUFA_perc" & 
                                              !(lakename %in% c("Lake Mendota", "Lake Monona"))))
summary(mufa4) #p=0.959

pufa4 <- aov(seasonal_avg ~ season, dat = filter(dat_long_seasonal_points, FA_type == "PUFA_perc" &
                                              !(lakename %in% c("Lake Mendota", "Lake Monona"))))
summary(pufa4) #p=0.716

safa4 <- aov(seasonal_avg ~ season, dat = filter(dat_long_seasonal_points, FA_type == "SAFA_perc" &
                                              !(lakename %in% c("Lake Mendota", "Lake Monona"))))
summary(safa4) #p=0.346
#none signif


# ----> lakes with >3 years matching
ggplot(filter(dat_long_seasonal_points, 
              FA_type %in% c("MUFA_perc", "PUFA_perc", "SAFA_perc") &
              lakename %in% lake_years_3more$lakename), 
       aes(season, seasonal_avg)) +
  geom_point(aes(color = lakename, group = lakename), position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(x = season, ymin = seasonal_avg - sd, ymax = seasonal_avg + sd,
                    color = lakename, group = lakename),
                position = position_dodge(width = 0.2)) +
  facet_grid(lakename~FA_type) +
  ggtitle("Lakes withat least 3 years")
#not seeing really consistent trends


# ----> just the <3 years lakes
ggplot(filter(dat_long_seasonal_points, 
              FA_type %in% c("MUFA_perc", "PUFA_perc", "SAFA_perc") &
                !(lakename %in% lake_years_3more$lakename)), 
       aes(season, seasonal_avg)) +
  geom_point(aes(color = lakename, group = lakename), position = position_dodge(width = 0.2)) +
  facet_grid(lakename~FA_type, scales = "free") +
  ggtitle("Lakes with <3 years data")

#not seeing consistent trends

#===========================================================================
# ----> lakes aggregated to seasons across years (C omegas)
#===========================================================================

head(full_dat_weighted_comm_agg)
summary(dat_long_seasonal_points)

#per AG: look at ratio of omega 6 to omega 3 PUFAs
#per MM: look at long chain C PUFAs:SAFA ratio
#(AG defines long chain as 20+ C, so here 20 and 22 Cs)

#unique(dat_long_seasonal_points$FA_type)
# "c18.2w6"   "c18.3w3"   "c18.3w6"   "c18.4w3"   "c18.5w3"   "c20.4w6"   "c20.5w3"   "c22.6w3" 


# tag PUFAs as short or long chain, omega 3 or omega 6
# dat_long_seasonal_C <- dat_long_seasonal_points %>% 
#   #if c20 or c22, identify as long chain - default is NA
#   #if c18, short chain
#   mutate(c_chain = ifelse(grepl("c20*", FA_type)==TRUE | grepl("c22*", FA_type)==TRUE, 
#                              "long", NA),
#          c_chain = ifelse(grepl("c18*", FA_type)==TRUE, "short", c_chain)) %>% 
#   #identify whether omega 3 or omega 6
#   mutate(omega_num = ifelse(grepl("*w3", FA_type)==TRUE, "omega_three", NA),
#          omega_num = ifelse(grepl("*w6", FA_type)==TRUE, "omega_six", omega_num))

# dat_long_seasonal_C %>% select(FA_type, c_chain) %>% unique() %>% arrange(c_chain)
# dat_long_seasonal_C %>% select(FA_type, omega_num) %>% unique() %>% arrange(omega_num)

#recall: everything in seasonal_avg is a percentage
#so can sum up percentage of C omega types within PUFA
#(should double check that sum c omegas !> sum PUFA)

# dat_long_seasonal_C <- dat_long_seasonal_C %>% 
#   group_by(lakename, season, omega_num) %>% 
#   mutate(total_omega = sum(seasonal_avg, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   group_by(lakename, season, c_chain) %>% 
#   mutate(total_chain = sum(seasonal_avg, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   as.data.frame()


#find omega and long/short chain totals
dat_seasonal_C <- full_dat_weighted_comm_agg %>% 
  #total omega 3 prop of PUFAs
  mutate(total_omega3 = seasonal_avg__c18.3w3 + 
                        seasonal_avg__c18.4w3 + 
                        seasonal_avg__c18.5w3 + 
                        seasonal_avg__c20.5w3 + 
                        seasonal_avg__c22.6w3,
         #total omega 6 prop of PUFAs
         total_omega6 = seasonal_avg__c18.2w6 + 
                        seasonal_avg__c18.3w6 + 
                        seasonal_avg__c20.4w6,
         #total short chain PUFAs (<20 C)
         total_short_chain = seasonal_avg__c18.2w6 +
                       seasonal_avg__c18.3w6 +
                       seasonal_avg__c18.3w3 +
                       seasonal_avg__c18.4w3 +
                       seasonal_avg__c18.5w3,
         #total long chain PUFAs (AG defined as >= 20 C)
         total_long_chain = seasonal_avg__c20.4w6 +
                       seasonal_avg__c20.5w3 +
                       seasonal_avg__c22.6w3)

#find omega6:omega3 for PUFAs and long chain PUFAs:SAFA
dat_seasonal_C_ratios <- dat_seasonal_C %>% 
  mutate(omega6_omega3_ratio = total_omega6/total_omega3,
         #may want just long chain PUFA down the road (%)
         longchainPUFA = (total_long_chain * seasonal_avg__PUFA_perc)/100,
         #total long chain is % of PUFAs that are long chain
         #divide by PUFAs prop to get ratio with SAFA
         #(i.e., proportion of ALL FATTY ACIDS that are long chain PUFAs compared to SAFA prop)
         longchainPUFA_SAFA = longchainPUFA/ seasonal_avg__SAFA_perc)

  
# ----> omega6:omega3 of PUFAs
ggplot(dat_seasonal_C_ratios, aes(season, omega6_omega3_ratio)) +
  geom_boxplot() +
  geom_jitter(aes(color = lakename), width = 0.1) +
  ggtitle("All lakes, omega6:omega3 ratio for PUFAs")

mod1 <- aov(omega6_omega3_ratio ~ season, dat = dat_seasonal_C_ratios)
summary(mod1) #p=0.487

ggplot(dat_seasonal_C_ratios, aes(total_omega6, total_omega3)) +
  geom_point(aes(color = lakename)) +
  #geom_smooth() +
  facet_wrap(~season) +
  ggtitle("omega6 vs omega3 for all lakes")

  
# ----> long chain PUFA:SAFA
ggplot(dat_seasonal_C_ratios, aes(season, longchainPUFA_SAFA)) +
  geom_boxplot() +
  geom_jitter(aes(color = lakename), width = 0.1) +
  ggtitle("All lakes, long chain (>=20 C) PUFA : SAFA")

mod2 <- aov(longchainPUFA_SAFA ~ season, dat = dat_seasonal_C_ratios)
summary(mod2) #p=0.7
  
ggplot(dat_seasonal_C_ratios, 
       aes(longchainPUFA, seasonal_avg__SAFA_perc)) +
  geom_point(aes(color = lakename)) +
  #geom_smooth(method = "lm") +
  facet_wrap(~season) +
  ggtitle("long chain (>=20 C) PUFA vs SAFA")


#===========================================================================
# ----> C omegas log transformed
#=========================================================================== 

# ----> log(omega6:omega3) of PUFAs
ggplot(dat_seasonal_C_ratios, aes(season, log(omega6_omega3_ratio))) +
  geom_boxplot() +
  geom_jitter(aes(color = lakename), width = 0.1) +
  ggtitle("All lakes, log(omega6:omega3) ratio for PUFAs")

mod1 <- aov(log(omega6_omega3_ratio) ~ season, dat = dat_seasonal_C_ratios)
summary(mod1) #p=0.706
#not seeing seasonal difference in logged omega ratio

ggplot(dat_seasonal_C_ratios, aes(log(total_omega6), log(total_omega3))) +
  geom_point(aes(color = lakename)) +
  facet_wrap(~season) +
  ggtitle("logged omega6 vs omega3 for all lakes")
#when log omegas, do see positive linear trend
#which means...?

linmod2 <- lm(log(total_omega3) ~ log(total_omega6), dat = dat_seasonal_C_ratios)
summary(linmod2) #0.0164 * 
#signif positive linear trend - increase in (log) omega6, increase in (log) omega3

#ratio of logged
ggplot(dat_seasonal_C_ratios, aes(season, (log(total_omega6)/log(total_omega3)))) +
  geom_boxplot() +
  geom_jitter(aes(color = lakename), width = 0.1) +
  ggtitle("All lakes, ratio of logged omegas for PUFAs")

mod1 <- aov((log(total_omega6)/log(total_omega3)) ~ season, dat = dat_seasonal_C_ratios)
summary(mod1) #p=0.829
#ratio of logged more challenging to interpret, but non-signif


# ----> long chain PUFA:SAFA
ggplot(dat_seasonal_C_ratios, aes(season, log(longchainPUFA_SAFA))) +
  geom_boxplot() +
  geom_jitter(aes(color = lakename), width = 0.1) +
  ggtitle("All lakes, log(long chain (>=20 C) PUFA : SAFA)")

mod3 <- aov(log(longchainPUFA_SAFA) ~ season, dat = dat_seasonal_C_ratios)
summary(mod3) #p=0.894

ggplot(dat_seasonal_C_ratios, 
       aes(log(longchainPUFA), log(seasonal_avg__SAFA_perc))) +
  geom_point(aes(color = lakename)) +
  #geom_smooth(method = "lm") +
  facet_wrap(~season) +
  ggtitle("logged long chain (>=20 C) PUFA vs SAFA")

linmod3 <- lm(log(seasonal_avg__SAFA_perc) ~ log(longchainPUFA), dat = dat_seasonal_C_ratios)
summary(linmod3) #0.821

