# ============================================================== #
## ========================= OVERVIEW ========================= ##
# ============================================================== #

# Looking at iceon/iceoff community FA profiles
# Only using the Ecology Under Lake Ice dataset.
# Using AG PLoS paper FA dataset, with some aggregation/crosswalking
# to match EULI data.

#libraries
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(boot)
library(purrr)

# read in ecology under lake ice dataset and FA dataset -------------------

euli_orig <- read.csv("../Data/under_ice_data.csv", stringsAsFactors = FALSE)

fa_orig <- read.csv("../Data/PLoS_supp/S1_Dataset.csv", stringsAsFactors = FALSE)

# wrangle ice phyto data --------------------------------------------------

euli <- euli_orig %>%
  # keep only columns of potential interest
  select(year, season, researcher, lakename, stationname, poolstation,
         lakeregloc, lakecountry, startday, startmonth, startyear, endday,
         endmonth, endyear, iceduration, fadata, avesecchidepth, avechla,
         phytomethod, avephytomass, avephytocount, propchloro, propcrypto,
         propcyano, propdiatom, propdiatom, propotherphyto) %>%
  # only interested when *not* all NA for phyto props
  filter(!(is.na(propchloro) & is.na(propcrypto) & is.na(propcyano) &
           is.na(propdiatom) & is.na(propdiatom) & is.na(propotherphyto)))

# some lakes have multiple stations within lake
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

# 1:1 match lakename:poolstation
#filter(euli, lakename %in% c("Blackstrap Reservoir", "Lake Diefenbaker")) %>% 
#  select(lakename, stationname, poolstation)

# aggregate to lake/poolstation
euli_lakes_phytos <- euli %>%
  # start by looking just at pytos, can circle back to incorporate snow/ice/secchi etc.
  select(year, season, lakename, poolstation,
         propchloro, propcrypto, propcyano, propdiatom, propotherphyto) %>%
  # group and aggregate
  group_by(lakename, poolstation, year, season) %>%
  summarize(prop_chloro = mean(propchloro, na.rm = TRUE),
            prop_crypto = mean(propcrypto, na.rm = TRUE),
            prop_cyano = mean(propcyano, na.rm = TRUE),
            prop_diatom = mean(propdiatom, na.rm = TRUE),
            prop_other = mean(propotherphyto, na.rm = TRUE)) %>%
  ungroup() %>%
  as.data.frame()
  
summary(euli_lakes_phytos) # 40 NA in crypto
str(euli_lakes_phytos) # 205

# going to start by using only lakes with *NO NAs* for phytos
euli_phytos_complete <- euli_lakes_phytos %>%
  filter(!(is.na(prop_chloro) | is.na(prop_crypto) | is.na(prop_cyano) |
             is.na(prop_diatom) | is.na(prop_other)))
# 158 obvs - only "complete" phyto profiles

# 24 lakes in total
unique(euli_phytos_complete$lakename)

# what's the temporal coverage?
# only want ones with matching iceon/iceoff
euli_phytos_complete %>%
  group_by(lakename) %>%
  summarize(seasons = n_distinct(season)) %>%
  as.data.frame() %>%
  arrange(seasons)

# identity lakes with both seasons per year
multi_seasons <- euli_phytos_complete %>%
  group_by(lakename, year) %>%
  summarize(seasons = n_distinct(season)) %>%
  as.data.frame() %>%
  arrange(seasons) %>%
  filter(seasons > 1) %>%
  mutate(lakeyear = paste(lakename, year, sep = "_"))

# keep only those lake/year/seasons
euli_phytos_complete_matching <- euli_phytos_complete %>%
  mutate(lakeyear = paste(lakename, year, sep = "_")) %>%
  filter(lakeyear %in% multi_seasons$lakeyear)

# make sure 1:1 match (i.e., iceon/iceoff in single year
# for lake - no missing pairs singles)

# euli_phytos_complete_matching %>%
#   group_by(lakename, year) %>%
#   summarize(n = n_distinct(season)) %>%
#   filter(n != 2)
# correct

# filter(euli_phytos_complete, lakename == "Saanajarvi")
# filter(euli, lakename == "Saanajarvi")
# yep...being true to the data, we ignore Saanajarvi
# I think it's actually a goof in the euli data,
# but moving forward without it...

# check out remaining
str(euli_phytos_complete_matching)
str(unique(euli_phytos_complete_matching$lakename))
# 15 lakes

# temporal coverage
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

# may be interested in lakes with >= 3 years down the road
lake_years_3more <- filter(lake_years, yrs >= 3)

# make long to be equivalent to FA data
phytos_long <- euli_phytos_complete_matching %>%
  select(-poolstation, -lakeyear) %>%
  melt(id.vars = c("lakename", "year", "season")) %>%
  rename(phyto_group = variable, prop = value)

# remove prop from group name
phytos_long$phyto_group <- gsub("prop_", "", phytos_long$phyto_group)

unique(phytos_long$phyto_group)
# "chloro"     "crypto"     "cyano"      "diatom"     "other"

# remove "other" phyto and find new total, then recalculate proportions
phyto_long_no_other <- phytos_long %>%
  # remove "other" phyto group
  filter(phyto_group != "other") %>%
  # for each lake/year/season (i.e. time point)
  group_by(lakename, year, season) %>%
  # find new total proportion
  mutate(new_total = sum(prop),
         # find new proportion for phyto groups
         new_prop = prop / new_total) %>%
  as.data.frame()

# summary(phyto_long_no_other)
# filter(phyto_long_no_other, new_total < 0.1)
# filter(phytos_long, phyto_group == "other" & prop > 0.5) %>% arrange(prop)

phyto_long_no_other <- phyto_long_no_other %>%
  select(-prop, -new_total) %>%
  rename(prop = new_prop)


# wrangle FA data ---------------------------------------------------------

fa <- fa_orig %>%
  # only fresh water
  filter(Salinity == 0) %>%
  # only proportional data
  filter(Data == "prop") %>%
  # for now, don't care about experimental info
  select(Group, Class, Order, Genus, species,
         sumSAFA, sumMUFA, sumPUFA,
         c18.2w6, c18.3w6, c18.3w3,
         c18.4w3, c18.5w3,
         c20.4w6, c20.5w3, c22.6w3)

unique(fa$Group)
# "Cyanobacteria" "Chlorophyta"   "Ochrophyta"    "Cryptophyta"   "Dinophyta"
# "Euglenozoa"    "Rhodophyta"  

# crosswalk to match euli to FA
fa_equiv <- fa %>%
  # crypto, chloro, cyano are direct matches
  mutate(fa_group = ifelse(Group == "Cryptophyta", "crypto", NA),
         fa_group = ifelse(Group == "Chlorophyta", "chloro", fa_group),
         fa_group = ifelse(Group == "Cyanobacteria", "cyano", fa_group),
         #diatoms are everything in these 3 classes in FA data
         fa_group = ifelse(Class %in% c("Bacillariophyceae", "Fragilariophyceae", "Coscinodiscophyceae"),
                           "diatom", fa_group))

# fa_equiv %>%
#   select(Group, fa_group) %>%
#   unique()
#
# fa_equiv %>%
#   select(Group, Class, fa_group) %>%
#   unique()
#
# fa_equiv %>% group_by(fa_group) %>%
#   summarize(n = n_distinct(Genus)) %>% as.data.frame()
#   fa_group  n
# 1   chloro 13
# 2   crypto  2
# 3    cyano 22
# 4   diatom  8
# 5     <NA> 11


# aggregate FA data -------------------------------------------------------

# aggregate to level of fa_group (average)

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

# fa_group_avgs

# combine EULI and FA datasets --------------------------------------------


head(phyto_long_no_other) # lake, year, season, group, prop
head(fa_group_avgs) # group, FAs

# combine euli and FA - have group, proportion, and FA data
full_dat <- merge(phyto_long_no_other, fa_group_avgs,
                  by.x = "phyto_group", by.y = "fa_group",
                  all.x = TRUE)

# weight data accordingly
full_dat_weighted <- full_dat %>%
  # prop phyto (already biomass/total biomass) * prop FA
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
  # keep only columns of interest
  # for now, only SAFA, MUFA, PUFA
  select(lakename, year, season, phyto_group,
         sumSAFA_prop, sumMUFA_prop, sumPUFA_prop,
         prop_c18.2w6, prop_c18.3w6,  prop_c18.3w3,
         prop_c18.4w3, prop_c18.5w3,
         prop_c20.4w6, prop_c20.5w3,
         prop_c22.6w3)

# sum within lake/time point (get community-level FA)
full_dat_weighted_comm <- full_dat_weighted %>%
  group_by(lakename, year, season) %>%
  # sum across phyto groups
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

head(full_dat_weighted_comm)

# write to csv for use in other scripts
write.csv(full_dat_weighted_comm, "../Data/EULI_lake_year_season_community_FAs.csv",
          row.names = FALSE)

# make a verion with one point per lake/season (aggregate across years)
# include standard deviation as measure of variance, as well as count
full_dat_weighted_comm_agg <- full_dat_weighted_comm %>%
  group_by(lakename, season) %>%
  # using __ so can separate later
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



write.csv(full_dat_weighted_comm_agg, "../Data/EULI_lake_seasonal_community_FAs.csv", row.names = FALSE)


full_dat_weighted_comm_ag_long <- melt(data = full_dat_weighted_comm_agg,
     # same as section below but selecting fewer vars
     measure.vars = c("seasonal_avg__MUFA_perc",
                      "seasonal_avg__PUFA_perc",
                      "seasonal_avg__SAFA_perc"),
     id.vars = c("lakename", "season", "n_years"))

full_dat_weighted_comm_ag_long$variable <- as.character(full_dat_weighted_comm_ag_long$variable)
full_dat_weighted_comm_ag_long$variable[which(full_dat_weighted_comm_ag_long$variable == "seasonal_avg__MUFA_perc")] <- "% MUFA"
full_dat_weighted_comm_ag_long$variable[which(full_dat_weighted_comm_ag_long$variable == "seasonal_avg__PUFA_perc")] <- "% PUFA"
full_dat_weighted_comm_ag_long$variable[which(full_dat_weighted_comm_ag_long$variable == "seasonal_avg__SAFA_perc")] <- "% SAFA"

mean.function <- function(x, index) {
  d <- x[index]     # This first line will go in ever bootstrap function you make.
  return(mean(d))  
}

euli_onelake_oneseason_onevalue <- 
  ggplot(data = full_dat_weighted_comm_ag_long %>%
           group_by(season, variable) %>%
           nest() %>%
           mutate(bootstrap = map(.x = data, 
                                  .f = ~ boot(data = .x$value, 
                                              statistic = mean.function,
                                              sim = "ordinary",
                                              R = 499)),
                  boot_mean = map(.x = bootstrap, 
                                  .f = ~ mean(.x$t, na.rm = TRUE)),
                  first_quantile = map(.x = bootstrap, 
                                       .f = ~ quantile(.x$t, c(0.025))),
                  last_quantile = map(.x = bootstrap, 
                                      .f = ~ quantile(.x$t, c(0.975)))) %>%
           select(-data, -bootstrap) %>%
           unnest(cols = c(boot_mean, first_quantile, last_quantile)) %>%
           mutate(season = case_when(season == "iceoff" ~ "ice-free",
                                     season == "iceon" ~ "ice-covered"))) +
  geom_point(aes(x = season, y = boot_mean), size = 6) +
  geom_errorbar(aes(x = season,
                    ymin = first_quantile,
                    ymax = last_quantile),
                width = 0, lwd = 1) +
  facet_wrap(~variable) +
  xlab("Season") + ylab("% of Total FA")  +
  theme_bw() +
  theme(axis.title.y = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        strip.text.x = element_text(size = rel(1.5)))

full_dat_agg_omegas <- full_dat_weighted_comm_agg %>%
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
           seasonal_avg__c22.6w3) %>%
  mutate(omega6_omega3_ratio = total_omega6 / total_omega3,
         #may want just long chain PUFA down the road (%)
         longchainPUFA = (total_long_chain * seasonal_avg__PUFA_perc) / 100,
         #total long chain is % of PUFAs that are long chain
         #divide by PUFAs prop to get ratio with SAFA
         #(i.e., proportion of ALL FATTY ACIDS that are long chain PUFAs compared to SAFA prop)
         longchainPUFA_SAFA = longchainPUFA / seasonal_avg__SAFA_perc,
         omega3_omega6_ratio = total_omega3 / total_omega6) 


euli_omega_ratio_plot_3.6 <- ggplot(full_dat_agg_omegas %>%
           group_by(season) %>%
           nest() %>%
           mutate(bootstrap = map(.x = data, 
                                  .f = ~ boot(data = .x$omega3_omega6_ratio, 
                                              statistic = mean.function,
                                              sim = "ordinary",
                                              R = 499)),
                  boot_mean = map(.x = bootstrap, 
                                  .f = ~ mean(.x$t, na.rm = TRUE)),
                  first_quantile = map(.x = bootstrap, 
                                       .f = ~ quantile(.x$t, c(0.025))),
                  last_quantile = map(.x = bootstrap, 
                                      .f = ~ quantile(.x$t, c(0.975)))) %>%
           select(-data, -bootstrap) %>%
           unnest(cols = c(boot_mean, first_quantile, last_quantile)) %>%
           mutate(season = case_when(season == "iceoff" ~ "ice-free",
                                     season == "iceon" ~ "ice-covered"))) +
  geom_point(aes(x = season, y = boot_mean), size = 6) +
  geom_errorbar(aes(x = season,
                    ymin = first_quantile,
                    ymax = last_quantile),
                width = 0, lwd = 1) +
  ylab("\U03C9-3:\U03C9-6") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))


euli_plots <- ggarrange(euli_onelake_oneseason_onevalue, euli_omega_ratio_plot_3.6,
                        ncol = 2, nrow = 1, common.legend = TRUE, legend = "right",
                        widths = c(2,1))

ggsave(filename = "../Figures/boostrapped_fas.png", plot = euli_plots)

