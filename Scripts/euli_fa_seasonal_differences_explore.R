#=========================================================#
## ===================== OVERVIEW ====================== ##
#=========================================================#

#see "euli_fa_alternate_aggregate.R" for processing up to this point
#reads in full_dat_weighted_comm_agg from that script
#aggregated to lake/season (means, collapsed over years)

# 1)
# for each lake, summer - winter as response variable
# is this signif diff from zero?
# do for FA props (so...differencing proportions...)
# do confidence intervals overlap with zero?
# 
# 2)
# euli lakes - look at trophic state
# difference data as response
# does, say TP, affect PUFAs/SAFAs?
# 
# basically, more on euli lakes (difference, trophic, maybe secchi)

#=========================================================#
## ================= GETTING STARTED =================== ##
#=========================================================#

#libraries
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)

# ----> read in data

dat <- read.csv("../Data/EULI_lake_seasonal_community_FAs.csv", stringsAsFactors = FALSE)

head(dat)

#=========================================================#
## ================ SEASONAL DIFFERENCES =============== ##
#=========================================================#

# ----> find seasonal differences

#reorganize so season across top
dat_seasonal <- dat %>% 
  #make long
  melt(id.vars = c("lakename", "n_years", "season")) %>% 
  #split variable into agg and type
  separate(variable, into =c("agg", "FA_type"), sep = "__") %>% 
  #keep only averages
  filter(agg == "seasonal_avg") %>% 
  #remove that column
  select(-agg) %>% 
  #make wide
  dcast(lakename + n_years + FA_type ~ season, value.var = "value")

#find differences (summer-winter)
dat_seasonal_diffs <- dat_seasonal %>% 
  mutate(summer_winter_diff = iceoff - iceon) 

# ----> one sample t-test - are diffs diff from zero?

#is sample mean (mean of all diffs) different from zero?
#not sure that's exactly what we want...

#one value per lake/season/FA_type

#quick check for normality (across ALL fa types)
hist(dat_seasonal_diffs$summer_winter_diff)
#not bad

#null of normality
shapiro.test(dat_seasonal_diffs$summer_winter_diff)
#hmmm, reject null

#plot
qqnorm(dat_seasonal_diffs$summer_winter_diff)
#yeah...not really normal

#well, testing anyways...

#one sample t-test
t.test(dat_seasonal_diffs$summer_winter_diff, mu=0, alternative = "two.sided")

# data:  dat_seasonal_diffs$summer_winter_diff
# t = -0.71741, df = 164, p-value = 0.4741
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.7761753  0.3624698
# sample estimates:
#   mean of x 
# -0.2068528 

#confidence interval includes zero


# ----> PUFA only

dat_pufa <- dat_seasonal_diffs %>% 
  filter(FA_type == "MUFA_perc")

hist(dat_pufa$summer_winter_diff)
#not really...
qqnorm(dat_pufa$summer_winter_diff)
#meh

t.test(dat_pufa$summer_winter_diff, mu=0, alternative = "two.sided")
# data:  dat_pufa$summer_winter_diff
# t = 0.3223, df = 14, p-value = 0.752
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -3.466713  4.692852
# sample estimates:
#   mean of x 
# 0.6130696 

#CI crosses zero

# ----> SAFA only

dat_safa <- dat_seasonal_diffs %>% 
  filter(FA_type == "SAFA_perc")

hist(dat_safa$summer_winter_diff)
#skewed
qqnorm(dat_safa$summer_winter_diff)
#yeah, not normal

#but testing just because

t.test(dat_safa$summer_winter_diff, mu=0, alternative = "two.sided")
# data:  dat_safa$summer_winter_diff
# t = 1.9383, df = 14, p-value = 0.07302
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.1669648  3.3014506
# sample estimates:
#   mean of x 
# 1.567243 

#again, crosses zero

#=========================================================#
## =================== TROPHIC STATE =================== ##
#=========================================================#

euli_full <- read.csv("../Data/under_ice_data.csv", stringsAsFactors = FALSE)

#smaller subset
euli_small <- euli_full %>% 
  select(lakename, year, season, watertemp,
         avetotphos, avetotdissphos, 
         avetotnitro, avetotdissnitro,
         avetotdoc, avecolor, avechla)

#merge, keep only lakes of interest
euli_fa <- merge(dat, euli_small, by = c("lakename", "year", "season"), all.x = TRUE)

head(euli_fa)
summary(euli_fa)
#various missing - work with what have









