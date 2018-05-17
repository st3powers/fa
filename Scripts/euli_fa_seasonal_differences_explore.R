#=========================================================#
## ===================== OVERVIEW ====================== ##
#=========================================================#

#see "euli_fa_alternate_aggregate.R" for processing up to this point
#reads in full_dat_weighted_comm from that script

# for each lake, summer - winter as response variable
# is this signif diff from zero?
# do for FA props (so...differncing props...)
# do confidence intervals overlap with zero?
# 
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

dat <- read.csv("../Data/EULI_lake_year_season_community_FAs.csv", stringsAsFactors = FALSE)

head(dat)

#=========================================================#
## ================ SEASONAL DIFFERENCES ================ ##
#=========================================================#

# ----> find seasonal differences

#reorganize so season across top
dat_seasonal <- dat %>% 
  #make long
  melt(id.vars = c("lakename", "year", "season")) %>% 
  rename(FA_type = variable) %>% 
  #make wide
  dcast(lakename + year + FA_type ~ season, value.var = "value")

#find differences (summer-winter)
dat_seasonal_diffs <- dat_seasonal %>% 
  mutate(summer_winter_diff = iceoff - iceon) 

# ----> one sample t-test - are diffs diff from zero?

#is sample mean (mean of all diffs) different from zero?
#not sure that's exactly what we want...
#but try with all lakes first, then by lake (for lakes with >1 year)

#quick check for normality
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

# t = -1.9038, df = 626, p-value = 0.05739
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.65465899  0.01014283
# sample estimates:
#   mean of x 
# -0.3222581 

#confidence interval includes zero

#that's for all lakes, all FAs

# ----> PUFA only

dat_pufa <- dat_seasonal_diffs %>% 
  filter(FA_type == "MUFA_perc")

hist(dat_pufa$summer_winter_diff)
qqnorm(dat_pufa$summer_winter_diff)
#not...ideal

t.test(dat_pufa$summer_winter_diff, mu=0, alternative = "two.sided")
# data:  dat_pufa$summer_winter_diff
# t = -1.2739, df = 56, p-value = 0.208
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -3.4656855  0.7713384
# sample estimates:
#   mean of x 
# -1.347174 

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
# t = 5.5855, df = 56, p-value = 7.108e-07
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   2.363166 5.006220
# sample estimates:
#   mean of x 
# 3.684693 

#not even closer to zero
