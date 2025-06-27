#Take the cleaned data from the 'scatter_plots.R' script and 
#run the linear regression models on them.


# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(olsrr)


# 2. Import data ----------------------------------------------------------

light_snow <- read_csv(here('2_data/light_snow.csv')) %>% 
  mutate(
    lakeid = as.factor(lakeid),
    lakename = as.factor(lakename)
  )

ntl_iq <- read_csv(here('2_data/ntl_iq_clean.csv')) %>% 
  mutate(
    lakeid = as.factor(lakeid)
  )

chl_a_iceon_log <- read_csv(here('2_data/chl_a_iceon_log.csv')) %>% 
  mutate(
    lakeid = as.factor(lakeid),
    lakename - as.factor(lakename)
  )

chl_iq <- read_csv(here('2_data/chl_iq.csv')) %>% 
  mutate(
    lakeid = as.factor(lakeid),
    lakename = as.factor(lakename)
  )

# 3. Linear regressions ---------------------------------------------------


# **3a. PAR ~ Snow --------------------------------------------------------

light_snow_tbl <- light_snow %>% 
  group_by(lakename) %>% 
  summarise(
    slope = summary(lm(log(1+light) ~ log(1+avsnow)))$coefficients[,1][2],
    intercept = summary(lm(log(1+light) ~ log(1+avsnow)))$coefficients[,1][1],
    p.value = summary(lm(log(1+light) ~ log(1+avsnow)))$coefficients[,4][2],
    adj.r2 = summary(lm(log(1+light) ~ log(1+avsnow)))$adj.r.squared,
    adj.r2_test = summary(lm(log(1+light) ~ log(1+avsnow)))$r.squared,
    n = length(light)
  )


# **3b. Chl-a ~ PAR -------------------------------------------------------

chl_light_tbl_test <- chl_a_iceon_log %>% 
  group_by(lakename) %>% 
  summarise(
    slope = summary(lm(log(1+chlor) ~ log(1+light)))$coefficients[,1][2],
    intercept = summary(lm(log(1+chlor) ~ log(1+light)))$coefficients[,1][1],
    p.value = summary(lm(log(1+chlor) ~ log(1+light)))$coefficients[,4][2],
    r2 = summary(lm(log(1+chlor) ~ log(1+light)))$r.squared,
    adj.r2 = summary(lm(log(1+chlor) ~ log(1+light)))$adj.r.squared,
    sw_test = shapiro.test(log(1+chlor))[2],
    n = length(chl_log)
  )


# 4. MLR ------------------------------------------------------------------


# **4a. Combine PAR and ice quality ---------------------------------------

ntl_short <- ntl_iq_clean %>% 
  select(
    lakeid,
    sampledate,
    wi_perc
  )

light_snow_iq <- light_snow %>% 
  inner_join(ntl_short)


# **4b. PAR ~ snow + ice_qual ---------------------------------------------

par_lm <- lm(data = light_snow_iq, log(1+light) ~ log(1+avsnow) + log(1+wi_perc))

summary(par_lm)

light_snw_iq_tbl2 <- light_snow_iq %>% 
  group_by(lakename) %>% 
  summarise(
    p.value = summary(lm(log(1+light) ~ log(1+avsnow) + log(1+wi_perc)))$coefficients[,4][2],
    adj.r2 = summary(lm(log(1+light) ~ log(1+avsnow) +log(1+wi_perc)))$adj.r.squared,
    n = length(light)
  )
light_snw_iq_tbl2

# **4c. Chl-a ~ PAR + snow ------------------------------------------------

chl_lm1 <- lm(data = chl_iq, log(1+chlor) ~ log(1+light) + log(1+avsnow))

summary(chl_lm1)

chl_light_snow_tbl <- chl_iq %>% 
  group_by(lakename) %>% 
  summarise(
    p.value = summary(lm(log(1+chlor) ~ log(1+light) + log(1+avsnow)))$coefficients[,4][2],
    adj.r2 = summary(lm(log(1+chlor) ~ log(1+light) + log(1+avsnow)))$adj.r.squared,
    n = length(chlor)
  )
chl_light_snow_tbl


# **4d. Chla ~ PAR + ice quality ------------------------------------------


chl_lm2 <- lm(data = chl_iq, log(1+chlor) ~ log(1+light) + log(1+wi_perc))

summary(chl_lm2)

chl_light_snow_tbl2 <- chl_iq %>% 
  group_by(lakename) %>% 
  summarise(
    p.value = summary(lm(log(1+chlor) ~ log(1+light) + log(1+wi_perc)))$coefficients[,4][2],
    adj.r2 = summary(lm(log(1+chlor) ~ log(1+light) + log(1+wi_perc)))$adj.r.squared,
    n = length(chlor)
  )
chl_light_snow_tbl2


# **4e. Chla ~ PAR + snow + ice quality -----------------------------------


chl_lm3 <- lm(data = chl_iq, log(1+chlor) ~ log(1+light) + log(1+avsnow) + log(1+wi_perc))

summary(chl_lm3)

chl_light_snow_tbl3 <- chl_iq %>% 
  group_by(lakename) %>% 
  summarise(
    p.value = summary(lm(log(1+chlor) ~ log(1+light) + log(1+avsnow) + log(1+wi_perc)))$coefficients[,4][2],
    adj.r2 = summary(lm(log(1+chlor) ~ log(1+light) + log(1+avsnow) + log(1+wi_perc)))$adj.r.squared,
    n = length(chlor)
  )
chl_light_snow_tbl3
