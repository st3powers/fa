#temperature profiles by LTER lake for summers 2014-2016
#summer dates not in EULI dataset - need to determine if want to use additional years

#load libraries
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(zoo)
library(lubridate)

#NTL LTER data - depth, temp, o2, light
dat_orig <- read.csv("../Data/ntl29_v5.csv", stringsAsFactors = FALSE)

#only interested in temp and depth
dat <- dat_orig %>% select(lakeid, year4, sampledate, depth, wtemp) %>% unique()

#missing years only (have dates up to 2013 in EULI dataset)
dat_recent <- dat %>% filter(year4 >= 2014)

#check
dat_recent %>%
  select(lakeid, year4) %>%
  unique()

dat_recent <- dat_recent %>%
  mutate(year = year(sampledate))

#plot for each lake and year - all into one PDF
pdf("../Figures/lake_temperature_profiles.pdf")

#loops through lakes/years - temperature profile for each sample date
for (i in unique(dat_recent$lakeid)) {
  #only that lake data
  dat_i <- filter(dat_recent, lakeid == i)

  for (j in unique(dat_i$year)) {

    dat_j <- filter(dat_i, year == j)

    #make plot
    plot_j <- ggplot(dat_j, aes(wtemp, depth)) +
      geom_path() +
      scale_y_reverse() +
      facet_wrap(~sampledate) +
      ggtitle(paste(i, j, sep = " "))

    #print out
    print(plot_j)
  } #close j loop
} #close i loop
dev.off()

#JUST MADISON LAKES
dat_mad <- filter(dat_recent, lakeid %in% c("ME", "MO"))

pdf("Figures/madison_lakes_temperature_profiles.pdf")

#loops through lakes/years - temperature profile for each sample date
for (i in unique(dat_mad$lakeid)) {
  #only that lake data
  dat_i <- filter(dat_mad, lakeid == i)

  for (j in unique(dat_i$year)) {

    dat_j <- filter(dat_i, year == j)

    #make plot
    plot_j <- ggplot(dat_j, aes(wtemp, depth)) +
      geom_path() +
      scale_y_reverse() +
      facet_wrap(~sampledate) +
      ggtitle(paste(i, j, sep = " "))

    #print out
    print(plot_j)
  } #close j loop
} #close i loop
dev.off()

