#set working directory

#load packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)

#=============================================================================
# ----> read in secchi data

secchi_orig <- read.csv("Data/ntl31_v4.csv", stringsAsFactors = FALSE)

# WARNING- to be resolved, SP's mac had a strange date import issue

#format dates
secchi <- secchi_orig %>% 
  mutate(date = as.Date(sampledate, format = "%m/%d/%Y")) %>% 
  select(-sampledate) %>% 
  rename(year = year4)


#=============================================================================
# ----> read in under ice data, for ice on/off timing

seasons_orig <- read.csv("Data/under_ice_data.csv", stringsAsFactors = FALSE)

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

#make lakeid to full lake name
seasons_lakes <- seasons_wisc %>% 
  mutate(lakeid = ifelse(lakename == "Allequash Lake", "AL", NA),
         lakeid = ifelse(lakename == "Big Muskellunge Lake", "BM", lakeid),
         lakeid = ifelse(lakename == "Crystal Bog", "CB", lakeid),
         lakeid = ifelse(lakename == "Crystal Lake", "CR", lakeid),
         lakeid = ifelse(lakename == "Fish Lake", "FI", lakeid),
         lakeid = ifelse(lakename == "Lake Mendota", "ME", lakeid),
         lakeid = ifelse(lakename == "Lake Monona", "MO", lakeid),
         lakeid = ifelse(lakename == "Lake Wingra", "WI", lakeid),
         lakeid = ifelse(lakename == "Sparkling Lake", "SP", lakeid),
         lakeid = ifelse(lakename == "Trout Bog", "TB", lakeid),
         lakeid = ifelse(lakename == "Trout Lake", "TR", lakeid)) %>% 
  select(season, lakeid, year, startdate, enddate)

#=============================================================================
# ----> tag secchi data with season

secchi_dates_only <- secchi %>% 
  select(lakeid, year, date) %>% 
  unique() %>% 
  rename(sample_year = year) %>% 
  arrange(lakeid, sample_year)

seasons_lakes_wide <- seasons_lakes %>% 
  melt(id.vars = c("lakeid", "year", "season"), variable.name = "category", value.name = "date") %>% 
  mutate(seasons_category = paste(season, category, sep = "_")) %>% 
  select(-season, -category) %>% 
  dcast(lakeid + year ~ seasons_category, value.var = "date") %>% 
  rename(winter_yr = year) %>% 
  select(lakeid, winter_yr, iceon_startdate, iceon_enddate, iceoff_startdate, iceoff_enddate)

#we're only interested in iceon/iceoff
secchi_seasons_pre <- merge(secchi_dates_only, seasons_lakes_wide,
                            by = c("lakeid"), all.x = TRUE) %>% 
  #tag seasons
  mutate(season = ifelse(date >= iceon_startdate & date <= iceon_enddate, "iceon", NA),
         season = ifelse(date >= iceoff_startdate & date <= iceoff_enddate, "iceoff", season)) %>% 
  #keep only samples that fall within iceon or iceoff
  filter(!is.na(season))

#merge back to full secchi data
secchi_seasons <- secchi_seasons_pre %>% 
  #group things by *winter* year
  select(lakeid, date, winter_yr, season) %>% 
  merge(secchi, by = c("lakeid", "date")) %>% 
  select(-year)

#sanity check
# secchi_seasons %>% 
#   select(season, ice) %>% 
#   unique()

# filter(secchi_seasons, season == "iceon" & is.na(ice)) %>% filter(lakeid == "ME")
# filter(seasons_lakes_wide, lakeid == "ME" & winter_yr == 2001) #yep, is iceon by our dates
# filter(seasons_lakes_wide, lakeid == "ME" & winter_yr == 2013) #yep, same, ice
#yes, there are some legitimate occasions when season is winter but ice is NA

#=============================================================================
# ----> check out secchi data

secchi_small <- select(secchi_seasons, lakeid, date, winter_yr, season, secview, secnview)

#secview is with viewer
#secnview is without viewer

ggplot(secchi_small, aes(season, secview)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~lakeid)

secchi_small %>% 
  filter(lakeid %in% c("MO", "ME"))

ggplot(secchi_small, aes(season, secnview)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~lakeid)

#most data points for secnview
#or take average of secview, secnview


#=============================================================================
# ----> read in snow/ice

icesnow_orig<-read.csv("Data/ntl34_v5.csv", stringsAsFactors = FALSE)
icesnow<-icesnow_orig %>% select(lakeid,year=year4,sampledate,sta,avsnow,sdsnow,totice,whiteice,blueice)


#=============================================================================
# ----> read in chl

chl_north_orig<-read.csv("Data/ntl35_v2.csv", stringsAsFactors = FALSE)
chl_south_orig<-read.csv("Data/ntl38_v3.csv", stringsAsFactors = FALSE)
chlr_north<-chl_north_orig %>% select(lakeid,year=year4,sampledate,depth,chl=chlor,phaeo)
chlr_south<-chl_south_orig %>% select(lakeid,year=year4,sampledate,depth=depth_range_m,chl=tri_chl_spec,phaeo=phaeo_spec)
chlr<-rbind(chlr_north,chlr_south)


#=============================================================================
# combine chl and snow/ice datasets

merge<-merge(icesnow,chlr, by=c("lakeid","sampledate")) #keep only dates that are in both
data<-merge

#make date a date
data$sampledate <- as.Date(data$sampledate, format = "%m/%d/%Y")

data$depthgroup<-data$depth

# play with depth groups?
data$depthgroup[which(data$depth <=2)]<-"0-2"
data$depthgroup[which(data$depth >2 & data$depth<=5)]<-"2-5"
data$depthgroup[which(data$depth >5 & data$depth<=10)]<-"5-10"
data$depthgroup[which(data$depth >10 & data$depth<=15)]<-"10-15"
data$depthgroup[which(data$depth >15 & data$depth<=20)]<-"15-20"
data$depthgroup[which(data$depth >20)]<-"20-99"
data$depthgroup[which(data$depth =="0-1")]<-"0-2"
data$depthgroup[which(data$depth =="0-8")]<-"0-8"

merge.test<-merge(icesnow,chlr, by=c("lakeid","sampledate"),all=TRUE)

#fix dates
merge.test$sampledate <- as.Date(merge.test$sampledate, format = "%m/%d/%Y")

# check to see how many jan/feb chla values lack snow/ice data
janfeb<-subset(merge.test,month(merge.test$sampledate) %in% c(1,2))
janfeb.missingsnowice<-janfeb[which((is.na(janfeb$avsnow)+is.na(janfeb$totice))==2),]
100*length(janfeb.missingsnowice[,1])/length(merge[,1])
# 0.6084396,  so ice or snow data measured for 99.4% of jan/feb chl samples

data.aggdepths<-data %>% group_by(lakeid,sampledate,avsnow,sdsnow,totice,whiteice,blueice) %>%
  dplyr::summarize(chl=mean(chl,na.rm=TRUE)) %>% as.data.frame()

data.aggdepthgroups<-data %>% group_by(lakeid,depthgroup,sampledate,avsnow,sdsnow,totice,whiteice,blueice) %>%
  dplyr::summarize(chl=mean(chl,na.rm=TRUE)) %>% as.data.frame()


# plot chl~snow, all depths
dataplot<-data
ggplot(dataplot,aes(x=avsnow,y=chl,color=sdsnow)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plot chl~snow, depth groups
dataplot<-data.aggdepthgroups
ggplot(dataplot,aes(x=avsnow,y=chl,color=sdsnow, shape=depthgroup)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plat chl~snow, aggregate depths
dataplot<-data.aggdepths
ggplot(dataplot,aes(x=avsnow,y=chl,color=sdsnow)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

