#open file from within Scripts directory - downstream file paths rely on this

#load packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)

#=============================================================================
# ----> read in secchi data

secchi_orig <- read.csv("../Data/ntl31_v4.csv", stringsAsFactors = FALSE)

# WARNING- to be resolved, SP's mac had a strange date import issue

#format dates
secchi <- secchi_orig %>% 
  mutate(date = as.Date(sampledate, format = "%m/%d/%Y")) %>% 
  select(-sampledate) %>% 
  rename(year = year4)


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
# ----> secchi vs FA props

#written out from other script (LTER_FA.R script)
madison_fa <- read.csv("../Data/LTER_Madison_weighted_year_season_FA.csv", stringsAsFactors = FALSE)

head(secchi_small)
head(madison_fa)

#aggregate secchi to year/season
#get max secnview and average
secchi_agg <- secchi_small %>% 
  group_by(lakeid, winter_yr, season) %>% 
  summarize(max_secnview = max(secnview, na.rm = TRUE),
            mean_secnview = mean(secnview, na.rm = TRUE)) %>% 
  as.data.frame()

#merge lake/year/season secchi and FA dataframes
fa_secchi <- merge(secchi_agg, madison_fa, 
                   #keep only rows in both
                   by = c("lakeid", "winter_yr", "season"), all = FALSE)

#write.csv(fa_secchi, "../Data/fa_secchi_data.csv", row.names = FALSE)

#recreate SH plot
ggplot(fa_secchi, aes(max_secnview, (MUFA_perc_avg + PUFA_perc_avg))) +
  geom_point(aes(color = lakeid)) +
  facet_wrap(~season) +
  ggtitle("MUFA/PUFA vs max secchi")

ggplot(fa_secchi, aes(max_secnview, SAFA_perc_avg)) +
  geom_point(aes(color = lakeid)) +
  facet_wrap(~season) +
  ggtitle("SAFA vs max secchi")

ggplot(fa_secchi, aes(mean_secnview, (MUFA_perc_avg + PUFA_perc_avg))) +
  geom_point(aes(color = lakeid)) +
  facet_wrap(~season) +
  ggtitle("MUFA/PUFA vs mean secchi")

ggplot(fa_secchi, aes(mean_secnview, SAFA_perc_avg)) +
  geom_point(aes(color = lakeid)) +
  facet_wrap(~season) +
  ggtitle("SAFA vs mean secchi")

#what are the far right communities?
ggplot(fa_secchi, aes(mean_secnview, (MUFA_perc_avg + PUFA_perc_avg), label = winter_yr)) +
  geom_text() +
  geom_point(aes(color = lakeid)) +
  facet_wrap(~season) +
  ggtitle("MUFA/PUFA vs mean secchi")


#=============================================================================
# ----> read in snow/ice

icesnow_orig<-read.csv("../Data/ntl34_v5.csv", stringsAsFactors = FALSE)

#can we get minimum snow for dates with multiple samples?
icesnow_orig %>% 
  group_by(lakeid, sampledate) %>% 
  summarize(n = length(avsnow)) %>% 
  filter(n > 1)



#keep only columns of interest
icesnow<-icesnow_orig %>% select(lakeid,year=year4,sampledate,sta,avsnow,sdsnow,totice,whiteice,blueice)

#=============================================================================
# ----> read in light

light_orig<-read.csv("../Data/ntl29_v5.csv", stringsAsFactors = FALSE)
light<-light_orig %>% select(lakeid,year=year4,sampledate,sta,depth,light)

# play with depth groups?
light$depthgroup<-light$depth
light$depthgroup[which(light$depth <=2)]<-"0-2"
light$depthgroup[which(light$depth >2 & light$depth<=5)]<-"2-5"
light$depthgroup[which(light$depth >5 & light$depth<=10)]<-"5-10"
light$depthgroup[which(light$depth >10 & light$depth<=15)]<-"10-15"
light$depthgroup[which(light$depth >15 & light$depth<=20)]<-"15-20"
light$depthgroup[which(light$depth >20)]<-"20-99"
light$depthgroup[which(light$depth =="0-1")]<-"0-2"
light$depthgroup[which(light$depth =="0-8")]<-"0-8"

light<-light %>% select(-depth)

#=============================================================================
# ----> read in chl

chl_north_orig<-read.csv("../Data/ntl35_v2.csv", stringsAsFactors = FALSE)
chl_south_orig<-read.csv("../Data/ntl38_v3.csv", stringsAsFactors = FALSE)
chlr_north<-chl_north_orig %>% select(lakeid,year=year4,sampledate,depth,chl=chlor,phaeo)
chlr_south<-chl_south_orig %>% select(lakeid,year=year4,sampledate,depth=depth_range_m,chl=tri_chl_spec,phaeo=phaeo_spec)
chlr<-rbind(chlr_north,chlr_south)

#=============================================================================
# combine chl, snow/ice, and light datasets

merge<-merge(icesnow,chlr, by=c("lakeid","sampledate","year")) #keep only dates that are in both
snowicechl<-merge

#make date a date
snowicechl$sampledate <- as.Date(snowicechl$sampledate, format = "%m/%d/%Y")

snowicechl$depthgroup<-snowicechl$depth

# play with depth groups?
snowicechl$depthgroup[which(as.numeric(snowicechl$depth) <=2)]<-"0-2"
snowicechl$depthgroup[which(as.numeric(snowicechl$depth) >2 & as.numeric(snowicechl$depth)<=5)]<-"2-5"
snowicechl$depthgroup[which(as.numeric(snowicechl$depth) >5 & as.numeric(snowicechl$depth)<=10)]<-"5-10"
snowicechl$depthgroup[which(as.numeric(snowicechl$depth) >10 & as.numeric(snowicechl$depth)<=15)]<-"10-15"
snowicechl$depthgroup[which(as.numeric(snowicechl$depth) >15 & as.numeric(snowicechl$depth)<=20)]<-"15-20"
snowicechl$depthgroup[which(as.numeric(snowicechl$depth) >20)]<-"20-99"
#snowicechl$depthgroup[which(snowicechl$depth =="0-1")]<-"0-2"
#snowicechl$depthgroup[which(snowicechl$depth =="0-8")]<-"0-8"

merge.test<-merge(icesnow,chlr, by=c("lakeid","sampledate"),all=TRUE)

#fix dates
merge.test$sampledate <- as.Date(merge.test$sampledate, format = "%m/%d/%Y")

# check to see how many jan/feb chla values lack snow/ice data
janfeb<-subset(merge.test,month(merge.test$sampledate) %in% c(1,2))
janfeb.missingsnowice<-janfeb[which((is.na(janfeb$avsnow)+is.na(janfeb$totice))==2),]
100*length(janfeb.missingsnowice[,1])/length(merge[,1])
# 0.6084396,  so ice or snow data measured for 99.4% of jan/feb chl samples

merge<-merge(snowicechl,light,by=c("lakeid","sampledate","depthgroup","sta","year"))
merge2<-subset(merge,merge$avsnow>=0|merge$totice>=0)
merge3<-merge2[-which(duplicated(merge2)==TRUE),]
snowicechllight<-merge3
#snowicechl<-snowicechllight

snowicechllight.aggdepths<-snowicechllight %>% group_by(lakeid,sampledate,avsnow,sdsnow,totice,whiteice,blueice) %>%
  dplyr::summarize(chl=mean(chl,na.rm=TRUE),
                   light=mean(light,na.rm=TRUE)) %>% as.data.frame()

snowicechllight.aggdepthgroups<-snowicechllight %>% group_by(lakeid,depthgroup,sampledate,avsnow,sdsnow,totice,whiteice,blueice) %>%
  dplyr::summarize(chl=mean(chl,na.rm=TRUE),
                   light=mean(light,na.rm=TRUE)) %>% as.data.frame()


# plot chl~snow, all depths
dataplot<-snowicechllight
ggplot(dataplot,aes(x=avsnow,y=chl,color=sdsnow)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plot chl~snow, depth groups
dataplot<-snowicechllight.aggdepthgroups
ggplot(dataplot,aes(x=avsnow,y=chl,color=sdsnow, shape=depthgroup)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plat chl~snow, aggregate depths
dataplot<-snowicechllight.aggdepths
ggplot(dataplot,aes(x=avsnow,y=chl,color=sdsnow)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plot chl~light, all depths
dataplot<-snowicechllight
ggplot(dataplot,aes(x=light,y=chl,color=sdsnow)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plot chl~light, depth groups
dataplot<-snowicechllight.aggdepthgroups
ggplot(dataplot,aes(x=light,y=chl,color=sdsnow, shape=depthgroup)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plat chl~light, aggregate depths
dataplot<-snowicechllight.aggdepths
ggplot(dataplot,aes(x=light,y=chl,color=sdsnow)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")


# plot snow~light, all depths
dataplot<-snowicechllight
ggplot(dataplot,aes(x=avsnow,y=light,color=sdsnow)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plot snow~light, depth groups
dataplot<-snowicechllight.aggdepthgroups
ggplot(dataplot,aes(x=avsnow,y=light,color=sdsnow, shape=depthgroup)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")

# plat snow~light, aggregate depths
dataplot<-snowicechllight.aggdepths
ggplot(dataplot,aes(x=avsnow,y=light,color=sdsnow)) +
  geom_point()+
  facet_wrap(~lakeid,scales = "free")




snowicechllight.shallow<-subset(snowicechllight.aggdepthgroups,snowicechllight.aggdepthgroups$depthgroup=="0-2")
snowicechllight.shallow<-subset(snowicechllight.shallow,snowicechllight.shallow$lakeid %in% c("AL","BM","CB","CR","SP","TB","TR"))

lightchla_7lakes<-ggplot(snowicechllight.shallow,aes(x=light,y=chl)) +
  geom_point()+
  ylab("Chlorophyll a")+
  xlab("PAR (uM per m2 per sec)")+
  facet_wrap(~lakeid,scales = "free")+
  theme_bw()

lightchla_BM<-ggplot(subset(snowicechllight.shallow,snowicechllight.shallow$lakeid=="BM"),aes(x=light,y=chl)) +
  geom_point()+
  ylab("Chlorophyll a")+
  xlab("PAR (uM per m2 per sec)")+
  theme_bw()#+
#  facet_wrap(~lakeid,scales = "free")


#snowicechllight.shallow<-subset(snowicechllight.aggdepthgroups,snowicechllight.aggdepthgroups$depthgroup=="0-2")
lightsnow_7lakes<-ggplot(snowicechllight.shallow,aes(x=avsnow,y=light)) +
  geom_point()+
  xlab("Snow Depth (cm)")+
  ylab("PAR (uM per m2 per sec)")+
  facet_wrap(~lakeid,scales = "free")+
  theme_bw()

lightsnow_BM<-ggplot(subset(snowicechllight.shallow,snowicechllight.shallow$lakeid=="BM"),aes(x=avsnow,y=light)) +
  geom_point()+
  xlab("Snow Depth (cm)")+
  ylab("PAR (uM per m2 per sec)")+
#  facet_wrap(~lakeid,scales = "free")+
  theme_bw()

png(filename = "../Figures/lightchla_7lakes.png",width = 5, height = 5, units = "in", res = 500)
lightchla_7lakes
dev.off()

png(filename = "../Figures/lightsnow_7lakes.png",width = 5, height = 5, units = "in", res = 500)
lightsnow_7lakes
dev.off()


png(filename = "../Figures/lightchla_BM.png",width = 2.5, height = 2.5, units = "in", res = 500)
lightchla_BM
dev.off()

png(filename = "../Figures/lightsnow_BM.png",width = 2.5, height = 2.5, units = "in", res = 500)
lightsnow_BM
dev.off()