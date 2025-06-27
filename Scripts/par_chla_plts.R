#Libraries----

library(tidyverse)
library(here)


#############
# Snow Data #
#############


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/34/34/9be297624fc843fbd41f29b161150946" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "sta",     
                 "nsnow",     
                 "avsnow",     
                 "sdsnow",     
                 "wlevel",     
                 "totice",     
                 "nice",     
                 "whiteice",     
                 "blueice"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$nsnow)=="factor") dt1$nsnow <-as.numeric(levels(dt1$nsnow))[as.integer(dt1$nsnow) ]               
if (class(dt1$nsnow)=="character") dt1$nsnow <-as.numeric(dt1$nsnow)
if (class(dt1$avsnow)=="factor") dt1$avsnow <-as.numeric(levels(dt1$avsnow))[as.integer(dt1$avsnow) ]               
if (class(dt1$avsnow)=="character") dt1$avsnow <-as.numeric(dt1$avsnow)
if (class(dt1$sdsnow)=="factor") dt1$sdsnow <-as.numeric(levels(dt1$sdsnow))[as.integer(dt1$sdsnow) ]               
if (class(dt1$sdsnow)=="character") dt1$sdsnow <-as.numeric(dt1$sdsnow)
if (class(dt1$wlevel)=="factor") dt1$wlevel <-as.numeric(levels(dt1$wlevel))[as.integer(dt1$wlevel) ]               
if (class(dt1$wlevel)=="character") dt1$wlevel <-as.numeric(dt1$wlevel)
if (class(dt1$totice)=="factor") dt1$totice <-as.numeric(levels(dt1$totice))[as.integer(dt1$totice) ]               
if (class(dt1$totice)=="character") dt1$totice <-as.numeric(dt1$totice)
if (class(dt1$nice)=="factor") dt1$nice <-as.numeric(levels(dt1$nice))[as.integer(dt1$nice) ]               
if (class(dt1$nice)=="character") dt1$nice <-as.numeric(dt1$nice)
if (class(dt1$whiteice)=="factor") dt1$whiteice <-as.numeric(levels(dt1$whiteice))[as.integer(dt1$whiteice) ]               
if (class(dt1$whiteice)=="character") dt1$whiteice <-as.numeric(dt1$whiteice)
if (class(dt1$blueice)=="factor") dt1$blueice <-as.numeric(levels(dt1$blueice))[as.integer(dt1$blueice) ]               
if (class(dt1$blueice)=="character") dt1$blueice <-as.numeric(dt1$blueice)


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/35/03e232a1b362900e0f059859abe8eb97" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


##############
# light Data #
##############


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "wtemp",     
                 "o2",     
                 "o2sat",     
                 "deck",     
                 "light",     
                 "frlight",     
                 "flagdepth",     
                 "flagwtemp",     
                 "flago2",     
                 "flago2sat",     
                 "flagdeck",     
                 "flaglight",     
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt2$lakeid<- as.factor(dt2$lakeid)
if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]               
if (class(dt2$year4)=="character") dt2$year4 <-as.numeric(dt2$year4)                                   
# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt2$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt2$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt2$depth)=="factor") dt2$depth <-as.numeric(levels(dt2$depth))[as.integer(dt2$depth) ]               
if (class(dt2$depth)=="character") dt2$depth <-as.numeric(dt2$depth)
if (class(dt2$rep)!="factor") dt2$rep<- as.factor(dt2$rep)
if (class(dt2$sta)!="factor") dt2$sta<- as.factor(dt2$sta)
if (class(dt2$event)!="factor") dt2$event<- as.factor(dt2$event)
if (class(dt2$wtemp)=="factor") dt2$wtemp <-as.numeric(levels(dt2$wtemp))[as.integer(dt2$wtemp) ]               
if (class(dt2$wtemp)=="character") dt2$wtemp <-as.numeric(dt2$wtemp)
if (class(dt2$o2)=="factor") dt2$o2 <-as.numeric(levels(dt2$o2))[as.integer(dt2$o2) ]               
if (class(dt2$o2)=="character") dt2$o2 <-as.numeric(dt2$o2)
if (class(dt2$o2sat)=="factor") dt2$o2sat <-as.numeric(levels(dt2$o2sat))[as.integer(dt2$o2sat) ]               
if (class(dt2$o2sat)=="character") dt2$o2sat <-as.numeric(dt2$o2sat)
if (class(dt2$deck)=="factor") dt2$deck <-as.numeric(levels(dt2$deck))[as.integer(dt2$deck) ]               
if (class(dt2$deck)=="character") dt2$deck <-as.numeric(dt2$deck)
if (class(dt2$light)=="factor") dt2$light <-as.numeric(levels(dt2$light))[as.integer(dt2$light) ]               
if (class(dt2$light)=="character") dt2$light <-as.numeric(dt2$light)
if (class(dt2$frlight)=="factor") dt2$frlight <-as.numeric(levels(dt2$frlight))[as.integer(dt2$frlight) ]               
if (class(dt2$frlight)=="character") dt2$frlight <-as.numeric(dt2$frlight)
if (class(dt2$flagdepth)!="factor") dt2$flagdepth<- as.factor(dt2$flagdepth)
if (class(dt2$flagwtemp)!="factor") dt2$flagwtemp<- as.factor(dt2$flagwtemp)
if (class(dt2$flago2)!="factor") dt2$flago2<- as.factor(dt2$flago2)
if (class(dt2$flago2sat)!="factor") dt2$flago2sat<- as.factor(dt2$flago2sat)
if (class(dt2$flagdeck)!="factor") dt2$flagdeck<- as.factor(dt2$flagdeck)
if (class(dt2$flaglight)!="factor") dt2$flaglight<- as.factor(dt2$flaglight)
if (class(dt2$flagfrlight)!="factor") dt2$flagfrlight<- as.factor(dt2$flagfrlight)

######################################
# Chlorophyll data - Trout Lake Area # 
######################################

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/35/32/50f9b5f93d0a0d47008147698fb413f3" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "rep",     
                 "sta",     
                 "depth",     
                 "chlor",     
                 "phaeo",     
                 "flagchlor",     
                 "flagphaeo"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$lakeid)!="factor") dt3$lakeid<- as.factor(dt3$lakeid)
if (class(dt3$year4)=="factor") dt3$year4 <-as.numeric(levels(dt3$year4))[as.integer(dt3$year4) ]               
if (class(dt3$year4)=="character") dt3$year4 <-as.numeric(dt3$year4)
if (class(dt3$daynum)=="factor") dt3$daynum <-as.numeric(levels(dt3$daynum))[as.integer(dt3$daynum) ]               
if (class(dt3$daynum)=="character") dt3$daynum <-as.numeric(dt3$daynum)                                   
# attempting to convert dt3$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt3$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt3$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt3$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt3$rep)!="factor") dt3$rep<- as.factor(dt3$rep)
if (class(dt3$sta)!="factor") dt3$sta<- as.factor(dt3$sta)
if (class(dt3$depth)=="factor") dt3$depth <-as.numeric(levels(dt3$depth))[as.integer(dt3$depth) ]               
if (class(dt3$depth)=="character") dt3$depth <-as.numeric(dt3$depth)
if (class(dt3$chlor)=="factor") dt3$chlor <-as.numeric(levels(dt3$chlor))[as.integer(dt3$chlor) ]               
if (class(dt3$chlor)=="character") dt3$chlor <-as.numeric(dt3$chlor)
if (class(dt3$phaeo)=="factor") dt3$phaeo <-as.numeric(levels(dt3$phaeo))[as.integer(dt3$phaeo) ]               
if (class(dt3$phaeo)=="character") dt3$phaeo <-as.numeric(dt3$phaeo)
if (class(dt3$flagchlor)!="factor") dt3$flagchlor<- as.factor(dt3$flagchlor)
if (class(dt3$flagphaeo)!="factor") dt3$flagphaeo<- as.factor(dt3$flagphaeo)

#########################################
# Chlorophyll data - Madison Lakes Area # 
#########################################

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/38/28/66796c3bc77617e7cc95c4b09d4995c5" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "sampledate",     
                 "depth_range_m",     
                 "rep",     
                 "tri_chl_spec",     
                 "mono_chl_spec",     
                 "phaeo_spec",     
                 "uncorrect_chl_fluor",     
                 "correct_chl_fluor",     
                 "phaeo_fluor",     
                 "flag_spec",     
                 "flag_fluor"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$lakeid)!="factor") dt4$lakeid<- as.factor(dt4$lakeid)
if (class(dt4$year4)=="factor") dt4$year4 <-as.numeric(levels(dt4$year4))[as.integer(dt4$year4) ]               
if (class(dt4$year4)=="character") dt4$year4 <-as.numeric(dt4$year4)                                   
# attempting to convert dt4$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt4$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt4$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt4$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt4$depth_range_m)!="factor") dt4$depth_range_m<- as.factor(dt4$depth_range_m)
if (class(dt4$rep)!="factor") dt4$rep<- as.factor(dt4$rep)
if (class(dt4$tri_chl_spec)=="factor") dt4$tri_chl_spec <-as.numeric(levels(dt4$tri_chl_spec))[as.integer(dt4$tri_chl_spec) ]               
if (class(dt4$tri_chl_spec)=="character") dt4$tri_chl_spec <-as.numeric(dt4$tri_chl_spec)
if (class(dt4$mono_chl_spec)=="factor") dt4$mono_chl_spec <-as.numeric(levels(dt4$mono_chl_spec))[as.integer(dt4$mono_chl_spec) ]               
if (class(dt4$mono_chl_spec)=="character") dt4$mono_chl_spec <-as.numeric(dt4$mono_chl_spec)
if (class(dt4$phaeo_spec)=="factor") dt4$phaeo_spec <-as.numeric(levels(dt4$phaeo_spec))[as.integer(dt4$phaeo_spec) ]               
if (class(dt4$phaeo_spec)=="character") dt4$phaeo_spec <-as.numeric(dt4$phaeo_spec)
if (class(dt4$uncorrect_chl_fluor)=="factor") dt4$uncorrect_chl_fluor <-as.numeric(levels(dt4$uncorrect_chl_fluor))[as.integer(dt4$uncorrect_chl_fluor) ]               
if (class(dt4$uncorrect_chl_fluor)=="character") dt4$uncorrect_chl_fluor <-as.numeric(dt4$uncorrect_chl_fluor)
if (class(dt4$correct_chl_fluor)=="factor") dt4$correct_chl_fluor <-as.numeric(levels(dt4$correct_chl_fluor))[as.integer(dt4$correct_chl_fluor) ]               
if (class(dt4$correct_chl_fluor)=="character") dt4$correct_chl_fluor <-as.numeric(dt4$correct_chl_fluor)
if (class(dt4$phaeo_fluor)=="factor") dt4$phaeo_fluor <-as.numeric(levels(dt4$phaeo_fluor))[as.integer(dt4$phaeo_fluor) ]               
if (class(dt4$phaeo_fluor)=="character") dt4$phaeo_fluor <-as.numeric(dt4$phaeo_fluor)
if (class(dt4$flag_spec)!="factor") dt4$flag_spec<- as.factor(dt4$flag_spec)
if (class(dt4$flag_fluor)!="factor") dt4$flag_fluor<- as.factor(dt4$flag_fluor)



#################
# Scatter plots #
#################

library(tidyverse)

df_light_snow <- dt2 %>% dplyr::select(lakeid,sampledate,light) %>% group_by(lakeid,sampledate) %>% 
  summarise(light_mean = mean(light,na.rm = T)) %>% 
  right_join(dplyr::select(dt1,lakeid,sampledate,avsnow,sdsnow),
             by = c('lakeid'='lakeid', 'sampledate'='sampledate')) %>% 
  drop_na(light_mean,avsnow)


# df_light_snow %>% ggplot(aes(avsnow,light_mean))+
#   geom_point(alpha =0.2)+
#   facet_wrap(~lakeid)


df_chl_light_v0 <- dt3 %>% dplyr::select(lakeid,sampledate,depth,chlor) %>% group_by(lakeid,sampledate) %>% 
  summarise(depth_mean = mean(depth,na.rm = T), chlor_mean = mean(chlor,na.rm = T)) %>% 
  right_join(dt2 %>% dplyr::select(lakeid,sampledate,light) %>% group_by(lakeid,sampledate) %>% 
               summarise(light_mean = mean(light,na.rm = T)),
             by = c('lakeid'='lakeid', 'sampledate'='sampledate')) %>% 
  drop_na(light_mean,chlor_mean)

df_chl_light_v1 <- dt3 %>% dplyr::select(lakeid,sampledate,depth,chlor) %>%  filter(depth <= 12) %>% group_by(lakeid,sampledate) %>% 
  summarise(chlor_mean = mean(chlor,na.rm = T)) %>% 
  right_join(dt2 %>% dplyr::select(lakeid,sampledate,light) %>% group_by(lakeid,sampledate) %>% 
               summarise(light_mean = mean(light,na.rm = T)),
             by = c('lakeid'='lakeid', 'sampledate'='sampledate')) %>% 
  drop_na(light_mean,chlor_mean)


# df_chl_light_v0 %>% ggplot(aes(chlor_mean,light_mean))+
#   geom_point(alpha =0.1)+
#   facet_wrap(~lakeid)



# 1. Light-Snow Plot-------------------------------------------------------

library(tidyverse)
library(here)
library(ggpmisc)
library(ggpubr)

#**Read in ice phenology data----
ice <- read_csv(here('2_data/ntl32_v8.csv')) %>% 
  mutate(
    lakeid = as.factor(lakeid)
  ) %>% 
  dplyr::select(
    lakeid,
    year,
    datefirstopen,
    datefirstice
  )

#**Filter light data to vars of interest----
light_clean <- dt2 %>% 
  dplyr::select(
    lakeid,
    year = year4,
    sampledate,
    light,
    depth
  ) %>% 
  mutate(
    lakeid = as.factor(lakeid)
  ) %>% 
  #Average for top 2 meters (as in methods)
  filter(
    depth <= 2
  ) %>% 
  group_by(
    lakeid, year, sampledate
  ) %>% 
  summarise(
    light = mean(light, na.rm = T)
  )

#**Alternate light data (0m only) added  2025.03.07----
light_clean2 <- dt2 %>% 
  dplyr::select(
    lakeid,
    year = year4,
    sampledate,
    light,
    depth
  ) %>% 
  mutate(
    lakeid = as.factor(lakeid)
  ) %>% 
  #Only surface values wanted
  filter(
    depth == 0
  ) 

#**Get light data only for periods when the lake was completely ice covered----
light_iceon <- light_clean2 %>% 
  full_join(ice) %>% 
  mutate(
    water_year = ifelse(month(sampledate)>=10, year+1, year)
  ) %>% 
  group_by(lakeid, water_year) %>% 
  filter(
    sampledate < datefirstopen | sampledate > datefirstice
  )

#**Filter snow data to vars of interest----

snow_iceon <- dt1 %>% 
  dplyr::select(lakeid, 
                year = year4, 
                sampledate, 
                avsnow)

#**Join the date-filtered light data with snow data----

light_snow1 <- light_iceon %>% 
  left_join(snow_iceon) %>% 
  mutate(
    #Add lake names for the facet plot, as requested in ms doc
    lakename = ifelse(lakeid == "AL", "Allequash Lake", NA),
    lakename = ifelse(lakeid == "BM", "Big Muskellunge Lake", lakename),
    lakename = ifelse(lakeid == "CB", "Crystal Bog",lakename),
    lakename = ifelse(lakeid == "CR", "Crystal Lake", lakename),
    lakename = ifelse(lakeid == "FI", "Fish Lake", lakename),
    lakename = ifelse(lakeid == "ME", "Lake Mendota", lakename),
    lakename = ifelse(lakeid == "MO", "Lake Monona", lakename),
    lakename = ifelse(lakeid == "WI", "Lake Wingra", lakename),
    lakename = ifelse(lakeid == "SP", "Sparkling Lake", lakename),
    lakename = ifelse(lakeid == "TB", "Trout Bog", lakename),
    lakename = ifelse(lakeid == "TR", "Trout Lake", lakename),
    #Lake name as factor for facet
    lakename = as.factor(lakename)
  )

#**Add an "Every" category----

every <- light_snow1 %>% 
  dplyr::select(-lakename) %>% 
  mutate(
    lakename = as.factor('All Lakes')
  )

light_snow <- light_snow1 %>% 
  bind_rows(every) %>% 
  mutate(
    par_log = log(light),
    snow_log = log(avsnow)
  )

light_snow[sapply(light_snow, is.infinite)] <- NA

#write_csv(light_snow, here('2_data/light_snow.csv'))

#how much does light decay?

light_avg <- every %>% 
  group_by(lakename) %>% 
  filter(
    !is.na(avsnow)
  ) %>% 
  summarise(
    mean = mean(light, na.rm = T),
    max = max(light, na.rm = T),
    min = min(light, na.rm = T)
  )

decay_test1 <- every %>% 
  filter(
    avsnow > 3.675
  ) %>% 
  group_by(lakename) %>% 
  summarise(
    mean = mean(light, na.rm = T),
    max = max(light, na.rm = T),
    min = min(light, na.rm = T)
  )
summary(decay_test1)

decay_test2 <- every %>% 
  filter(
    avsnow < 1
  )%>% 
  group_by(lakename) %>% 
  summarise(
    mean = mean(light, na.rm = T),
    max = max(light, na.rm = T),
    min = min(light, na.rm = T)
  )
summary(decay_test2)

decay_test3 <- every %>% 
  filter(
    avsnow > 10.1
  )%>% 
  group_by(lakename) %>% 
  summarise(
    mean = mean(light, na.rm = T),
    max = max(light, na.rm = T),
    min = min(light, na.rm = T)
  )
summary(decay_test3)


# ggplot(data = every, aes(x = avsnow, y = light))+
#   geom_point()

#**Create scatter plot for snow and PAR----

#Fromula for lm to be printed on plot

model <- log(1+light_snow$light) ~ log(1+light_snow$avsnow)

#par_snow_plt <- ggplot(data = light_snow, aes(x = snow_log, y = par_log))+
par_snow_plt <- ggplot(data = light_snow, aes(x = avsnow, y = light))+
  geom_point(color = '#6495ED', alpha = 0.7)+
  theme_bw()+
  xlab('Snow Depth (cm)')+
  ylab(~ paste(, "PAR ", '(',mu , " M ", 'm'^-2, ' s'^-1, ')'))+
  theme_bw()+
  facet_wrap(~lakename, scales = 'free', nrow = 2)+ #, scales = 'free'
  theme_classic()+
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 25),
    strip.background = element_blank(), #removes boxes from facet_wrap titles
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )+
  geom_smooth(method = 'lm', color = 'grey50', alpha = 0.2)+
  stat_poly_eq(label.y = 0.95, label.x = 0.95)+
  scale_x_continuous(trans = 'log1p', breaks = c(0, 5, 20, 60))+
  scale_y_continuous(trans = 'log1p', breaks = c(0, 100, 500, 1000))

par_snow_plt  

#**Save plot----
#ggsave(here('4_figs/light_snow_log_pval_revised_2025.03.07.pdf'), dpi = 300, height = 8, width = 15, units = 'in')


# 2. Par~Snow: Summary stats from linear regression --------------------------

light_snow_tbl2 <- light_snow %>% 
  group_by(lakename) %>% 
  summarise(
    #slope = summary(lm(par_log ~ snow_log))$coefficients[,1][2],
    slope = summary(lm(log(1+light) ~ log(1+avsnow)))$coefficients[,1][2],
    #intercept = summary(lm(par_log ~ snow_log))$coefficients[,1][1],
    intercept = summary(lm(log(1+light) ~ log(1+avsnow)))$coefficients[,1][1],
    #p.value = summary(lm(par_log ~ snow_log))$coefficients[,4][2],
    p.value = summary(lm(log(1+light) ~ log(1+avsnow)))$coefficients[,4][2],
    #r2 = summary(lm(par_log~snow_log))$r.squared,
    #r2 = summary(lm(log(1+light) ~ log(1+avsnow)))$r.squared,
    #adj.r2 = summary(lm(par_log~snow_log))$adj.r.squared,
    adj.r2 = summary(lm(log(1+light) ~ log(1+avsnow)))$adj.r.squared,
    adj.r2_test = summary(lm(log(1+light) ~ log(1+avsnow)))$r.squared,
    # sw_test = shapiro.test(log(1+light))[2],
    n = length(light)
  )


# 3. Chl-PAR Plot ------------------------------------------------------------

#**Clean chl-a data----

#Want to look at only Jan and Feb
dt3_test <- dt3 %>% 
  dplyr::select(
    lakeid,
    year = year4,
    sampledate,
    depth,
    chlor
  ) %>%
  mutate(
    month = month(sampledate)
  ) %>% 
  filter(
    month == 1 | month == 2
  )

chl_a <- dt3_test %>% 
  # Filtering for top 2 meters as in light data
  filter(
    #depth <= 2
    depth == 0
  ) %>%
  group_by(lakeid, year, sampledate) %>% 
  summarise(
    chlor = mean(chlor, na.rm = T)
  )

chl_a_iceon1 <- chl_a %>% 
  full_join(ice) %>% 
  mutate(
    water_year = ifelse(month(sampledate)>=10, year+1, year)
  ) %>% 
  group_by(lakeid, water_year) %>% 
  filter(
    sampledate < datefirstopen | sampledate > datefirstice
  ) %>% 
  mutate(
    #Add lake names for the facet plot, as requested in ms doc
    lakename = ifelse(lakeid == "AL", "Allequash Lake", NA),
    lakename = ifelse(lakeid == "BM", "Big Muskellunge Lake", lakename),
    lakename = ifelse(lakeid == "CB", "Crystal Bog",lakename),
    lakename = ifelse(lakeid == "CR", "Crystal Lake", lakename),
    lakename = ifelse(lakeid == "FI", "Fish Lake", lakename),
    lakename = ifelse(lakeid == "ME", "Lake Mendota", lakename),
    lakename = ifelse(lakeid == "MO", "Lake Monona", lakename),
    lakename = ifelse(lakeid == "WI", "Lake Wingra", lakename),
    lakename = ifelse(lakeid == "SP", "Sparkling Lake", lakename),
    lakename = ifelse(lakeid == "TB", "Trout Bog", lakename),
    lakename = ifelse(lakeid == "TR", "Trout Lake", lakename),
    #Lake name as factor for facet
    lakename = as.factor(lakename)
  )

#**Combine with PAR data

par <- light_snow %>% 
  dplyr::select(
    lakeid,
    year,
    sampledate,
    light
  )

chl_par <- chl_a_iceon1 %>% 
  left_join(par)

#**Add an "Every" category----

every_chl <- chl_par %>% 
  dplyr::select(-lakename) %>% 
  mutate(
    lakename = as.factor('All Lakes')
  )

chl_a_iceon <- chl_par %>% 
  bind_rows(every_chl) %>% 
  filter(
    chlor >= 0
  )


#**Create scatter plot for Chla and PAR----

library(ggpmisc)

#Add log values to chl_a_iceon

chl_a_iceon_log <- chl_a_iceon %>% 
  mutate(
    chl_log = log(chlor),
    par_log = log(light)
  )

chl_a_iceon_log[sapply(chl_a_iceon_log, is.infinite)] <- NA

#write_csv(chl_a_iceon_log, here('2_data/chl_a_iceon_log.csv'))

#chl_light_plt <- ggplot(data = chl_a_iceon_log, aes(x = par_log, y = chl_log))+
chl_light_plt <- ggplot(data = chl_a_iceon_log, aes(x = light, y = chlor))+
  geom_point(color = '#348217', alpha = 0.5)+
  theme_bw()+
  facet_wrap(~lakename, scales = 'free', nrow = 2)+
  theme_classic()+
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 25),
    strip.background = element_blank(), #removes boxes from facet_wrap titles
    axis.text.x = element_text(size = 15, hjust = 0.6),
    axis.text.y = element_text(size = 15)
  )+
  eq(use_label('eq'), label.y = 0.99, label.x = 1)+
  stat_poly_eq(label.y = 0.98, label.x = 1)+
  geom_smooth(method = 'lm', color = 'grey50', alpha = 0.2)+
  xlab(~ paste("PAR ", '(',mu , " M ", 'm'^-2, ' s'^-1, ')'))+
  ylab(~ paste('Chlorophyll ', italic('a '), '(', mu,'g ', 'L'^-1,')'))+
  scale_x_continuous(trans = 'log1p', breaks = c(0, 100, 900))+ #
  scale_y_continuous(trans = 'log1p', breaks = c(0, 5, 25, 100)) ##


chl_light_plt  

#**Save plot----
# ggsave(
#   here('4_figs/chl_par_log_r2_pval_nobox_2025.06.19.png'), 
#   dpi = 300, 
#   height = 8, 
#   width = 15, 
#   units = 'in'
# )

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

# 5. PAR ~ % White Ice ----------------------------------------------------

#Read in NTL ice quality data
ntl_iq <- read_csv(here('2_data/ntl_ice_quality.csv')) 

#Clean data to bind with chl data above
ntl_iq_clean <- ntl_iq %>% 
  mutate(
    lakeid = as.factor(lakeid),
    sampledate = mdy(sampledate),
    wi_perc = round(whiteice/totice*100)
  )

#write_csv(ntl_iq_clean, here('2_data/ntl_iq_clean.csv'))

#Bind with chl data
chl_iq <- ntl_iq_clean %>% 
  inner_join(chl_a_iceon_log) 

#write_csv(chl_iq, here('2_data/chl_iq.csv'))

PAR_wi_perc_plt <- ggplot(data = chl_iq, aes(x = wi_perc, y = light))+
  geom_point(aes(colour = avsnow), size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', color = '#E69F00', alpha = 0.2)+
  facet_wrap(~lakename)+
  theme_classic()+
  ylab(~ paste("PAR ", '(',mu , " M ", 'm'^-2, ' s'^-1, ')'))+
  xlab('White Ice (%)')+
  scale_color_gradientn(
    name = 'Snow Depth (cm)',
    #colors = c('#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7'),
    colors = c('#D3D4D9', '#ADAEB3', '#828388', '#55565B', '#232429'),
    breaks = c(0,10,20,30,40,50),
    labels = format(c(0,10,20,30,40,50)),
    guide = guide_colorsteps()
  )+
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(1.5, 'cm')
  )+
  scale_y_continuous(trans = 'log1p', breaks = c(0, 6, 60, 600)) ##

PAR_wi_perc_plt

ggsave(here('4_figs/PAR_wi_perc_plt_2025.06.17.png'), dpi = 300, width = 1800, height = 1800, units = 'px')

# 6. Multiple Regression Models -------------------------------------------

library(olsrr)

chl_iq1 <- chl_iq %>% 
  filter(lakename != "All Lakes") %>%
  dplyr::select(
    lakeid, avsnow, light, par_log, chlor, chl_log, wi_perc
  ) %>% 
  mutate(
    snow_log = log(avsnow)
  ) %>%
  dplyr::select(-lakeid)

chl_iq1[sapply(chl_iq1, is.infinite)] <- NA




#Model for light as a function of snow and ice quality
# par_lm <- lm(data = chl_iq1, par_log ~ snow_log )
par_lm <- lm(data = chl_iq1, log(1+light) ~ log(1+avsnow) + wi_perc)

summary(par_lm)

plot(par_lm, which = 1) #Constant variance violated

plot(par_lm, which = 2) #Normally distributed residuals violated

chl_iq2 <- chl_iq1 %>% 
  na.omit()

ols_step_all_possible(par_lm)

chl_iq3 <- as.data.frame(chl_iq2)

vif.1 <- usdm::vif(chl_iq3)

vif.1

#Model for chl-a as function of PAR + snow depth + ice quality

# chl_lm <- lm(data = chl_iq1, chl_log ~ par_log+ snow_log + wi_perc)
# 
#summary(chl_lm)

chl_lm1 <- lm(data = chl_iq1, log(1+chlor) ~ log(1+light))

summary(chl_lm1)

chl_lm2 <- lm(data = chl_iq1, log(1+chlor) ~ log(1+light) + log(1+avsnow))

summary(chl_lm2)

chl_lm3 <- lm(data = chl_iq1, log(1+chlor) ~ log(1+light) + log(1+avsnow) + wi_perc)

summary(chl_lm3)


#**MLR for all lakes----
#*

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