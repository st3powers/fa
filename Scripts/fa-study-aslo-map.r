#Make a map of all lakes used in the data from ALSO presentation

#I initially set working directory in Ancient-Lakes-Viz folder because
#this is the home of the large global lakes & wetlands shapefile
#However, much of the data comes from fatty acids project

######### Load packages ############
library(grid)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(tidyr)
library(ggmap)
library(rgdal)
library(raster)
library(cowplot)
library(maps)
library(maptools)
library(ggplot2)
library(grid)
# http://egallic.fr/scale-bar-and-north-arrow-on-a-ggplot2-map/
# devtools::install_github("3wen/legendMap")
library(legendMap)


##############################################################
##                      Read, wrangle data                  ##
##############################################################

#Read in aggregated FA data, created by 'euli_fa_alternate_aggregate.R' script
full_dat_weighted_comm_agg <- read.csv("../fa/Data/EULI_lake_seasonal_community_FAs.csv")

#Pull the lake names (i.e., lakes in this study) 
study_names <- unique(full_dat_weighted_comm_agg$lakename)

#Bring in original euli data to get lake lat/lons 
euli_orig <- read.csv("../fa/Data/under_ice_data.csv", stringsAsFactors = FALSE)

#Pull data from only relevant lakes
study_subset <- euli_orig[which(euli_orig$lakename %in% study_names),] 
  study_subset <- study_subset[!duplicated(study_subset$lakename),] #keep only one row of each lake's data
  
#Check
#unique(euli_orig$lakename) == study_names #FALSE
#unique(euli_orig[which(euli_orig$lakename %in% study_names),"lakename"]) == study_names #TRUE
  
#Pull in shapefile layer for lakes (global lakes and wetlands)  
lakes <-readOGR(dsn=getwd(),layer="glwd_1")
#lakes <- spTransform(lakes_orig, CRS("+proj=longlat +datum=WGS84")) #doesn't work, and unclear of purpose
lakes <- fortify(lakes)  

##############################################################
##                        Build map                         ##
##############################################################

#Base map of world borders 
mapWorld <- borders("world", colour="gray28", fill="snow")

mp <- ggplot(data = study_subset, aes(x = stationlong, y = stationlat))  +   mapWorld 

#Add GLWD lakes to world map
mp <- mp + geom_polygon(aes(x=long, y=lat, group=group), fill='lightblue1',color='gray55', data=lakes)

#Detour to plot a restricted map for pres:


######restricted data set for only one point per map region
#Remove extra Saskatchewan
study_subset_restrict <- study_subset[-which(study_subset$lakename %in% c("Blackstrap Reservoir","Broderick Reservoir",
                                                                          "St. Denis Pond 1","St. Denis Pond 90",
                                                                          "St. Denis Pond S5338")),]
#Remove extra Ontario
study_subset_restrict <- study_subset_restrict[-which(study_subset_restrict$lakename %in% c("Lake 227")),]

#Remove extra Madison 
study_subset_restrict <- study_subset_restrict[-which(study_subset_restrict$lakename %in% c("Lake Monona")),]

#Remove extra Europe
study_subset_restrict <- study_subset_restrict[-which(study_subset_restrict$lakename %in% c("Lake Valkea-Kotinen")),]

mp_restrict <- mp + geom_point(data = study_subset_restrict, aes(x = stationlong, y = stationlat),
                               shape = 21, color="black", fill = "yellow", stroke = 1.5, size = 5.5)

#Make oceans blue
mp_restrict <- mp_restrict + ylab("") + xlab("") + theme(panel.background = element_rect(fill = 'lightblue1', colour = 'lightblue1'))

#Create one map for North America and one for Europe 
mp_NAm_res <-  mp_restrict + coord_equal(xlim = c(-115,-65), ylim = c(35, 60)) 

mp_Eur_res <-  mp_restrict + coord_equal(xlim = c(1,36), ylim = c(40, 65))

combine_map_res <- plot_grid(mp_NAm_res,mp_Eur_res, nrow = 1, align = "h", rel_heights = c(1,1))

x.grob <- textGrob("Longitude", gp = gpar(fontface = "bold", col ="black")) #x axis label

y.grob <- textGrob("Latitude", gp = gpar(fontface = "bold", col = "black"), rot = 90) #y axis label

grid.arrange(combine_map_res, left = y.grob, bottom = x.grob) #arranges map and both axis labels


#####Go ahead with normal map
#Add points for lakes of interest
mp <- mp +  geom_point(shape = 21, color="black", fill = "yellow", stroke = 1.5, size = 5.5)

#Make oceans blue
mp <- mp + ylab("") + xlab("") + theme(panel.background = element_rect(fill = 'lightblue1', colour = 'lightblue1'))

#Create one map for North America and one for Europe 
mp_NAm <-  mp + coord_equal(xlim = c(-115,-65), ylim = c(35, 60)) 
  #mp_NAm  

mp_Eur <-  mp + coord_equal(xlim = c(1,36), ylim = c(40, 65))
  #mp_Eur

  

#Combine the two maps onto one figure
combine_map <- plot_grid(mp_NAm,mp_Eur, nrow = 1, align = "h", rel_heights = c(1,1))

x.grob <- textGrob("Longitude", gp = gpar(fontface = "bold", col ="black")) #x axis label

y.grob <- textGrob("Latitude", gp = gpar(fontface = "bold", col = "black"), rot = 90) #y axis label


#Save the map, BUT the grid.arrange call does weird things with white space SO
#have been exporting manually after adjusting size in RStudio window
#something ~1000x400 dimensions seems to work well

png(filename = "../fa/Figures/aslo-map.png",width = 12, height = 4, units = "in", res = 500)
grid.arrange(combine_map, left = y.grob, bottom = x.grob) #arranges map and both axis labels
dev.off()































