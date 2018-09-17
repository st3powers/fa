# This script is used to make a map of all lakes used in the 
# data from Stephanie Hampton's 2018 ALSO presentation. 


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
# Read in aggregated fatty acid (FA) data, created by 'euli_fa_seasonal_analysis_other-rm.R' script
full_dat_weighted_comm_agg <- read.csv("../Data/EULI_lake_seasonal_community_FAs.csv")

# Pull names of lakes used in this study
study_names <- unique(full_dat_weighted_comm_agg$lakename)

# Bring in original EULI data to get lake lat/lons 
euli_orig <- read.csv("../Data/under_ice_data.csv", stringsAsFactors = FALSE)

# Pull data from only relevant lakes
study_subset <- euli_orig[which(euli_orig$lakename %in% study_names), ]
study_subset <- study_subset[!duplicated(study_subset$lakename), ] #keep only one row of each lake's data

# Pull in shapefile layer for global lakes and wetlands (GLWD) data
#lakes <- readOGR(dsn = "../../Ancient-Lakes-Viz", layer = "glwd_1") # Lives outside of FA repo
# Projection
#proj4string(lakes) <- CRS("+proj=longlat +datum=WGS84") # MRB change - could use verification

#lakes <- fortify(lakes) # MRB unclear of purpose of this


lakes.ne <- readOGR(dsn = "../data", layer = "ne_50m_lakes") # Lives outside of FA repo

proj4string(lakes.ne) <- CRS("+proj=longlat +datum=WGS84") # MRB change - could use verification

lakes.ne <- fortify(lakes.ne) # MRB unclear of purpose of this




##############################################################
##                        Build map                         ##
##############################################################

# 1. Base map of world borders 
mapWorld <- borders("world", colour = "gray28", fill = "snow")

mp <- ggplot(data = study_subset, aes(x = stationlong, y = stationlat))  +   mapWorld

# Add GLWD lakes to world map
mp <- mp + geom_polygon(aes(x = long, y = lat, group = group), fill = "lightblue1", color = "gray55", data = lakes.ne)


mp


mp.main <- mp #+  geom_point(shape = 21, color = "black", fill = "yellow", stroke = 1.5, size = 5.5)

# Make oceans blue
mp.main <- mp.main + 
  ylab("") + 
  xlab("") + 
  theme(panel.background = element_rect(fill = "lightblue1",
                                        colour = "lightblue1"))

# Create one map for North America and second for Europe  
mp_NAm <-  mp.main + coord_equal(xlim = c(-115, -65), ylim = c(35, 60)) +xlab("Longitude")+ylab("Latitude")
mp_Eur <-  mp.main + coord_equal(xlim = c(1, 36), ylim = c(40, 65))+xlab("Longitude")+ylab("Latitude")


# Plot both together
combine_map <- plot_grid(mp_NAm, mp_Eur, nrow = 1, align = "h", rel_heights = c(1 ,1))
#combine_map <- plot_grid(mp_NAm, mp_Eur, nrow = 1, align = "h", rel_heights = c(1 ,1))

combine_map


png(file="map.png",res=500,units="in",width=8,height=3)
#grid.arrange(mp_NAm,mp_Eur,ncol=2)
plot_grid(mp_NAm, mp_Eur, nrow = 1, align = "h", rel_heights = c(1 ,1))
#mp_NAm
#mp.main
dev.off()



