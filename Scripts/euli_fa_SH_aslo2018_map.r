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

# Check
# unique(euli_orig$lakename) == study_names #FALSE
# unique(euli_orig[which(euli_orig$lakename %in% study_names),"lakename"]) == study_names #TRUE

# Pull in shapefile layer for global lakes and wetlands (GLWD) data
lakes <- readOGR(dsn = "../../Ancient-Lakes-Viz", layer = "glwd_1") # Lives outside of FA repo
  # Projection
  proj4string(lakes) <- CRS("+proj=longlat +datum=WGS84") # MRB change - could use verification

  lakes <- fortify(lakes) # MRB unclear of purpose of this



##############################################################
##                        Build map                         ##
##############################################################

# 1. Base map of world borders 
mapWorld <- borders("world", colour = "gray28", fill = "snow")

mp <- ggplot(data = study_subset, aes(x = stationlong, y = stationlat))  +   mapWorld

# Add GLWD lakes to world map
mp <- mp + geom_polygon(aes(x = long, y = lat, group = group), fill = "lightblue1", color = "gray55", data = lakes)



# 2. Detour to plot a restricted map for presentation:

# Create restricted data set w/ only one point per map region
# (i.e. avoid overplotting)

# Remove extra Saskatchewan pts
study_subset_restrict <- study_subset[-which(study_subset$lakename %in%
                                               c("Blackstrap Reservoir",
                                                 "Broderick Reservoir",
                                                 "St. Denis Pond 1",
                                                 "St. Denis Pond 90",
                                                 "St. Denis Pond S5338")), ]
# Remove extra Ontario pts
study_subset_restrict <- study_subset_restrict[-which(study_subset_restrict$lakename %in%
                                                        c("Lake 227")), ]
# Remove extra Madison pts
study_subset_restrict <- study_subset_restrict[-which(study_subset_restrict$lakename %in%
                                                        c("Lake Monona")), ]
# Remove extra Europe pts
study_subset_restrict <- study_subset_restrict[-which(study_subset_restrict$lakename %in%
                                                        c("Lake Valkea-Kotinen")), ]

# Build the restricted map
mp_restrict <- mp +
  geom_point(data = study_subset_restrict, aes(x = stationlong, y = stationlat),
             shape = 21, color = "black", fill = "yellow", stroke = 1.5,
             size = 5.5)

# Oceans = blue
mp_restrict <- mp_restrict +
  ylab("") +
  xlab("") +
  theme(panel.background = element_rect(fill = "lightblue1",
                                        colour = "lightblue1"))


# Create one map for North America and second for Europe
mp_NAm_res <-  mp_restrict + coord_equal(xlim = c(-115, -65), ylim = c(35, 60))
mp_Eur_res <-  mp_restrict + coord_equal(xlim = c(1, 36), ylim = c(40, 65))

# Plot both together
combine_map_res <- plot_grid(mp_NAm_res, mp_Eur_res, nrow = 1, 
                             align = "h", rel_heights = c(1, 1))

x.grob <- textGrob("Longitude", gp = gpar(fontface = "bold", col = "black")) #x axis label
y.grob <- textGrob("Latitude", gp = gpar(fontface = "bold", col = "black"), rot = 90) #y axis label
grid.arrange(combine_map_res, left = y.grob, bottom = x.grob) #arranges map and both axis labels

# Save the map, BUT the grid.arrange call below does weird things with white space SO
# have been:
  # 1. Running grid.arrange call above, then...
  # 2. Exporting manually after adjusting size (1000x400) in RStudio Export window

    # png(filename = "../fa/Figures/aslo-map-yellow-large-restrict.png",width = 12, height = 4, units = "in", res = 500)
    # grid.arrange(combine_map_res, left = y.grob, bottom = x.grob) #arranges map and both axis labels
    # dev.off()



# 3. Go ahead with normal map
# Add points for lakes of interest
mp.main <- mp +  geom_point(shape = 21, color = "black", fill = "yellow", stroke = 1.5, size = 5.5)

# Make oceans blue
mp.main <- mp.main + 
  ylab("") + 
  xlab("") + 
  theme(panel.background = element_rect(fill = "lightblue1",
                                        colour = "lightblue1"))

# Create one map for North America and second for Europe  
mp_NAm <-  mp.main + coord_equal(xlim = c(-115, -65), ylim = c(35, 60)) 
mp_Eur <-  mp.main + coord_equal(xlim = c(1, 36), ylim = c(40, 65))

# Plot both together
combine_map <- plot_grid(mp_NAm, mp_Eur, nrow = 1, align = "h", rel_heights = c(1 ,1))

x.grob <- textGrob("Longitude", gp = gpar(fontface = "bold", col = "black")) #x axis label
y.grob <- textGrob("Latitude", gp = gpar(fontface = "bold", col = "black"), rot = 90) #y axis label
grid.arrange(combine_map, left = y.grob, bottom = x.grob) #arranges map and both axis labels

# Save the map, BUT the grid.arrange call below does weird things with white space SO
# have been:
  # 1. Running grid.arrange call above, then...
  # 2. Exporting manually after adjusting size (1000x400) in RStudio Export window 

    # png(filename = "../fa/Figures/aslo-map-yellow-large.png",width = 12, height = 4, units = "in", res = 500)
    # grid.arrange(combine_map, left = y.grob, bottom = x.grob) #arranges map and both axis labels
    # dev.off()
