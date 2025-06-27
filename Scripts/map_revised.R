# This script is used to make a map of all lakes used in the 
# data from Stephanie Hampton's 2018 ASLO presentation. 

# Load packages -----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggplot2)
library(ggpubr)
library(broom)
library(viridisLite)
library(ggrepel)


# Read, wrangle data ------------------------------------------------------

# Read in aggregated fatty acid (FA) data, created by 'euli_fa_seasonal_analysis_other-rm.R' script
full_dat_weighted_comm_agg <- read.csv("../Data/EULI_lake_seasonal_community_FAs.csv")

# Pull names of lakes used in this study
study_names <- unique(full_dat_weighted_comm_agg$lakename)

# Bring in original EULI data to get lake lat/lons 
euli_orig <- read.csv("../Data/under_ice_data.csv", stringsAsFactors = FALSE)

# Pull data from only relevant lakes, averaging coordinates if multiple sets
study_subset <- euli_orig %>%
  filter(lakename %in% study_names) %>%
  select(lakename, stationlat, stationlong, lakecountry) %>%
  group_by(lakename) %>%
  summarise(stationlat = mean(stationlat),
            stationlong = mean(stationlong),
            lakecountry = unique(lakecountry)) %>%
  unique() %>%
  mutate(site_source = "EULI")

ntl_sites <- euli_orig %>%
  # Note, the MS lists Sparkling Bog, too, but not present in EULI. So only
  # six lakes selected for this fig for now, but will be labeled as 7.
  filter(lakename %in% c("Trout Lake", "Sparkling Lake",
                         "Crystal Lake", "Allequash Lake",
                         "Big Muskellunge Lake", "Trout Bog")) %>%
  select(lakename, stationlat, stationlong, lakecountry) %>%
  unique() %>%
  mutate(site_source = "NTL")

# Combine lake location info from two lake groups. Also, average coords where
# lakes are very close to ea/o and label them as a group for the map
lake_locations <- bind_rows(study_subset, ntl_sites) %>%
  mutate(label = case_when(
    
    lakename %in% c("St. Denis Pond S5338", "St. Denis Pond 1",
                    "St. Denis Pond 90", "Blackstrap Reservoir",
                    "Broderick Reservoir", "Lake Diefenbaker") ~ "6 lakes",
    lakename %in% c("Lake 227", "Lake 239") ~ "2 lakes_a",
    lakename %in% c("Lake Mendota", "Lake Monona") ~ "2 lakes_b",
    lakename %in% c("Lake Valkea-Kotinen", "Lake Vanajanselka") ~ "2 lakes_c",
    lakename %in% c("Trout Lake", "Crystal Lake", "Sparkling Lake",
                    "Trout Bog", "Big Muskellunge Lake") ~ "7 lakes",
    TRUE ~ lakename)) %>%
  group_by(label) %>%
  mutate(avg_lat = mean(stationlat),
         avg_long = mean(stationlong)) %>%
  ungroup() %>%
  mutate(label = case_when(
    
    grepl(pattern = "_[a-c]", x = label) ~ gsub(pattern = "_[a-c]",
                                                replacement = "",
                                                x = label),
    grepl(pattern = "lakes$", x = label) ~ label,
    TRUE ~ NA_character_),
    panel = case_when(
      # Separate lake labeling schemes for different lake clusters:
      ((lakecountry %in% c("Canada", "USA")) &
         (label == "7 lakes")) ~ "North America A",
      ((lakecountry %in% c("Canada", "USA")) &
        (label != "7 lakes")) ~ "North America B",
      ((lakecountry %in% c("Canada", "USA")) &
         (is.na(label)) & 
         (site_source == "EULI")) ~ "North America C",
      ((lakecountry %in% c("Canada", "USA")) &
         (is.na(label)) & 
         (site_source == "NTL")) ~ "North America B",
      lakecountry %in% c("Italy", "Finland", "Germany") ~ "Europe"
    )) %>%
  select(site_source, label, avg_lat, avg_long, panel) %>%
  unique() %>%
  split(f = .$panel)


# Read in and prepare spatial data
lakes.ne <- st_read("../Data/ne_50m_lakes.shp")

# Make map ----------------------------------------------------------------

# Base map of world borders 
mapWorld <- borders("world", colour = "gray28", fill = "snow")

main_map <- ggplot() +
  mapWorld +
  geom_sf(fill = "lightblue1", color = "gray35", data = lakes.ne) +
  ylab("") +
  xlab("") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "lightblue1",
                                        colour = "lightblue1"),
        panel.grid = element_blank())

# Create one map for North America and second for Europe  
mp_NAm <- main_map +
  geom_point(data = lake_locations[["North America A"]],
             aes(x = avg_long, y = avg_lat,
                 fill = site_source, shape = site_source),
             size = 3, color = "gray35") +
  geom_point(data = lake_locations[["North America B"]],
             aes(x = avg_long, y = avg_lat,
                 fill = site_source, shape = site_source),
             size = 3, color = "gray35") +
  geom_point(data = lake_locations[["North America C"]],
             aes(x = avg_long, y = avg_lat,
                 fill = site_source, shape = site_source),
             size = 3, color = "gray35") +
  coord_sf(xlim = c(-115, -65), ylim = c(35, 60)) +
  scale_fill_manual(values = inferno(n = 2, begin = 0.4, end = 0.85)) +
  scale_shape_manual(values = c(21, 24)) +
  # One layer with a point that needs a segment
  geom_text_repel(data = lake_locations[["North America A"]],
                  aes(x = avg_long, y = avg_lat, label = label),
                  min.segment.length = 0, nudge_x = -8, nudge_y = -2) +
  # One layer with no segments needed
  geom_text_repel(data = lake_locations[["North America B"]],
                  aes(x = avg_long, y = avg_lat, label = label),
                  seed = 46) +
  theme(legend.position = "none", axis.text = element_text(size = 11)) 

mp_Eur <- main_map +
  geom_point(data = lake_locations[["Europe"]],
             aes(x = avg_long, y = avg_lat,
                 fill = site_source, shape = site_source),
             size = 3, color = "gray35") +
  coord_sf(xlim = c(1, 36), ylim = c(40, 65)) +
  scale_fill_manual(values = inferno(n = 2, begin = 0.4, end = 0.85)) +
  scale_shape_manual(values = c(21, 24)) +
  geom_text_repel(data = lake_locations[["Europe"]],
                  aes(x = avg_long, y = avg_lat, label = label),
                  min.segment.length = 0, nudge_x = -7, nudge_y = 3) +
  theme(legend.position = "none", axis.text = element_text(size = 11)) 


# Combine both maps
combine_map <- ggarrange(mp_NAm, mp_Eur, ncol = 2, nrow = 1, align = 'h')
combine_annotate <- annotate_figure(p = combine_map,
                                    left = "Latitude", bottom = "Longitude")

# Preview figure
dev.off()
combine_annotate

# Export
ggsave(filename = "../Figures/map.png", plot = combine_map,
       width = 8, height = 5, units = "in", bg = "white")




