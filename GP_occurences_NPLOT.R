library(tidyverse)
library(ggplot2) # load ggplot graphics library
library(sf) # load Rspatial package
library(maps) # load "maps" package
library(elevatr) # load "elevatr" package
library(terra) # load "terra" package

# set map projectionsmontana_northdakota_southdakota_records.csv
map_proj <- st_crs("EPSG:4326")

# get state and county shapefiles
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = map_proj)

# filter for Montana, North Dakota, South Dakota
MT_ND_SD_states <- states[states$ID %in% c("montana", "north dakota", "south dakota"),]

counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE), crs = map_proj)

# filter counties for Montana, North Dakota, South Dakota
MT_ND_SD_counties <- subset(counties, grepl("montana|north dakota|south dakota", counties$ID))

# check geometry of objects
st_geometry(MT_ND_SD_states)
st_geometry(MT_ND_SD_counties)

# transform CRS
MT_ND_SD_states <- st_transform(MT_ND_SD_states, crs = map_proj)
MT_ND_SD_counties <- st_transform(MT_ND_SD_counties, crs = map_proj)

# double-check
st_geometry(MT_ND_SD_states)
st_geometry(MT_ND_SD_counties)

# load occurence data!
points <- read_csv("data/occurences.csv")

# look at variable spellings of state names
points$stateProvi %>% unique() %>% sort()
points <- points %>% subset(stateProvi %in% c("[North Dakota]", "Nd", "North dakota", "North Dakota",
                                              "North Dakota (State)", "Sd", "South dakota", "South Dakota", 
                                              "Montana (State)", "Montana"))

points_sf <- st_as_sf(points, coords = c("decimalLon", "decimalLat"), crs = map_proj)

# Define bounding box for Montana, North Dakota, and South Dakota due to an outlying point
bbox <- st_sfc(st_polygon(list(matrix(c(-115, 40,   # Longitudes and latitudes for the bounding box
                                        -85, 40,
                                        -85, 50,
                                        -115, 50,
                                        -115, 40), 
                                      ncol = 2, byrow = TRUE))),
               crs = map_proj)

# Filter out points outside the bounding box
points_sf_filtered <- points_sf[st_intersects(points_sf, bbox, sparse = FALSE), ]

# Simple ggplot of Montana, North Dakota, and South Dakota with filtered points
ggplot() +
  theme_bw() +
  geom_sf(data = MT_ND_SD_states, fill = "lightgray", color = "black") +  # States
  geom_sf(data = MT_ND_SD_counties, fill = "lightblue", color = "darkblue") +  # Counties (optional)
  geom_sf(data = points_sf_filtered, pch = 1) +  # Add filtered points
  theme(axis.title = element_blank()) +
  coord_sf()  # Automatically crops to the boundaries of the selected states

# Filter out points outside the bounding box for Montana, North Dakota, and South Dakota
points_sf_filtered <- points_sf[st_intersects(points_sf, bbox, sparse = FALSE), ]

# Separate out the map by class and focus on the three states, showing only filtered points
ggplot(data = MT_ND_SD_counties) +
  theme_bw() +
  geom_sf() +  # Plot only counties in Montana, North Dakota, and South Dakota
  geom_sf(data = points_sf_filtered, aes(color = class), pch = 1) +  # Show filtered points colored by 'class'
  theme(axis.title = element_blank()) +
  coord_sf()  # Automatically crop to the borders of the selected states


# Load ecoregion shapefiles for Montana, North Dakota, and South Dakota
mt_shp <- st_read("data/mt_eco_l4/mt_eco_l4.shp")  # Montana ecoregions
nd_shp <- st_read("data/nd_eco_l4/nd_eco_l4.shp")  # North Dakota ecoregions
sd_shp <- st_read("data/sd_eco_l4/sd_eco_l4.shp")  # South Dakota ecoregions

# Transform shapefiles to the same CRS
mt_shp_transf <- st_transform(mt_shp, crs = map_proj)
nd_shp_transf <- st_transform(nd_shp, crs = map_proj)
sd_shp_transf <- st_transform(sd_shp, crs = map_proj)

# subset down to 
mt_shp_transf <- mt_shp_transf %>% select(!OBJECTID)
sd_shp_transf <- sd_shp_transf %>% select(!OBJECTID)

# Combine the shapefiles for Montana, North Dakota, and South Dakota
combined_shp <- rbind(mt_shp_transf, nd_shp_transf, sd_shp_transf)

# Plot ecoregions for Montana, North Dakota, and South Dakota with filtered points
ggplot() +
  theme_bw() +
  geom_sf(data = combined_shp, aes(fill = US_L3NAME), alpha = 0.5) +  # Ecoregions
  geom_sf(data = points_sf_filtered, pch = 1) +  # Add filtered points (no outlier)
  xlab("longitude") +
  ylab("latitude")

# another common task is plotting elevation. we can use the "elevatr" package to download a DEM to do so
# download state elev raster or load locally
  # Set file path for the combined elevation data
  f_elev_states <- "data/mt_nd_sd_elevs.tif"

# Download elevation raster for Montana, North Dakota, and South Dakota, or load if exists
if (!file.exists(f_elev_states)) {
  elevation_data <- get_elev_raster(locations = MT_ND_SD_states, z = 5, clip = "locations")
  terra::writeRaster(elevation_data, f_elev_states, filetype = "GTiff")                                                           
} else {
  elevation_data <- rast(f_elev_states)
}

# calculate ground resolution for z=5 
# (https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution)
ground_resolution <- (cos(45 * pi/180) * 2 * pi * 6378137) / (256 * 2^5)
ground_resolution

# crop raster down to scale
cropped_elev <- crop(elevation_data, MT_ND_SD_states)
elev_df <- raster::as.data.frame(cropped_elev, xy = TRUE)
colnames(elev_df) <- c("x", "y", "elevation")

# Plot as gradient
ggplot(data = MT_ND_SD_states) +
  theme_bw() +
  geom_sf() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c(na.value = "transparent") +
  xlab("longitude") +
  ylab("latitude")

ggplot(data = MT_ND_SD_states) +
  theme_bw() +
  geom_sf() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = points_sf_filtered, pch = 1, color = "white") +
  xlab("longitude") +
  ylab("latitude")

# plot with simple break!
ggplot(data = MT_ND_SD_states) +
  theme_bw() +
  geom_sf() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  binned_scale(aesthetics = "fill",
               palette = function(x) c("grey", "darkgreen"),
               breaks = c(0, 1800),
               limits = c(0, 3500),
               show.limits = TRUE, 
               guide = FALSE) +
  geom_sf(data = points_sf_filtered, pch = 1, color = "black") +
  xlab("longitude") +
  ylab("latitude")

# Make sure geometries are valid
MT_ND_SD_states_valid <- st_make_valid(MT_ND_SD_states)

# Create the grid
MT_ND_SD_grid <- MT_ND_SD_states_valid %>%
  st_make_grid(cellsize = .5) %>%
  st_intersection(MT_ND_SD_states_valid) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf()

# Order grid cells from southwest to northeast
MT_ND_SD_grid <- MT_ND_SD_grid %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2]) %>%
  arrange(y, x) %>%
  mutate(cellid = row_number()) %>%
  select(-centroid, -x, -y)

# why don't we look at this?
plot(MT_ND_SD_grid)

# Transform CRS if needed
MT_ND_SD_grid <- st_transform(MT_ND_SD_grid, crs = map_proj)

# Calculate species richness in each grid cell
richness_grid <- MT_ND_SD_grid %>%  # Use MT_ND_SD_grid instead of mtgrid
  st_join(points_sf_filtered) %>%  # Join grid with species occurrence data
  mutate(overlap = ifelse(!is.na(id), 1, 0)) %>%  # Create "overlap" column
  group_by(cellid) %>%  # Group by cell IDs
  summarize(num_species = sum(overlap))  # Summarize number of species per cell

# Plot the species richness grid
ggplot(richness_grid) +
  geom_sf(data = MT_ND_SD_grid, fill = NA, size = .1) +
  geom_sf(aes(fill = num_species), color = NA) +
  labs(fill = "Richness") +
  scale_fill_viridis_c() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  ggtitle("Species Richness in the Northern Plains")

# Plot the log transformed species richness grid 
max_richness <- 150  # Set a cap for max richness

richness_grid <- richness_grid %>%
  mutate(log_num_species = log(num_species + 1))  # Adding 1 to avoid log(0)

# Plot the species richness grid with modifications
ggplot(richness_grid) +
  geom_sf(data = MT_ND_SD_grid, fill = NA, size = 0.1) +
  geom_sf(aes(fill = log_num_species), color = NA) +  # Use log-transformed values
  scale_fill_viridis_c(limits = c(0, log(max_richness + 1)), na.value = "transparent") +  # Cap the color scale
  labs(fill = "Richness (log scale)") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  ggtitle("Species Richness in the Northern Plains (Log Scale)")

