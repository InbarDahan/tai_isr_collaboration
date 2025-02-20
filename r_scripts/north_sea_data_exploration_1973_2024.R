

                           # Data exploration 
                      # full data set (1983 - 2024)

# Aim - to get depth affinity of species at different strips along the north sea
# Modular code - should fit to multiple sea, polygon sizes and time periods
# ---> loop based code

library(ggplot2)
library(dplyr)
library(sf)           # dealing with spatial objects
library(raster)       # dealing with raster objects
library(terra)        # has a function to rasterize polygons

# read - depth (raster), grid (shp), north_sea data (csv):
depth <- raster("./data/raw/north_sea/depth_shp/gebco_2024_n63.0_s49.0_w-5.0_e15.0.tif")
grid <- read_sf("./data/raw/north_sea/NorthSeaShapeFile/DATRAS.shp")
north_sea_full <- read.csv("./data/raw/north_sea/NorthSea_Quarter1_Abund_1983_2024.csv")

# - - - - - - - - -
# Convert sample data to sf object:
north_sea_sf <- st_as_sf(north_sea_full, coords = c("ShootLong", "ShootLat"), crs = st_crs(grid))  

#______________________________________________________________

               # data_exploration 
       # how many samples in a grid cell 

# Spatial join: Count how many samples fall in each grid cell:
grid_data <- grid %>%
  st_join(north_sea_sf, join = st_intersects) %>%
  group_by(GRIDID) %>%  # Replace with actual grid ID column name
  summarise(sample_count = n(), .groups = "drop") %>%
  mutate(fill_color = ifelse(sample_count > 0, sample_count, NA))  # Assign fill color based on count

# - - - - - - - - - 

# plot  - number of samples in each grid cell:
ggplot() +
  geom_sf(data = grid_data, aes(fill = sample_count), color = "black") +
  scale_fill_gradient(low = "white", high = "black", na.value = "white") +
  theme_minimal() +
  labs(title = "Sample Density Across Grid Cells", fill = "Sample Count")

# - - - - - - - - - 

# plot - histogram of samples count per grid:
hist(grid_data$sample_count) # most grids have <1000 samples

#______________________________________________________________

       # a loop do define the study area polygons

# ...



#______________________________________________________________

# after we have the polygons:

       # a loop to calculate the depth dist' for each species 
         # in a polygon, for modular number of years

# what I want the for loop to do:
# - for any polygon in the polygon list
# - extract the samples that fall within the area of the polygon
# - filter the time period of interest
# - for any species of fish
# - model their depth distribution ("Hof" or "senlm" models - complex relationships)
# - extract the properties of interest from the model (mode, CI...)
# (next step - plot to compare depth preferences among polygons)







