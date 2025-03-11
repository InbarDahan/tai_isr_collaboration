

                           # Data exploration 
                      # full data set (1983 - 2024)

# Aim - to get depth affinity of species at different strips along the north sea
# Modular code - should fit to multiple sea, polygon sizes and time periods
# ---> loop based code

#data wrangling:
library(ggplot2)
library(dplyr)
library(tidyr)

#spatial data:
library(sf)           # dealing with spatial objects
library(raster)       # dealing with raster objects
library(terra)        # has a function to rasterize polygons
library(mapview)

#models:
library(eHOF)
library(senlm)
# - - - - - - - - -

# read - depth (raster), grid (shp), north_sea data (csv):
depth <- raster("./data/raw/north_sea/depth_shp/gebco_2024_n63.0_s49.0_w-5.0_e15.0.tif")
grid <- read_sf("./data/raw/north_sea/NorthSeaShapeFile/DATRAS.shp")
north_sea_full <- read.csv("./data/raw/north_sea/NorthSea_Quarter1_Abund_1983_2024.csv")

# - - - - - - - - -
# add a sample ID column based on HaulNo, Year and GRIDID:
north_sea_full <- north_sea_full %>% mutate(Sample_ID = paste(HaulNo, Year, GRIDID, Depth, ShootLat, sep = "_"))

# sum together the abundance for duplicate samples per species (because in the original data they correspond to different age groups)
north_sea_full_sum <- north_sea_full %>% 
  group_by(Sample_ID, Year, Depth, ShootLong, ShootLat, HaulNo, GRIDID, Sci_name) %>%
  summarise(abundance = sum(TotalNo, na.rm = TRUE), .groups = "drop") %>%
  mutate(abundance = as.integer(abundance)) 

# - - - - - - - - -
# Convert sample data to sf object:
north_sea_sf <- st_as_sf(north_sea_full, coords = c("ShootLong", "ShootLat"), crs = st_crs(grid)) 

# - - - - - - - - -
# mapview - visualization:
mapview(grid)
mapview(depth, add = T) +
mapview(north_sea_sf, add = T, zcol = 'Year')

# - - - - - - - - -
# crop depth layer by polygon extent + match crs (depth layer is too big):
new_depth = crop(depth, extent(grid))   

# Mask the raster to the shapefile boundary
new_depth <- mask(depth, grid)

# plot
mapview(new_depth)

# save
writeRaster(new_depth, "./data/raw/north_sea/depth_shp/new_depth.tif", format="GTiff")
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

    #  1 - a loop to define the study area polygons - by depth

#create an empty list of polygons

# what I want the for loop to do:
# - for the study area of the north sea 
# - divide the area to multiple polygons in a x degrees tilt

#
#
#
#
#
#
#
#
#


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

# - - - - - 
# polygon_list <- list()
# 
# 
# extract_polygon_samples <- purrr::map(polygon_list ~ {
# 
#   
#   })

#____________________

                         # 'senlm'
               # modeling depth distributions

# 1 - preparing the data in the right format for the models:

# Replace NULL (NA) values with 0
north_sea_full_sum[is.na(north_sea_full_sum)] <- 0

# - - - - - 
# changing format to data frame
north_sea_full_sum <- as.data.frame(north_sea_full_sum) 

# - - - - - - - - - - - - - - - - - -  
### In case we want to run the models for each year separately

# create a vector for the survey years
years <- north_sea_full_sum %>% pull(Year) %>% unique() %>% as.list %>% purrr::set_names(.)

# ~ loop - create a subset data for each year:
subset_years <- purrr::map(years,
                           function(year) {
                             north_sea_full_sum %>%
                               dplyr::filter(Year == year)
                           })

# - - - - - - - - - - - - - - - - - -  

# 2 - creating the list of models to run over:

# count models - discreet data (depth) 
count_models <- set_models (mean_class = "main", err_class= "count", method = "crossed")

  # - - - - - - - - - - - - - - - - - - 

# 3 - fitting models - getting the best one:

# i)
Pars_c <- create_default_par_list (count_models)

# ii)
fitted_models_one_year <- msenlm(models = count_models, data = subset_years[[1]] ,xvar = "depth", yvar = "depth", conf.level=0.95)


fitted_models_year <- purrr::map(subsets_years, function(year) {
  msenlm(models = count_models, data = year, xvar = "depth", yvar = "depth", conf.level=0.95)})


#______________________________________________________________

##################################################################

### the code for the loop from chat gpt:

library(selmn)  # Load the package if not already loaded

# Assuming 'df' is your dataset
unique_years <- unique(df$year)  # Get all unique years
model_results <- list()  # Store results

for (yr in unique_years) {
  subset_year <- df[df$year == yr, ]  # Subset data for the year
  unique_species <- unique(subset_year$species)  # Get species for that year
  
  for (sp in unique_species) {
    subset_species <- subset_year[subset_year$species == sp, ]  # Filter species data
    
    # Ensure there is sufficient data to model
    if (nrow(subset_species) > 3) {  # Adjust this threshold as needed
      
      # Define the model (assuming count_models is predefined)
      model <- msenlm(models = count_models, 
                      data = subset_species, 
                      xvar = "depth", 
                      yvar = "count", 
                      conf.level = 0.95)
      
      # Store results with unique identifier
      model_results[[paste(yr, sp, sep = "_")]] <- model
    }
  }
}

# View the stored models
str(model_results)

#####################################################################



















# 
#                          # 'senlm'
#                # modeling depth distributions
# 
# # 1 - preparing the data in the right format for the models:
# 
# # transpose species from one column to multiple columns, with their abundances as the fill:
# north_sea_wide <- north_sea_full_sum %>% pivot_wider(names_from = Sci_name, values_from = abundance)
# 
# # - - - - - 
# # Replace NULL (NA) values with 0
# north_sea_wide[is.na(north_sea_wide)] <- 0 
# 
# # - - - - - 
# # changing format to data frame
# north_sea_wide <- as.data.frame(north_sea_wide) 
# 
# # - - - - - - - - - - - - - - - - - -  
# ### In case we want to run the models for each year separately
# 
# # create a vector for the survey years
# years <- north_sea_wide %>% pull(Year) %>% unique() %>% as.list %>% purrr::set_names(.)
# 
# # ~ loop - create a subset data for each year:
# subset_years <- purrr::map(years,
#                              function(year) {
#                                north_sea_wide %>%
#                                  dplyr::filter(Year == year)
#                              })
# 
# # - - - - - - - - - - - - - - - - - -  
# 
# # 2 - creating the list of models to run over:
# 
# # count models - discreet data (depth) 
# count_models <- set_models (mean_class = "main", err_class= "count", method = "crossed")
# 
# # - - - - - - - - - - - - - - - - - - 
# 
# # 3 - fitting models - getting the best one:
# 
# # i)
# Pars_c <- create_default_par_list (count_models)
# 
# # ii)
# fitted_models_one_year <- msenlm(models = count_models, data = subset_years[[1]] ,xvar = "depth", yvar = "depth", conf.level=0.95)
# 
# 
# fitted_models_year <- purrr::map(subsets_years, function(year) {
#   msenlm(models = count_models, data = year, xvar = "depth", yvar = "depth", conf.level=0.95)})
# 
# 
# 
# 
# 
# # - - - - -
# #species list on this year
# species_list <- unique(north_sea_full$Sci_name)
# 





























# test hof models:
# 

hof_results <- lapply(species_list, function(species) {
  species_data <- subset(d_1983, Sci_name == species)
  if(nrow(species_data) > 5) {  # Ensure enough data points
    model <- HOF(TotalNo ~ Depth, data = species_data, family = poisson)
    return(list(species = species, model = model))
  } else {
    return(NULL)  # Skip species with insufficient data
  }
})
























# checking why there were duplicates:
Raja_clavata <- north_sea_full_t %>% filter( Sci_name == 'Raja clavata')

anyDuplicated(Raja_clavata$Sample_ID)

duplicates <- north_sea_full %>%
  group_by(Sci_name, Sample_ID) %>%
  filter(n() > 1) %>%
  ungroup()

duplicates_2 <- north_sea_full_t %>%
  group_by(Sci_name, Sample_ID) %>%
  filter(n() > 1) %>%
  ungroup()






