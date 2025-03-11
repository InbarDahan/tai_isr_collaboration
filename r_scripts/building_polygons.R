
library(sf)
library(terra)

# Get bounding box of the study area
extent_bounds <- as.vector(ext(new_depth))  # xmin, xmax, ymin, ymax

# Define number of polygons
num_polygons <- 10

# Function to create a tilted polygon from SW to NE
create_tilted_polygon <- function(xmin, xmax, ymin, ymax, shift_x, shift_y) {
  coords <- matrix(c(
    xmin, ymin,
    xmax, ymin + shift_y,
    xmax - shift_x, ymax,
    xmin - shift_x, ymax - shift_y,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)
  
  st_polygon(list(coords)) %>% st_sfc(crs = st_crs(new_depth))
}

# Generate 10 polygons with incremental shifts
polygon_list <- list()
for (i in 1:num_polygons) {
  shift_x <- (extent_bounds[2] - extent_bounds[1]) * 0.1 * i  # Shift right
  shift_y <- (extent_bounds[4] - extent_bounds[3]) * 0.1 * i  # Shift up
  
  poly <- create_tilted_polygon(
    xmin = extent_bounds[1] + shift_x,
    xmax = extent_bounds[2] - shift_x,
    ymin = extent_bounds[3] + shift_y,
    ymax = extent_bounds[4] - shift_y,
    shift_x = shift_x / 2,  # Controls tilt
    shift_y = shift_y / 2
  )
  
  polygon_list[[paste0("Polygon_", i)]] <- poly
}


#################################################
# deosnt work on a polygon list!! solve
polygon_sf <- st_sf(geometry = st_union(st_sfc(polygon_list)))


# Combine into a single sf object
polygon_sf <- do.call(st_union, polygon_list) %>% st_sf()
polygon_sf <- do.call(st_union, polygon_list) %>% st_sf()


# Save the polygons for later use
st_write(polygon_sf, "north_sea_tilted_polygons.shp")



