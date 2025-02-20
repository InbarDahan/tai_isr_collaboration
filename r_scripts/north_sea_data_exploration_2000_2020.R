

                     # Data exploration
         # connecting richness variation to depth range shifts
                # Exploring the 2000 - 2020 data

# - Species: checking if we have enough species that were present across all the time periods
# - and have multiple depth data points at each time period and grid cells

# packages:
library(dplyr)
library(ggplot2)

# read data
north_sea <- read.csv("./data/raw/north_sea/NorthSea_2000_2020.csv")

#______________________________________________________________

# add a "time period" column
north_sea <- north_sea %>% mutate(time_periods = case_when(
    Year %in% 2000:2002 ~ "first_p",
    Year %in% 2009:2011 ~ "second_p",
    Year %in% 2018:2020 ~ "third_p",
    TRUE ~ NA_character_  # Assign NA to other years
  ))

# number of samples per species per year
sp_year <- north_sea %>% select(Year, Sci_name) %>% group_by(Year, Sci_name) %>%
  summarise(n = n())


# number of years per species
sp_n_year <- sp_year %>% group_by(Sci_name) %>% summarise(n_years = n())


# number of species per number of years
y_count <- sp_n_year %>% group_by(n_years) %>% summarise(n = n())

#______________________________________________________________

         # number of samples per species faceted by year:

# Reorder Sci_name as a factor based on total sample count (descending order)
sp_year_fa <- sp_year %>%
  mutate(Sci_name = factor(Sci_name, levels = unique(Sci_name[order(-n)])))
# ---

### plot - number of samples per species per year

ggplot(sp_year_fa, aes(x = Sci_name, y = n, color = Year)) +
  geom_point() +
  labs(
    title = "Samples (n) per species",
    x = "Species",
    y = "Sample Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels for readability
  ) +
  facet_wrap(~ Year)

#______________________________________________________________

           # count how many unique grid cells 
        #each species appears in per time period

species_grid_counts <- north_sea %>%
  group_by(time_periods, Sci_name) %>%
  summarise(n_cells = n_distinct(GRIDID), .groups = "drop")
# ---

### Plot - species occurrences across grid cells

ggplot(species_grid_counts, aes(x = n_cells, fill = time_periods)) +
  geom_histogram(binwidth = 2, alpha = 0.7, position = "dodge") +
  labs(
    title = "Species Occurrence Across Grid Cells",
    x = "Number of Grid Cells a Species Appears In",
    y = "Species Count"
  ) +
  facet_wrap(~ time_periods)

#______________________________________________________________

species_depth_ranges <- north_sea %>%
  group_by(time_periods, Sci_name) %>%
  summarise(min_depth = min(Depth, na.rm = TRUE),
            max_depth = max(Depth, na.rm = TRUE),
            range_depth = max_depth - min_depth,
            .groups = "drop")

# Plot depth range distribution
ggplot(species_depth_ranges, aes(x = range_depth, fill = time_periods)) +
  geom_histogram(binwidth = 10, alpha = 0.7, position = "dodge") +
  labs(
    title = "Species Depth Range Across Time Periods",
    x = "Depth Range (m)",
    y = "Species Count"
  ) +
  theme_minimal()

#______________________________________________________________

# looking for grid cells that appeared through the whole time period and have 
        # a consistent species list for the depth analysis

species_sample_counts <- north_sea %>%
  group_by(time_periods, Year, Sci_name) %>%
  summarise(n_samples = n_distinct(HaulNo), .groups = "drop")

# Plot: Number of samples per species per period
ggplot(species_sample_counts, aes(x = Sci_name, y = n_samples, fill = time_periods)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Samples per Species in Each Time Period",
    x = "Species",
    y = "Number of Samples"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#______________________________________________________________

                 # Data filtering

# Step 1 - find the grid cells that appeared through all time periods and count them:

### per time period

# - tables that summarises the number of time periods for each grid cell
grids_periods <- north_sea %>%
  filter(!is.na(time_periods)) %>%
  group_by(GRIDID) %>%
  summarise(n_periods = n_distinct(time_periods), .groups = "drop") 

# - counts of cells that appeared in 1, 2 or 3 periods
n_grids_period <- grids_periods %>% 
  group_by(n_periods) %>% 
  summarise(n_cells = n())
  
# - list of cells that appeared through all time periods (n_periods = 3, n = 156):
common_grids <- north_sea %>% 
  filter(!is.na(time_periods)) %>% 
  group_by(GRIDID) %>%
  summarise(n_periods = n_distinct(time_periods), .groups = "drop") %>%
  filter(n_periods == 3) %>%
  pull(GRIDID)

# - - - - - - - - - - 

### per year

# - tables that summarises the number of years for each grid cell
grids_years <- north_sea %>%
  filter(!is.na(Year)) %>%
  group_by(GRIDID) %>%
  summarise(n_years = n_distinct(Year), .groups = "drop") 

# - counts of cells that appeared in 1, 2 or 3 periods
n_grids_years <- grids_years %>% 
  group_by(n_years) %>% 
  summarise(n_cells = n())

# - list of cells that appeared through all time periods (n_periods = 3, n = 156):
common_grids_y <- north_sea %>% 
  filter(!is.na(Year)) %>% 
  group_by(GRIDID) %>%
  summarise(n_years = n_distinct(Year), .groups = "drop") %>%
  filter(n_years == 9) %>%
  pull(GRIDID)

#____________________________________________________________________
 
                             # To do:

### check in how many periods each species was present and how many samples in has within each period
## - plot and sum for each species how many samples it had at each period
## - chose a threshold that is enough for the calculations of the depth ranges

