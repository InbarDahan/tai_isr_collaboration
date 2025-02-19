

#              Workflow (assuming we have the appropriate data):

# 1 - For each grid cell, calculate the average richness per year (chose a coherent sampling effort)
# 2 - For each grid cell, extract the depth range for each species (min - max depth) per year 
#     * if not enough depth data maybe we can group a few years together
# 3 - Regress each species depth range against time - extract slope value
# 4 - Regress grid cell richness against time - extract slope value
# 5 - Regress slopes values to see if change in depth range correlates to change in richness

# Hypothesis - positive richness slope will be correlated with a negative depth 
#   range slope because competitive interactions reduce species depth ranges

###-----------------------------------------------------------------------------###


        # connecting richness variation to depth range shifts
                      # Running the workflow


## install.packages("dplyr")
## install.packages("readr")
library(dplyr)
library(readr)

###-----------------------------------------------------------------------------###

# Stage 1:

# Read the CSV file (replace "your_file.csv" with the actual file name)
fish_data <- read_csv("./data/raw/north_sea/NorthSea_2000_2020.csv")

head(fish_data)
# View column names
colnames(fish_data)

# Calculate the number of unique species (Sci_name) for each Year and GRIDID
result_Year_GRIDID <- fish_data %>%
  group_by(Year, GRIDID) %>%
  summarise(unique_species_count = n_distinct(Sci_name), .groups = "drop")

###-----------------------------------------------------------------------------###
### Workflow 2
# Sort the data by Year and GRIDID
sorted_fish_data <- fish_data %>%
  arrange(Year, GRIDID)

# Filter data for the years 2000 to 2002
fish_data_2000_2002 <- fish_data %>%
  filter(Year >= 2000 & Year <= 2002)

# Group by GRIDID and Sci_name, then calculate Depth_max, Depth_min, and Depth_range
Depth_range_2000_2002 <- fish_data_2000_2002 %>%
  group_by(GRIDID, Sci_name) %>%
  summarise(
    Depth_max_2000_2002 = max(Depth, na.rm = TRUE),
    Depth_min_2000_2002 = min(Depth, na.rm = TRUE),
    Depth_range_2000_2002 = Depth_max_2000_2002 - Depth_min_2000_2002,
    .groups = "drop"
  )

# Filter data for the years 2009 to 2011
fish_data_2009_2011 <- fish_data %>%
  filter(Year >= 2009 & Year <= 2011)

# Group by GRIDID and Sci_name, then calculate Depth_max, Depth_min, and Depth_range
Depth_range_2009_2011 <- fish_data_2009_2011 %>%
  group_by(GRIDID, Sci_name) %>%
  summarise(
    Depth_max_2009_2011 = max(Depth, na.rm = TRUE),
    Depth_min_2009_2011 = min(Depth, na.rm = TRUE),
    Depth_range_2009_2011 = Depth_max_2009_2011 - Depth_min_2009_2011,
    .groups = "drop"
  )

# Filter data for the years 2018 to 2020
fish_data_2018_2020 <- fish_data %>%
  filter(Year >= 2018 & Year <= 2020)

# Group by GRIDID and Sci_name, then calculate Depth_max, Depth_min, and Depth_range
Depth_range_2018_2020 <- fish_data_2018_2020 %>%
  group_by(GRIDID, Sci_name) %>%
  summarise(
    Depth_max_2018_2020 = max(Depth, na.rm = TRUE),
    Depth_min_2018_2020 = min(Depth, na.rm = TRUE),
    Depth_range_2018_2020 = Depth_max_2018_2020 - Depth_min_2018_2020,
    .groups = "drop"
  )

# Merge depth range data for the periods 2000-2002, 2009-2011, and 2018-2020
merged_depth_range <- Depth_range_2000_2002 %>%
  full_join(Depth_range_2009_2011, by = c("GRIDID", "Sci_name")) %>%
  full_join(Depth_range_2018_2020, by = c("GRIDID", "Sci_name"))


###-----------------------------------------------------------------------------###
### Workflow 3
library(dplyr)
library(tidyr)
library(purrr)

# Retain only the specified columns
filtered_depth_range <- merged_depth_range %>%
  select(GRIDID, Sci_name, Depth_range_2000_2002, Depth_range_2009_2011, Depth_range_2018_2020)

# Transform the data into long format for analysis
depth_long <- filtered_depth_range %>%
  pivot_longer(
    cols = c(Depth_range_2000_2002, Depth_range_2009_2011, Depth_range_2018_2020),
    names_to = "Year_range",
    values_to = "Depth_range"
  ) %>%
  mutate(
    Year = case_when(
      Year_range == "Depth_range_2000_2002" ~ 2001,
      Year_range == "Depth_range_2009_2011" ~ 2010,
      Year_range == "Depth_range_2018_2020" ~ 2019,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(Depth_range)

# Calculate the regression slope for each species
slope_results <- depth_long %>%
  group_by(Sci_name) %>%
  summarise(
    slope = if(n() >= 2) {
      lm_model <- lm(Depth_range ~ Year, data = cur_data())
      round(coef(lm_model)[2], 3)  # Extract the slope and round to 3 decimal places
    } else {
      NA_real_
    },
    .groups = "drop"
  )


###-----------------------------------------------------------------------------###
### Workflow 4
# Select the required columns
fish_data_GRIDID_Year_Sciname <- fish_data %>%
  select(GRIDID, Year, Sci_name)

# Display the results
print(fish_data_GRIDID_Year_Sciname)

# Calculate species richness for each GRIDID and Year
Richness_GRIDID_Year <- fish_data_GRIDID_Year_Sciname %>%
  group_by(GRIDID, Year) %>%
  summarise(
    richness = n_distinct(Sci_name),
    .groups = "drop"
  ) 

# Filter for specific years and add a Year_range column
Richness_with_YearRange <- Richness_GRIDID_Year %>%
  filter(Year %in% c(2000:2002, 2009:2011, 2018:2020)) %>%
  mutate(
    Year_range = case_when(
      Year %in% 2000:2002 ~ "2000_2002",
      Year %in% 2009:2011 ~ "2009_2011",
      Year %in% 2018:2020 ~ "2018_2020",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(GRIDID)

# Perform regression and calculate slope for each GRIDID
gridid_slope_results <- Richness_with_YearRange %>%
  group_by(GRIDID) %>%
  summarise(
    slope = {
      # Prepare valid data for regression
      years <- case_when(
        Year_range == "2000_2002" ~ 2001,
        Year_range == "2009_2011" ~ 2010,
        Year_range == "2018_2020" ~ 2019,
        TRUE ~ NA_real_
      )
      valid_data <- drop_na(data.frame(Year = years, Richness = richness))
      
      # Perform regression if there are at least 2 data points
      if (nrow(valid_data) >= 2) {
        lm_model <- lm(Richness ~ Year, data = valid_data)
        round(coef(lm_model)[2], 3)  # Extract the slope and round to 3 decimal places
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )