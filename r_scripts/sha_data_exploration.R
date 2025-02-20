

                         # Mediterranean Sea 
                         # data exploration

# packages:
library(dplyr)
library(ggplot2)

# read data:
data_cleaned <- read.csv("./data/raw/shahar/data_cleaned.csv")
Readme <- read.csv("./data/raw/shahar/Readme.csv")
sp_habitat <- read.csv("./data/raw/shahar/sp_habitat_research.csv")

# filter fish species
data_fish <- data_cleaned %>% filter(subgroup == "fish") # 742 depth distribution observations
                                                         # 162 species of fish
                                                         # min and max depth

# Shahars' depth data overview:
# literature review on the depth ranges o3 taxonomic groups across the Mediterranean sea
 # "mollusca (cephalopoda)"  "crustacea (malacostraca)" "chordata (fish)" 
  # variables: max depth and min depth of distribution per species across multiple populations 
   # data on the survey location, environmental variables (tem + salinity) at each location
    # species traits:habitat along the water coloumn, thermail affinity, depth affinity, ,in\max depth from litrature 
