

         # connecting richness variation to depth range shifts

# packages:
library(dplyr)
library(ggplot2)

# read data:
north_sea <- read.csv("./data/raw/north_sea/NorthSea_2000_2020.csv")


# number of samples per species per year
sp_year <- north_sea %>% select(Year, Sci_name) %>% group_by(Year, Sci_name) %>%
  summarise(n = n())


# number of years per species
sp_n_year <- sp_year %>% group_by(Sci_name) %>% summarise(n_years = n())


# number of species per number of years
y_count <- sp_n_year %>% group_by(n_years) %>% summarise(n = n())

# ________________________________________________________________

# number of samples per species facet by year



ggplot(sp_year, aes(x = Sci_name, y = n, color = Year)) +
  geom_point() +
  labs(
    title = "Samples (n) per species",
    x = "Species",
    y = "samples count"
  )  + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )+ facet_wrap(~ Year)


#
## Define a new variable (first_p , middle_p, last_p) - corresponding to the years
## check in how many periods each species was present and how many samples in has within each period
## - plot and sum for each species how many samples it had at each period
## - chose a threshold that is enough for the calculations of the depth ranges