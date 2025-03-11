

          # Depth preferences per decade for different species

#______________________________________________________________

#data wrangling:
library(ggplot2)
library(dplyr)
library(tidyr)

#models:
library(eHOF)
library(senlm)

# read:
north_sea <- read.csv("./data/raw/north_sea/NorthSea_Quarter1_Abund_1983_2024.csv")
#______________________________________________________________

#list wds
wd_first_models <- 'C:/Users/inbar/OneDrive/desktop/r/taiwan/tai_isr_collaboration/tai_isr_collaboration/outputs/Depth_senlm_first'

                         # Preparing the data

# add a column for the stripe group:
north_sea <- north_sea %>% mutate( 
    lat_stripes = case_when(
    ShootLat < 54 ~ "under_54",
    ShootLat >= 54 & ShootLat <= 57 ~ "between_54_57",
    ShootLat > 57 ~ "above_57",
    TRUE ~ NA_character_  # Assign NA if needed
  )
)
# - - - - - - - - -

# add a "time period" column
north_sea <- north_sea %>% mutate( 
  time_periods = case_when(
  Year %in% 1983:1993 ~ "1983_1993",
  Year %in% 1994:2003 ~ "1994_2003",
  Year %in% 2004:2013 ~ "2004_2013",
  Year %in% 2014:2024 ~ "2014_2024",
  TRUE ~ NA_character_  
))
# - - - - - - - - -

# Summarizing the abundance for each depth strip and decade per species:
north_sea_groups <- north_sea %>% 
  group_by(Sci_name, lat_stripes, time_periods, Depth) %>% 
  summarise(abundance = sum(TotalNo, na.rm = TRUE), .groups = "drop") %>%
  mutate(abundance = as.integer(abundance)) %>%
  relocate(abundance, .after = Sci_name) %>%
  mutate(group = paste(lat_stripes, time_periods, sep = "_"))

# - - - - - - - - -

# list of all groups:
groups <- north_sea_groups %>% pull(group) %>% unique() %>% as.list %>% purrr::set_names(.)

# - - - - - - - - -

# ~ loop - creating subset groups, comprising of the combination of years and latitude stripe:
subsets_gorups <- purrr::map(groups,
                             function(group) {
                               north_sea_groups %>%
                                 dplyr::filter(group == group)
                             })

#______________________________________________________________

                          # senlm package 

# - - -

                  ### - one model for all species:

# 1 - fit the model - loop:

#create empty list to save results for each group
gaus_zinb_depth <- list()

for(i in 1:length(subsets_gorups))
{
  # pull one data set:
  sub_gr <- subsets_gorups[[i]]
  
  # pull the list of unique species in this particular data set:
  sps_list <- unique(sub_gr$Sci_name)
  
  #create empty list to save results for each species in the group
  sps_res <- list()
  
  for(j in 1:length(sps_list))
  {
    # pull one species data:
    sub_gr_sps <- sub_gr[sub_gr$Sci_name == sps_list[j],]
 
    # fit the Gaussian model for the abundance\depth of all species - skip species with insufficient data:
    sps_res[[j]] <- try(senlm(data = as.data.frame(sub_gr_sps),
                        xvar = "Depth", yvar = "abundance",
                        mean_fun="gaussian", err_dist="zinb",
                        conf.level=0.95), silent = T)
    
    # print the group and species id:
    print(paste0(i,'_',j))
  }
   
  # add the names of the species to the models results:
  names(sps_res) <- sps_list
  
  # add the models result to the list of groups:
  gaus_zinb_depth[[i]] <- sps_res
}

# add the name of the groups to the results:
names(gaus_zinb_depth) <- names(subsets_gorups)

# only Jesus saves your models
setwd(wd_first_models)
saveRDS(gaus_zinb_depth, 'gaus_zinb_depth')

# - - - - - - - - -

#create color because I am too cool for HEX code
mygrey <- grey(level = 0.65, alpha = 0.4)

#create a list of lists for species that worked in each group
sps_working <- list()

# 2 - visualize:
for(i in 1:length(gaus_zinb_depth))
{

  # pull the list of unique species in this particular data set:
  sps_n <- length(gaus_zinb_depth[[i]])
  
  # create a list for species that worked in this group
  sps_working[[i]] <- character()
  
  for(j in 1:sps_n)
  {
    
    #make an if and else condition to plot only the good shit
    if(class(gaus_zinb_depth[[i]][[j]]) == "senlm"){
      
      #plot ugly
      predict.x <- seq(from=min(gaus_zinb_depth[[i]][[j]]$x),
                       to = max(gaus_zinb_depth[[i]][[j]]$x),
                       length.out = length(gaus_zinb_depth[[i]][[j]]$x))
      
      fitted_values <- predict(gaus_zinb_depth[[i]][[j]], predict.x)
      plot(gaus_zinb_depth[[i]][[j]]$x,
           gaus_zinb_depth[[i]][[j]]$y,
           ylim = c(min(fitted_values), max(fitted_values)),
           xlim = c(0,300),
           pch = 21, col = mygrey, bg = mygrey,
           main = names(gaus_zinb_depth[[i]])[j],
           ylab="Abundance", xlab = "Depth")
      
      lines(predict.x, fitted_values, col = "#228875", lwd = 2) 
      
      sps_working[[i]][length(sps_working[[i]]) + 1] <- names(gaus_zinb_depth[[i]])[j]
    }
    # print the group and species id:
    print(paste0(i,'_',j))
  }
}

# - - - - - - - - -

#name groups in sps that worked list of lists
names(sps_working) <- names(gaus_zinb_depth)

# - - - - - - - - -

#get a list of all species in the North
sps_list_north <- unique(north_sea$Sci_name)

# - - - - - - - - -

#check which species are in each group
sps_all_group <- sps_list_north[sps_list_north %in% sps_working[[1]] &
                                sps_list_north %in% sps_working[[2]] &
                                sps_list_north %in% sps_working[[3]] &
                                sps_list_north %in% sps_working[[4]] &
                                sps_list_north %in% sps_working[[5]] &
                                sps_list_north %in% sps_working[[6]] &
                                sps_list_north %in% sps_working[[7]] &
                                sps_list_north %in% sps_working[[8]] &
                                sps_list_north %in% sps_working[[9]] &
                                sps_list_north %in% sps_working[[10]] &
                                sps_list_north %in% sps_working[[11]] &
                                sps_list_north %in% sps_working[[12]] ]

# - - - - - - - - -

# check which species intersect:
sps_intersect <- Reduce(intersect, sps_working)

# check how many intersect
length(sps_intersect) # 144 out of 188

# - - - - - - - - -

# if I want to filter only the species that appear in all the groups I need to 
# write a loop that keeps the model if the species name is in the sp_intersect
# list
#______________________________________________________________

  # extract the m and H values from each species model from each group

# Load required packages
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# Convert the list of models into a structured data frame
results_df <- map_dfr(names(gaus_zinb_depth), function(group_name) {
  
  # Extract lat_stripes and time_periods from the group name
  parts <- str_split(group_name, "_", simplify = TRUE)
  lat_stripes <- paste(parts[1], parts[2], sep = "_")  # e.g., "above_57"
  time_periods <- paste(parts[3], parts[4], sep = "_")  # e.g., "1983_1993"
  
  # Get the species models for this group
  species_models <- gaus_zinb_depth[[group_name]]
  
  # Extract results for each species
  map_dfr(names(species_models), function(species_name) {
    
    model <- species_models[[species_name]]
    
    # Check if the model ran successfully (avoid errors)
    if (inherits(model, "try-error")) {
      return(tibble(Group = group_name, lat_stripes = lat_stripes, time_periods = time_periods, 
                    Species = species_name, m = NA, H = NA))
    }
    
    tibble(
      Group = group_name,
      lat_stripes = lat_stripes,
      time_periods = time_periods,
      Species = species_name,
      Model = model$model,
      m = model$theta["m"],
      H = model$theta["H"]
    )
  })
})

# View the final data frame
print(results_df)

# - - - - - - - - -

# remove unsuccessful models:
results_drop_na <- drop_na(results_df)

# - - - - - - - - -

# count of groups per species - all of them apper in all years
record_s_g_count <- results_drop_na %>% group_by(Species) %>% summarise(n = n())

#______________________________________________________________














































                  ### - multiple models:

# 1
# setting models to fit - count models (discreet data):
# - all models for all species (among them one will be chosen)
count_models <- set_models (mean_class = "main", err_class= "count", method = "crossed")

Pars_c <- create_default_par_list (count_models)

# - - - - - - - - -

# 2 - define and fit the models:
#create empty list to save results for each group
models_depth <- list()

for(i in 1:length(subsets_gorups))
{
  # pull one data set:
  sub_gr <- subsets_gorups[[i]]
  
  # pull the list of unique species in this particular data set:
  sps_list <- unique(sub_gr$Sci_name)
  
  #create empty list to save results for each species in the group
  sps_res <- list()
  
  for(j in 1:length(sps_list))
  {
    # pull one species data:
    sub_gr_sps <- sub_gr[sub_gr$Sci_name == sps_list[j],]
    
    # fit all combinations of models to each species from each group:
    sps_res <- msenlm(count_models, data = as.data.frame(sub_gr_sps),
                                        xvar = "Depth", yvar = "abundance",
                                        conf.level=0.95)
    
    # print the group and species id:
    print(paste0(i,'_',j))
  }
  
  # add the names of the species to the models results:
  names(sps_res) <- sps_list
  
  # add the models result to the list of groups:
  gaus_zinb_depth[[i]] <- sps_res
}

# add the name of the groups to the results:
names(gaus_zinb_depth) <- names(subsets_gorups)

# - - - - - - - - -

# # 3 - Create a list of the top abundance models (based on AIC values) for each taxon:
# best_models_a <- purrr::map(taxas, ~ {
#   taxa_models <- fitted_models_abu[[.x]]$abundance
#   aic_values <- purrr::map_dbl(taxa_models, ~.x$IC["AIC"])
#   names(aic_values) <- names(taxa_models)
#   ordered_aic_values <- sort(aic_values)
#   head(ordered_aic_values)
# })
# 
# # save the models output:
# setwd(wd_processed_data)
# 
# write.csv(best_models_a, file = "best_models_a.csv")
# # 4 - Extract the best model for each taxon:
# model_summaries_a <- purrr::map(taxas, ~ {
#   taxa_data <- fitted_models_abu[[.x]]
#   summary(msenlm.best(taxa_data, best="AICc"))
# })
# 





