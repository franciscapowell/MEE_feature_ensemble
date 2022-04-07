
if(!require(caret)){
  install.packages('caret')
}

if(!require(dplyr)){
  install.packages('dplyr')
}

if(!require(gbm)){
  install.packages('gbm')
}

if(!require(mgcv)){
  install.packages('mgcv')
}

if(!require(purrr)){
  install.packages('purrr')
}

if(!require(randomForestSRC)){
  install.packages('randomForestSRC')
}

if(!require(ggplot2)){
  install.packages('ggplot2')
}

if(!require(Hmsc)){
  install.packages('Hmsc')
}

if(!require(parallel)){
  install.packages('parallel')
}

if(!require(MRFcov)){
  install.packages('MRFcov')
}


setwd("C:/Users/uqfpowe1/OneDrive - The University of Queensland/Documents/PhD/Writing Documents/Data Structure Study/MEE_feature_ensemble")

source('functions/stacking_function.R')
source('functions/workflow_function.R')


# Global parameters for setting random seeds and the cross-validation split proportion
seeds <- c(15, 10, 5)
split_prop <- 0.7

# 2.  helminths ####
load("data/datasets_for_analysis/helminths_ys.rda")
load("data/datasets_for_analysis/helminths_xs.rda")

all_dat <- cbind(helminths_ys, helminths_xs)

ys <- all_dat[1:4]
xs <- all_dat[5:9]

helminths_metrics = model_weights_function(xs, ys, all_dat)

save(helminths_metrics, file = "outputs/training_data_outputs/helminths_metrics.rda")

rm(helminths_xs, helminths_ys, helminths_metrics)

# 3.  fennoscandia_birds ####
load("data/datasets_for_analysis/fennoscandia_birds_ys.rda")
load("data/datasets_for_analysis/fennoscandia_birds_xs.rda")

all_dat <- cbind(fennoscandia_birds_ys, fennoscandia_birds_xs)

ys <- all_dat[1:141]
xs <- all_dat[142:146]

fennoscandia_birds_metrics = model_weights_function(xs, ys, all_dat)

save(fennoscandia_birds_metrics, file = "outputs/training_data_outputs/fennoscandia_birds_metrics.rda")

rm(fennoscandia_birds_xs, fennoscandia_birds_ys, fennoscandia_birds_metrics)

# 5.  victoria_plants ####

load("data/datasets_for_analysis/victoria_plants_ys.rda")
load("data/datasets_for_analysis/victoria_plants_xs.rda")

all_dat <- cbind(victoria_plants_ys, victoria_plants_xs)

ys <- all_dat[1:162]
xs <- all_dat[163:167]

victoria_plants_metrics = model_weights_function(xs, ys, all_dat)

save(victoria_plants_metrics, file = "outputs/training_data_outputs/victoria_plants_metrics.rda")

rm(victoria_plants_xs, victoria_plants_ys, victoria_plants_metrics)

# 6.  usa_trees ####

load("data/datasets_for_analysis/usa_trees_ys.rda")
load("data/datasets_for_analysis/usa_trees_xs.rda")

all_dat <- cbind(usa_trees_ys, usa_trees_xs)

ys <- all_dat[1:63]
xs <- all_dat[64:66]

usa_trees_metrics = model_weights_function(xs, ys, all_dat)

save(usa_trees_metrics, file = "outputs/training_data_outputs/usa_trees_metrics.rda")

rm(usa_trees_xs, usa_trees_ys, usa_trees_metrics)

# 8.  eelgrass ####

load("data/datasets_for_analysis/eelgrass_ys.rda")
load("data/datasets_for_analysis/eelgrass_xs.rda")

all_dat <- cbind(eelgrass_ys, eelgrass_xs)

ys <- all_dat[1:33]
xs <- all_dat[34:38]

eelgrass_metrics = model_weights_function(xs, ys, all_dat)

save(eelgrass_metrics, file = "outputs/training_data_outputs/eelgrass_metrics.rda")

rm(eelgrass_xs, eelgrass_ys, eelgrass_metrics)

# 9.  shrews ####

load("data/datasets_for_analysis/shrews_ys.rda")
load("data/datasets_for_analysis/shrews_xs.rda")

all_dat <- cbind(shrews_ys, shrews_xs)

ys <- all_dat[1:7]
xs <- all_dat[8:10]

shrews_metrics = model_weights_function(xs, ys, all_dat)

save(shrews_metrics, file = "outputs/training_data_outputs/shrews_metrics.rda")

rm(shrews_xs, shrews_ys, shrews_metrics)

# 10. mussel parasites ####

load("data/datasets_for_analysis/mussel_parasites_ys.rda")
load("data/datasets_for_analysis/mussel_parasites_xs.rda")

all_dat <- cbind(mussel_parasites_ys, mussel_parasites_xs)

ys <- all_dat[1:14]
xs <- all_dat[15:18]

mussel_parasites_metrics = model_weights_function(xs, ys, all_dat)

save(mussel_parasites_metrics, file = "outputs/training_data_outputs/mussel_parasites_metrics.rda")

rm(mussel_parasites_xs, mussel_parasites_ys, mussel_parasites_metrics)

# 11. lion_infections ####

load("data/datasets_for_analysis/lion_infections_ys.rda")
load("data/datasets_for_analysis/lion_infections_xs.rda")

all_dat <- cbind(lion_infections_ys, lion_infections_xs)

ys <- all_dat[1:5]
xs <- all_dat[6:10]

lion_infections_metrics = model_weights_function(xs, ys, all_dat)

save(lion_infections_metrics, file = "outputs/training_data_outputs/lion_infections_metrics.rda")

rm(lion_infections_xs, lion_infections_ys, lion_infections_metrics)

# 12. eucalyptus ####

load("data/datasets_for_analysis/eucalyptus_ys.rda")
load("data/datasets_for_analysis/eucalyptus_xs.rda")

all_dat <- cbind(eucalyptus_ys, eucalyptus_xs)

ys <- all_dat[1:20]
xs <- all_dat[21:25]

eucalyptus_metrics = model_weights_function(xs, ys, all_dat)

save(eucalyptus_metrics, file = "outputs/training_data_outputs/eucalyptus_metrics.rda")

rm(eucalyptus_xs, eucalyptus_ys, eucalyptus_metrics)

# 15. usa_birds ####

load("data/datasets_for_analysis/usa_birds_ys.rda")
load("data/datasets_for_analysis/usa_birds_xs.rda")

all_dat <- cbind(usa_birds_ys, usa_birds_xs)

ys <- all_dat[1:102]
xs <- all_dat[103:107]

usa_birds_metrics = model_weights_function(xs, ys, all_dat)

save(usa_birds_metrics, file = "outputs/training_data_outputs/usa_birds_metrics.rda")

rm(usa_birds_xs, usa_birds_ys, usa_birds_metrics)

# 17. swiss_forest #### 

load("data/datasets_for_analysis/swiss_forest_ys.rda")
load("data/datasets_for_analysis/swiss_forest_xs.rda")

all_dat <- cbind(swiss_forest_ys, swiss_forest_xs)

ys <- all_dat[1:63]
xs <- all_dat[64:68]

swiss_forest_metrics = model_weights_function(xs, ys, all_dat)

save(swiss_forest_metrics, file = "outputs/training_data_outputs/swiss_forest_metrics.rda")

rm(swiss_forest_xs, swiss_forest_ys, swiss_forest_metrics)

# 18.  fish_parasites ####

load("data/datasets_for_analysis/fish_parasites_ys.rda")
load("data/datasets_for_analysis/fish_parasites_xs.rda")

all_dat <- cbind(fish_parasites_ys, fish_parasites_xs)

ys <- all_dat[1:42]
xs <- all_dat[43:47]

fish_parasites_metrics = model_weights_function(xs, ys, all_dat)

save(fish_parasites_metrics, file = "outputs/training_data_outputs/fish_parasites_metrics.rda")

rm(fish_parasites_xs, fish_parasites_ys, fish_parasites_metrics)

# 19. brazil_fish ####

load("data/datasets_for_analysis/brazil_fish_ys.rda")
load("data/datasets_for_analysis/brazil_fish_xs.rda")

all_dat <- cbind(brazil_fish_ys, brazil_fish_xs)

ys <- all_dat[1:92]
xs <- all_dat[93:97]

brazil_fish_metrics = model_weights_function(xs, ys, all_dat)

save(brazil_fish_metrics, file = "outputs/training_data_outputs/brazil_fish_metrics.rda")

rm(brazil_fish_xs, brazil_fish_ys, brazil_fish_metrics)

# 20. reptiles ####

load("data/datasets_for_analysis/reptiles_ys.rda")
load("data/datasets_for_analysis/reptiles_xs.rda")

all_dat <- cbind(reptiles_ys, reptiles_xs)

ys <- all_dat[1:104]
xs <- all_dat[105:109]

reptiles_metrics = model_weights_function(xs, ys, all_dat)

save(reptiles_metrics, file = "outputs/training_data_outputs/reptiles_metrics.rda")

rm(reptiles_xs, reptiles_ys, reptiles_metrics)

# 21. canopy_ants ####
load("data/datasets_for_analysis/canopy_ants_ys.rda")
load("data/datasets_for_analysis/canopy_ants_xs.rda")

all_dat <- cbind(canopy_ants_ys, canopy_ants_xs)

ys <- all_dat[1:128]
xs <- all_dat[129:132]

canopy_ants_metrics = model_weights_function(xs, ys, all_dat)

save(canopy_ants_metrics, file = "outputs/training_data_outputs/canopy_ants_metrics.rda")

rm(canopy_ants_xs, canopy_ants_ys, canopy_ants_metrics)

# 22. swissalps_plants ####

load("data/datasets_for_analysis/swissalps_plants_ys.rda")
load("data/datasets_for_analysis/swissalps_plants_xs.rda")

all_dat <- cbind(swissalps_plants_ys, swissalps_plants_xs)

ys <- all_dat[1:175]
xs <- all_dat[176:179]

swissalps_plants_metrics = model_weights_function(xs, ys, all_dat)

save(swissalps_plants_metrics, file = "outputs/training_data_outputs/swissalps_plants_metrics.rda")

rm(swissalps_plants_xs, swissalps_plants_ys, swissalps_plants_metrics)

# 25. buffalo_infections ####
load("data/datasets_for_analysis/buffalo_infections_ys.rda")
load("data/datasets_for_analysis/buffalo_infections_xs.rda")

all_dat <- cbind(buffalo_infections_ys, buffalo_infections_xs)

ys <- all_dat[1:6]
xs <- all_dat[7:11]

buffalo_infections_metrics = model_weights_function(xs, ys, all_dat)

save(buffalo_infections_metrics, file = "outputs/training_data_outputs/buffalo_infections_metrics.rda")

rm(buffalo_infections_xs, buffalo_infections_ys, buffalo_infections_metrics)

# 27. finland_beetles ####

load("data/datasets_for_analysis/finland_beetles_ys.rda")
load("data/datasets_for_analysis/finland_beetles_xs.rda")

all_dat <- cbind(finland_beetles_ys, finland_beetles_xs)

ys <- all_dat[1:241]
xs <- all_dat[242:246]

finland_beetles_metrics = model_weights_function(xs, ys, all_dat)

save(finland_beetles_metrics, file = "outputs/training_data_outputs/finland_beetles_metrics.rda")

rm(finland_beetles_xs, finland_beetles_ys, finland_beetles_metrics)

# 28. germany_beetles ####

load("data/datasets_for_analysis/germany_beetles_ys.rda")
load("data/datasets_for_analysis/germany_beetles_xs.rda")

all_dat <- cbind(germany_beetles_ys, germany_beetles_xs)

ys <- all_dat[1:75]
xs <- all_dat[76:80]

germany_beetles_metrics = model_weights_function(xs, ys, all_dat)

save(germany_beetles_metrics, file = "outputs/training_data_outputs/germany_beetles_metrics.rda")

rm(germany_beetles_xs, germany_beetles_ys, germany_beetles_metrics)

# 30. nz_forest ####

load("data/datasets_for_analysis/nz_forest_ys.rda")
load("data/datasets_for_analysis/nz_forest_xs.rda")

all_dat <- cbind(nz_forest_ys, nz_forest_xs)

ys <- all_dat[1:205]
xs <- all_dat[206:207]

nz_forest_metrics = model_weights_function(xs, ys, all_dat)

save(nz_forest_metrics, file = "outputs/training_data_outputs/nz_forest_metrics.rda")

rm(nz_forest_xs, nz_forest_ys, nz_forest_metrics)
