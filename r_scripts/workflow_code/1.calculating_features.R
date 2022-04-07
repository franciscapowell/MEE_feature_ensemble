

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

setwd("C:/.../MEE_feature_ensemble")


source('Functions/feature_function.R')
source('Functions/feature_function_without_PCA.R')

## 1. bird_parasites ####

load("data/datasets_for_analysis/bird_parasites_ys.rda")
load("data/data_prep/bird_parasites_prep_xs.rda")

ys <- bird_parasites_ys
xs <- bird_parasites_prep_xs

bird_parasites_fax = feature_function(ys, xs)

bird_parasites_features <- bird_parasites_fax$features_dat
bird_parasites_xs <- bird_parasites_fax$covariates
bird_parasites_xs <- data.frame(bird_parasites_xs)

save(bird_parasites_features, file = "outputs/features/bird_parasites_features.rda")
save(bird_parasites_xs, file = "data/datasets_for_analysis/bird_parasites_xs.rda")

## 2. helminths ####

load("data/datasets_for_analysis/helminths_ys.rda")
load("data/data_prep/helminths_prep_xs.rda")

ys <- helminths_ys
xs <- helminths_prep_xs

helminths_fax = feature_function(ys, xs)

helminths_features <- helminths_fax$features_dat
helminths_xs <- helminths_fax$covariates
helminths_xs <- data.frame(helminths_xs)


save(helminths_features, file = "outputs/features/helminths_features.rda")
save(helminths_xs, file = "data/datasets_for_analysis/helminths_xs.rda")

## 3. fennoscandia_birds ####

load("data/datasets_for_analysis/fennoscandia_birds_ys.rda")
load("data/data_prep/fennoscandia_birds_prep_xs.rda")

ys <- fennoscandia_birds_ys
xs <- fennoscandia_birds_prep_xs

fennoscandia_birds_fax = feature_function_without_PCA(ys, xs)

fennoscandia_birds_features <- fennoscandia_birds_fax$features_dat
fennoscandia_birds_features$no_preds <- 21
fennoscandia_birds_features$var_exp <- 0.56
fennoscandia_birds_xs <- fennoscandia_birds_fax$covariates
fennoscandia_birds_xs <- data.frame(fennoscandia_birds_xs)

save(fennoscandia_birds_features, file = "outputs/features/fennoscandia_birds_features.rda")
save(fennoscandia_birds_xs, file = "data/datasets_for_analysis/fennoscandia_birds_xs.rda")

## 4. uk_butterflies####

load("data/datasets_for_analysis/uk_butterflies_ys.rda")
load("data/data_prep/uk_butterflies_prep_xs.rda")

ys <- uk_butterflies_ys
xs <- uk_butterflies_prep_xs

ys <- ys %>% 
  select(-uk_butterfly_1, -uk_butterfly_29, -uk_butterfly_38)

uk_butterflies_fax = feature_function_without_PCA(ys, xs)

uk_butterflies_features <- uk_butterflies_fax$features_dat
uk_butterflies_features$no_preds <- 34
uk_butterflies_features$var_exp <- 0.46
uk_butterflies_xs <- uk_butterflies_fax$covariates
uk_butterflies_xs <- data.frame(uk_butterflies_xs)


save(uk_butterflies_features, file = "outputs/features/uk_butterflies_features.rda")
save(uk_butterflies_xs, file = "data/datasets_for_analysis/uk_butterflies_xs.rda")

## 5. victoria_plants ####

load("data/datasets_for_analysis/victoria_plants_ys.rda")
load("data/data_prep/victoria_plants_prep_xs.rda")

ys <- victoria_plants_ys
xs <- victoria_plants_prep_xs

victoria_plants_fax = feature_function_without_PCA(ys, xs)

victoria_plants_features <- victoria_plants_fax$features_dat
victoria_plants_features$no_preds <- 19
victoria_plants_features$var_exp <- 0.78
victoria_plants_xs <- victoria_plants_fax$covariates
victoria_plants_xs <- data.frame(victoria_plants_xs)


save(victoria_plants_features, file = "outputs/features/victoria_plants_features.rda")
save(victoria_plants_xs, file = "data/datasets_for_analysis/victoria_plants_xs.rda")


## 6. usa_trees ####

load("data/datasets_for_analysis/usa_trees_ys.rda")
load("data/data_prep/usa_trees_prep_xs.rda")

ys <- usa_trees_ys
xs <- usa_trees_prep_xs

usa_trees_fax = feature_function_without_PCA(ys, xs)

usa_trees_features <- usa_trees_fax$features_dat
usa_trees_features$no_preds <- 38
usa_trees_features$var_exp <- 0.83
usa_trees_xs <- usa_trees_fax$covariates
usa_trees_xs <- data.frame(usa_trees_xs)

save(usa_trees_features, file = "outputs/features/usa_trees_features.rda")
save(usa_trees_xs, file = "data/datasets_for_analysis/usa_trees_xs.rda")

## 7. norway_vegetation ####

load("data/datasets_for_analysis/norway_vegetation_ys.rda")
load("data/data_prep/norway_vegetation_prep_xs.rda")

ys <- norway_vegetation_ys
xs <- norway_vegetation_prep_xs

norway_vegetation_fax = feature_function_without_PCA(ys, xs)

norway_vegetation_features <- norway_vegetation_fax$features_dat
norway_vegetation_features$no_preds <- 6
norway_vegetation_features$var_exp <- 0.88
norway_vegetation_xs <- norway_vegetation_fax$covariates
norway_vegetation_xs <- data.frame(norway_vegetation_xs)

save(norway_vegetation_features, file = "outputs/features/norway_vegetation_features.rda")
save(norway_vegetation_xs, file = "data/datasets_for_analysis/norway_vegetation_xs.rda")

## 8. eelgrass #### 

load("data/datasets_for_analysis/eelgrass_ys.rda")
load("data/data_prep/eelgrass_prep_xs.rda")

ys <- eelgrass_ys
xs <- eelgrass_prep_xs

ys <- ys %>% 
  select(-eelgrass_5)

eelgrass_fax = feature_function(ys, xs)

eelgrass_features <- eelgrass_fax$features_dat
eelgrass_xs <- eelgrass_fax$covariates
eelgrass_xs <- data.frame(eelgrass_xs)

save(eelgrass_features, file = "outputs/features/eelgrass_features.rda")
save(eelgrass_xs, file = "data/datasets_for_analysis/eelgrass_xs.rda")

## 9. shrews #### 

load("data/datasets_for_analysis/shrews_ys.rda")
load("data/data_prep/shrews_prep_xs.rda")

ys <- shrews_ys
xs <- shrews_prep_xs

shrews_fax = feature_function(ys, xs)

shrews_features <- shrews_fax$features_dat
shrews_xs <- shrews_fax$covariates
shrews_xs <- data.frame(shrews_xs)

save(shrews_features, file = "outputs/features/shrews_features.rda")
save(shrews_xs, file = "data/datasets_for_analysis/shrews_xs.rda")


## 10. mussel_parasites #### 

load("data/datasets_for_analysis/mussel_parasites_ys.rda")
load("data/data_prep/mussel_parasites_prep_xs.rda")

ys <- mussel_parasites_ys
xs <- mussel_parasites_prep_xs

ys <- ys %>% 
  select(-mussel_parasite_1)

mussel_parasites_fax = feature_function(ys, xs)

mussel_parasites_features <- mussel_parasites_fax$features_dat
mussel_parasites_xs <- mussel_parasites_fax$covariates
mussel_parasites_xs <- data.frame(mussel_parasites_xs)

save(mussel_parasites_features, file = "outputs/features/mussel_parasites_features.rda")
save(mussel_parasites_xs, file = "data/datasets_for_analysis/mussel_parasites_xs.rda")

## 11. lion_infections #### 

load("data/datasets_for_analysis/lion_infections_ys.rda")
load("data/data_prep/lion_infections_prep_xs.rda")

ys <- lion_infections_ys
xs <- lion_infections_prep_xs

lion_infections_fax = feature_function(ys, xs)

lion_infections_features <- lion_infections_fax$features_dat
lion_infections_xs <- lion_infections_fax$covariates
lion_infections_xs <- data.frame(lion_infections_xs)

save(lion_infections_features, file = "outputs/features/lion_infections_features.rda")
save(lion_infections_xs, file = "data/datasets_for_analysis/lion_infections_xs.rda")

## 12. eucalyptus #### 

load("data/datasets_for_analysis/eucalyptus_ys.rda")
load("data/data_prep/eucalyptus_prep_xs.rda")

ys <- eucalyptus_ys
xs <- eucalyptus_prep_xs

eucalyptus_fax = feature_function(ys, xs)

eucalyptus_features <- eucalyptus_fax$features_dat
eucalyptus_xs <- eucalyptus_fax$covariates
eucalyptus_xs <- data.frame(eucalyptus_xs)

save(eucalyptus_features, file = "outputs/features/eucalyptus_features.rda")
save(eucalyptus_xs, file = "data/datasets_for_analysis/eucalyptus_xs.rda")

## 13. grassland_birds #### 

load("data/datasets_for_analysis/grassland_birds_ys.rda")
load("data/data_prep/grassland_birds_prep_xs.rda")

ys <- grassland_birds_ys
xs <- grassland_birds_prep_xs

grassland_birds_fax = feature_function(ys, xs)

grassland_birds_features <- grassland_birds_fax$features_dat
grassland_birds_xs <- grassland_birds_fax$covariates
grassland_birds_xs <- data.frame(grassland_birds_xs)

save(grassland_birds_features, file = "outputs/features/grassland_birds_features.rda")
save(grassland_birds_xs, file = "data/datasets_for_analysis/grassland_birds_xs.rda")

## 14. mulu_birds #### 

load("data/datasets_for_analysis/mulu_birds_ys.rda")
load("data/data_prep/mulu_birds_prep_xs.rda")

ys <- mulu_birds_ys
xs <- mulu_birds_prep_xs

mulu_birds_fax = feature_function(ys, xs)

mulu_birds_features <- mulu_birds_fax$features_dat
mulu_birds_xs <- mulu_birds_fax$covariates
mulu_birds_xs <- data.frame(mulu_birds_xs)

save(mulu_birds_features, file = "outputs/features/mulu_birds_features.rda")
save(mulu_birds_xs, file = "data/datasets_for_analysis/mulu_birds_xs.rda")

## 15. usa_birds ####

load("data/datasets_for_analysis/usa_birds_ys.rda")
load("data/data_prep/usa_birds_prep_xs.rda")

ys <- usa_birds_ys
xs <- usa_birds_prep_xs

ys <- ys %>% 
  select(-usa_birds_87)

usa_birds_fax = feature_function(ys, xs)

usa_birds_features <- usa_birds_fax$features_dat
usa_birds_xs <- usa_birds_fax$covariates
usa_birds_xs <- data.frame(usa_birds_xs)

save(usa_birds_features, file = "outputs/features/usa_birds_features.rda")
save(usa_birds_xs, file = "data/datasets_for_analysis/usa_birds_xs.rda")

## 16. swiss_birds ####

load("data/datasets_for_analysis/swiss_birds_ys.rda")
load("data/data_prep/swiss_birds_prep_xs.rda")

ys <- swiss_birds_ys
xs <- swiss_birds_prep_xs

swiss_birds_fax = feature_function(ys, xs)

swiss_birds_features <- swiss_birds_fax$features_dat
swiss_birds_xs <- swiss_birds_fax$covariates
swiss_birds_xs <- data.frame(swiss_birds_xs)

save(swiss_birds_features, file = "outputs/features/swiss_birds_features.rda")
save(swiss_birds_xs, file = "data/datasets_for_analysis/swiss_birds_xs.rda")

## 17. swiss_forest ####

load("data/datasets_for_analysis/swiss_forest_ys.rda")
load("data/data_prep/swiss_forest_prep_xs.rda")

ys <- swiss_forest_ys
xs <- swiss_forest_prep_xs

swiss_forest_fax = feature_function(ys, xs)

swiss_forest_features <- swiss_forest_fax$features_dat
swiss_forest_xs <- swiss_forest_fax$covariates
swiss_forest_xs <- data.frame(swiss_forest_xs)

save(swiss_forest_features, file = "outputs/features/swiss_forest_features.rda")
save(swiss_forest_xs, file = "data/datasets_for_analysis/swiss_forest_xs.rda")

## 18. fish_parasites ####

load("data/datasets_for_analysis/fish_parasites_ys.rda")
load("data/data_prep/fish_parasites_prep_xs.rda")

ys <- fish_parasites_ys
xs <- fish_parasites_prep_xs

fish_parasites_fax = feature_function(ys, xs)

fish_parasites_features <- fish_parasites_fax$features_dat
fish_parasites_xs <- fish_parasites_fax$covariates
fish_parasites_xs <- data.frame(fish_parasites_xs)

save(fish_parasites_features, file = "outputs/features/fish_parasites_features.rda")
save(fish_parasites_xs, file = "data/datasets_for_analysis/fish_parasites_xs.rda")

## 19. brazil_fish ####

load("data/datasets_for_analysis/brazil_fish_ys.rda")
load("data/data_prep/brazil_fish_prep_xs.rda")

ys <- brazil_fish_ys
xs <- brazil_fish_prep_xs

ys <- ys %>% 
  select(-brazil_fish_3, -brazil_fish_10, -brazil_fish_15, -brazil_fish_16, -brazil_fish_30,
         -brazil_fish_32, -brazil_fish_34, -brazil_fish_35, -brazil_fish_40, -brazil_fish_46,
         -brazil_fish_48, -brazil_fish_49, -brazil_fish_51, -brazil_fish_52, -brazil_fish_56,
         -brazil_fish_58, -brazil_fish_61, -brazil_fish_67, -brazil_fish_68, -brazil_fish_70,
         -brazil_fish_71, -brazil_fish_77, -brazil_fish_81, -brazil_fish_84, -brazil_fish_85,
         -brazil_fish_87)

brazil_fish_fax = feature_function(ys, xs)

brazil_fish_features <- brazil_fish_fax$features_dat
brazil_fish_xs <- brazil_fish_fax$covariates
brazil_fish_xs <- data.frame(brazil_fish_xs)

save(brazil_fish_features, file = "outputs/features/brazil_fish_features.rda")
save(brazil_fish_xs, file = "data/datasets_for_analysis/brazil_fish_xs.rda")

## 20. reptiles ####

load("data/datasets_for_analysis/reptiles_ys.rda")
load("data/data_prep/reptiles_prep_xs.rda")

ys <- reptiles_ys
xs <- reptiles_prep_xs

reptiles_fax = feature_function(ys, xs)

reptiles_features <- reptiles_fax$features_dat
reptiles_xs <- reptiles_fax$covariates
reptiles_xs <- data.frame(reptiles_xs)

save(reptiles_features, file = "outputs/features/reptiles_features.rda")
save(reptiles_xs, file = "data/datasets_for_analysis/reptiles_xs.rda")

## 21. canopy_ants ####

load("data/datasets_for_analysis/canopy_ants_ys.rda")
load("data/data_prep/canopy_ants_prep_xs.rda")

ys <- canopy_ants_ys
xs <- canopy_ants_prep_xs

ys <- ys %>% 
  select(-canopy_ant_3, -canopy_ant_12, -canopy_ant_19, -canopy_ant_24, -canopy_ant_29,
         -canopy_ant_31, -canopy_ant_32, -canopy_ant_38, -canopy_ant_55, -canopy_ant_58,
         -canopy_ant_60, -canopy_ant_65, -canopy_ant_66, -canopy_ant_67, -canopy_ant_69,
         -canopy_ant_71, -canopy_ant_72, -canopy_ant_74, -canopy_ant_79, -canopy_ant_84,
         -canopy_ant_88, -canopy_ant_92, -canopy_ant_93, -canopy_ant_98, -canopy_ant_106,
         -canopy_ant_115, -canopy_ant_121, -canopy_ant_123, -canopy_ant_125)

canopy_ants_fax = feature_function(ys, xs)

canopy_ants_features <- canopy_ants_fax$features_dat
canopy_ants_xs <- canopy_ants_fax$covariates
canopy_ants_xs <- data.frame(canopy_ants_xs)

save(canopy_ants_features, file = "outputs/features/canopy_ants_features.rda")
save(canopy_ants_xs, file = "data/datasets_for_analysis/canopy_ants_xs.rda")

## 22. swissalps_plants ####

load("data/datasets_for_analysis/swissalps_plants_ys.rda")
load("data/data_prep/swissalps_plants_prep_xs.rda")

ys <- swissalps_plants_ys
xs <- swissalps_plants_prep_xs

swissalps_plants_fax = feature_function(ys, xs)

swissalps_plants_features <- swissalps_plants_fax$features_dat
swissalps_plants_xs <- swissalps_plants_fax$covariates
swissalps_plants_xs <- data.frame(swissalps_plants_xs)

save(swissalps_plants_features, file = "outputs/features/swissalps_plants_features.rda")
save(swissalps_plants_xs, file = "data/datasets_for_analysis/swissalps_plants_xs.rda")

## 23. earthworms ####

load("data/datasets_for_analysis/earthworms_ys.rda")
load("data/data_prep/earthworms_prep_xs.rda")

ys <- earthworms_ys
xs <- earthworms_prep_xs

ys <- ys %>% 
  select(-earthworm_31, -earthworm_79)

earthworms_fax = feature_function(ys, xs)

earthworms_features <- earthworms_fax$features_dat
earthworms_xs <- earthworms_fax$covariates
earthworms_xs <- data.frame(earthworms_xs)

save(earthworms_features, file = "outputs/features/earthworms_features.rda")
save(earthworms_xs, file = "data/datasets_for_analysis/earthworms_xs.rda")

## 24. vines ####

load("data/datasets_for_analysis/vines_ys.rda")
load("data/data_prep/vines_prep_xs.rda")

ys <- vines_ys
xs <- vines_prep_xs

ys <- ys %>% 
  select(-vine_46)

vines_fax = feature_function(ys, xs)

vines_features <- vines_fax$features_dat
vines_xs <- vines_fax$covariates
vines_xs <- data.frame(vines_xs)

save(vines_features, file = "outputs/features/vines_features.rda")
save(vines_xs, file = "data/datasets_for_analysis/vines_xs.rda")

## 25. buffalo_infections ####

load("data/datasets_for_analysis/buffalo_infections_ys.rda")
load("data/data_prep/buffalo_infections_prep_xs.rda")

ys <- buffalo_infections_ys
xs <- buffalo_infections_prep_xs

buffalo_infections_fax = feature_function(ys, xs)

buffalo_infections_features <- buffalo_infections_fax$features_dat
buffalo_infections_xs <- buffalo_infections_fax$covariates
buffalo_infections_xs <- data.frame(buffalo_infections_xs)

save(buffalo_infections_features, file = "outputs/features/buffalo_infections_features.rda")
save(buffalo_infections_xs, file = "data/datasets_for_analysis/buffalo_infections_xs.rda")

## 26. andean_birds ####

load("data/datasets_for_analysis/andean_birds_ys.rda")
load("data/data_prep/andean_birds_prep_xs.rda")

ys <- andean_birds_ys
xs <- andean_birds_prep_xs

andean_birds_fax = feature_function(ys, xs)

andean_birds_features <- andean_birds_fax$features_dat
andean_birds_xs <- andean_birds_fax$covariates
andean_birds_xs <- data.frame(andean_birds_xs)

save(andean_birds_features, file = "outputs/features/andean_birds_features.rda")
save(andean_birds_xs, file = "data/datasets_for_analysis/andean_birds_xs.rda")

## 27. finland_beetles ####

load("data/datasets_for_analysis/finland_beetles_ys.rda")
load("data/data_prep/finland_beetles_prep_xs.rda")

ys <- finland_beetles_ys
xs <- finland_beetles_prep_xs

ys <- ys %>% 
  select(-finland_beetle_77, -finland_beetle_96)

finland_beetles_fax = feature_function(ys, xs)

finland_beetles_features <- finland_beetles_fax$features_dat
finland_beetles_xs <- finland_beetles_fax$covariates
finland_beetles_xs <- data.frame(finland_beetles_xs)

save(finland_beetles_features, file = "outputs/features/finland_beetles_features.rda")
save(finland_beetles_xs, file = "data/datasets_for_analysis/finland_beetles_xs.rda")

## 28. germany_beetles ####

load("data/datasets_for_analysis/germany_beetles_ys.rda")
load("data/data_prep/germany_beetles_prep_xs.rda")

ys <- germany_beetles_ys
xs <- germany_beetles_prep_xs

germany_beetles_fax = feature_function(ys, xs)

germany_beetles_features <- germany_beetles_fax$features_dat
germany_beetles_xs <- germany_beetles_fax$covariates
germany_beetles_xs <- data.frame(germany_beetles_xs)

save(germany_beetles_features, file = "outputs/features/germany_beetles_features.rda")
save(germany_beetles_xs, file = "data/datasets_for_analysis/germany_beetles_xs.rda")

## 29. norway_beetles ####

load("data/datasets_for_analysis/norway_beetles_ys.rda")
load("data/data_prep/norway_beetles_prep_xs.rda")

ys <- norway_beetles_ys
xs <- norway_beetles_prep_xs

norway_beetles_fax = feature_function(ys, xs)

norway_beetles_features <- norway_beetles_fax$features_dat
norway_beetles_xs <- norway_beetles_fax$covariates
norway_beetles_xs <- data.frame(norway_beetles_xs)

save(norway_beetles_features, file = "outputs/features/norway_beetles_features.rda")
save(norway_beetles_xs, file = "data/datasets_for_analysis/norway_beetles_xs.rda")

## 30. nz_forest ####

load("data/datasets_for_analysis/nz_forest_ys.rda")
load("data/data_prep/nz_forest_prep_xs.rda")

ys <- nz_forest_ys
xs <- nz_forest_prep_xs

nz_forest_fax = feature_function(ys, xs)

nz_forest_features <- nz_forest_fax$features_dat
nz_forest_xs <- nz_forest_fax$covariates
nz_forest_xs <- data.frame(nz_forest_xs)

save(nz_forest_features, file = "outputs/features/nz_forest_features.rda")
save(nz_forest_xs, file = "data/datasets_for_analysis/nz_forest_xs.rda")
