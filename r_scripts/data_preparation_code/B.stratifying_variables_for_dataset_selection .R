## stratifying variables for selecting training and testing datasets 

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

if(!require(gjam)){
  install.packages('gjam')
}

if(!require(parallel)){
  install.packages('parallel')
}

if(!require(MRFcov)){
  install.packages('MRFcov')
}


setwd("C:/.../MEE_feature_ensemble")

load('outputs/features/bird_parasites_features.rda')

bird_parasites_features$dataset <- "bird_parasites"
bird_parasites_features <- bird_parasites_features[colnames(bird_parasites_features)[c(25,1:24)]]
bird_parasites_features$med_prev <- median(bird_parasites_features$prevalence)

load('outputs/features/helminths_features.rda')

helminths_features$dataset <- "helminths"
helminths_features <- helminths_features[colnames(helminths_features)[c(25,1:24)]]
helminths_features$med_prev <- median(helminths_features$prevalence)

load('outputs/features/fennoscandia_birds_features.rda')

fennoscandia_birds_features$dataset <- "fennoscandia_birds"
fennoscandia_birds_features <- fennoscandia_birds_features[colnames(fennoscandia_birds_features)[c(25,1:24)]]
fennoscandia_birds_features$med_prev <- median(fennoscandia_birds_features$prevalence)

load('outputs/features/uk_butterflies_features.rda')

uk_butterflies_features$dataset <- "uk_butterflies"
uk_butterflies_features <- uk_butterflies_features[colnames(uk_butterflies_features)[c(25,1:24)]]
uk_butterflies_features$med_prev <- median(uk_butterflies_features$prevalence)

load('outputs/features/victoria_plants_features.rda')

victoria_plants_features$dataset <- "victoria_plants"
victoria_plants_features <- victoria_plants_features[colnames(victoria_plants_features)[c(25,1:24)]]
victoria_plants_features$med_prev <- median(victoria_plants_features$prevalence)

load('outputs/features/usa_trees_features.rda')

usa_trees_features$dataset <- "usa_trees"
usa_trees_features <- usa_trees_features[colnames(usa_trees_features)[c(25,1:24)]]
usa_trees_features$med_prev <- median(usa_trees_features$prevalence)

load('outputs/features/norway_vegetation_features.rda')

norway_vegetation_features$dataset <- "norway_vegetation"
norway_vegetation_features <- norway_vegetation_features[colnames(norway_vegetation_features)[c(25,1:24)]]
norway_vegetation_features$med_prev <- median(norway_vegetation_features$prevalence)

load('outputs/features/eelgrass_features.rda')

eelgrass_features$dataset <- "eelgrass"
eelgrass_features <- eelgrass_features[colnames(eelgrass_features)[c(25,1:24)]]
eelgrass_features$med_prev <- median(eelgrass_features$prevalence)

load('outputs/features/shrews_features.rda')

shrews_features$dataset <- "shrews"
shrews_features <- shrews_features[colnames(shrews_features)[c(25,1:24)]]
shrews_features$med_prev <- median(shrews_features$prevalence)

load('outputs/features/mussel_parasites_features.rda')

mussel_parasites_features$dataset <- "mussel_parasites"
mussel_parasites_features <- mussel_parasites_features[colnames(mussel_parasites_features)[c(25,1:24)]]
mussel_parasites_features$med_prev <- median(mussel_parasites_features$prevalence)

load('outputs/features/lion_infections_features.rda')

lion_infections_features$dataset <- "lion_infections"
lion_infections_features <- lion_infections_features[colnames(lion_infections_features)[c(25,1:24)]]
lion_infections_features$med_prev <- median(lion_infections_features$prevalence)

load('outputs/features/eucalyptus_features.rda')

eucalyptus_features$dataset <- "eucalyptus"
eucalyptus_features <- eucalyptus_features[colnames(eucalyptus_features)[c(25,1:24)]]
eucalyptus_features$med_prev <- median(eucalyptus_features$prevalence)

load('outputs/features/grassland_birds_features.rda')

grassland_birds_features$dataset <- "grassland_birds"
grassland_birds_features <- grassland_birds_features[colnames(grassland_birds_features)[c(25,1:24)]]
grassland_birds_features$med_prev <- median(grassland_birds_features$prevalence)

load('outputs/features/mulu_birds_features.rda')

mulu_birds_features$dataset <- "mulu_birds"
mulu_birds_features <- mulu_birds_features[colnames(mulu_birds_features)[c(25,1:24)]]
mulu_birds_features$med_prev <- median(mulu_birds_features$prevalence)

load('outputs/features/usa_birds_features.rda')

usa_birds_features$dataset <- "usa_birds"
usa_birds_features <- usa_birds_features[colnames(usa_birds_features)[c(25,1:24)]]
usa_birds_features$med_prev <- median(usa_birds_features$prevalence)

load('outputs/features/swiss_birds_features.rda')

swiss_birds_features$dataset <- "swiss_birds"
swiss_birds_features <- swiss_birds_features[colnames(swiss_birds_features)[c(25,1:24)]]
swiss_birds_features$med_prev <- median(swiss_birds_features$prevalence)

load('outputs/features/swiss_forest_features.rda')

swiss_forest_features$dataset <- "swiss_forest"
swiss_forest_features <- swiss_forest_features[colnames(swiss_forest_features)[c(25,1:24)]]
swiss_forest_features$med_prev <- median(swiss_forest_features$prevalence)

load('outputs/features/fish_parasites_features.rda')

fish_parasites_features$dataset <- "fish_parasites"
fish_parasites_features <- fish_parasites_features[colnames(fish_parasites_features)[c(25,1:24)]]
fish_parasites_features$med_prev <- median(fish_parasites_features$prevalence)

load('outputs/features/brazil_fish_features.rda')

brazil_fish_features$dataset <- "brazil_fish"
brazil_fish_features <- brazil_fish_features[colnames(brazil_fish_features)[c(25,1:24)]]
brazil_fish_features$med_prev <- median(brazil_fish_features$prevalence)

load('outputs/features/reptiles_features.rda')

reptiles_features$dataset <- "reptiles"
reptiles_features <- reptiles_features[colnames(reptiles_features)[c(25,1:24)]]
reptiles_features$med_prev <- median(reptiles_features$prevalence)

load('outputs/features/canopy_ants_features.rda')

canopy_ants_features$dataset <- "canopy_ants"
canopy_ants_features <- canopy_ants_features[colnames(canopy_ants_features)[c(25,1:24)]]
canopy_ants_features$med_prev <- median(canopy_ants_features$prevalence)

load('outputs/features/swissalps_plants_features.rda')

swissalps_plants_features$dataset <- "swissalps_plants"
swissalps_plants_features <- swissalps_plants_features[colnames(swissalps_plants_features)[c(25,1:24)]]
swissalps_plants_features$med_prev <- median(swissalps_plants_features$prevalence)

load('outputs/features/earthworms_features.rda')

earthworms_features$dataset <- "earthworms"
earthworms_features <- earthworms_features[colnames(earthworms_features)[c(25,1:24)]]
earthworms_features$med_prev <- median(earthworms_features$prevalence)

load('outputs/features/vines_features.rda')

vines_features$dataset <- "vines"
vines_features <- vines_features[colnames(vines_features)[c(25,1:24)]]
vines_features$med_prev <- median(vines_features$prevalence)

load('outputs/features/buffalo_infections_features.rda')

buffalo_infections_features$dataset <- "buffalo_infections"
buffalo_infections_features <- buffalo_infections_features[colnames(buffalo_infections_features)[c(25,1:24)]]
buffalo_infections_features$med_prev <- median(buffalo_infections_features$prevalence)

load('outputs/features/andean_birds_features.rda')

andean_birds_features$dataset <- "andean_birds"
andean_birds_features <- andean_birds_features[colnames(andean_birds_features)[c(25,1:24)]]
andean_birds_features$med_prev <- median(andean_birds_features$prevalence)

load('outputs/features/finland_beetles_features.rda')

finland_beetles_features$dataset <- "finland_beetles"
finland_beetles_features <- finland_beetles_features[colnames(finland_beetles_features)[c(25,1:24)]]
finland_beetles_features$med_prev <- median(finland_beetles_features$prevalence)

load('outputs/features/germany_beetles_features.rda')

germany_beetles_features$dataset <- "germany_beetles"
germany_beetles_features <- germany_beetles_features[colnames(germany_beetles_features)[c(25,1:24)]]
germany_beetles_features$med_prev <- median(germany_beetles_features$prevalence)

load('outputs/features/norway_beetles_features.rda')

norway_beetles_features$dataset <- "norway_beetles"
norway_beetles_features <- norway_beetles_features[colnames(norway_beetles_features)[c(25,1:24)]]
norway_beetles_features$med_prev <- median(norway_beetles_features$prevalence)

load('outputs/features/nz_forest_features.rda')

nz_forest_features$dataset <- "nz_forest"
nz_forest_features <- nz_forest_features[colnames(nz_forest_features)[c(25,1:24)]]
nz_forest_features$med_prev <- median(nz_forest_features$prevalence)


all_features <- rbind(bird_parasites_features,
                      helminths_features,
                      fennoscandia_birds_features,
                      uk_butterflies_features,
                      victoria_plants_features,
                      usa_trees_features,
                      norway_vegetation_features,
                      eelgrass_features,
                      shrews_features,
                      mussel_parasites_features,
                      lion_infections_features,
                      eucalyptus_features,
                      grassland_birds_features,
                      mulu_birds_features,
                      usa_birds_features,
                      swiss_birds_features,
                      swiss_forest_features,
                      fish_parasites_features,
                      brazil_fish_features,
                      reptiles_features,
                      canopy_ants_features,
                      swissalps_plants_features,
                      earthworms_features,
                      vines_features,
                      buffalo_infections_features,
                      andean_birds_features,
                      finland_beetles_features,
                      germany_beetles_features,
                      norway_beetles_features,
                      nz_forest_features)

save(all_features, file = "outputs/features/all_features.rda")

dataset_selection <- all_features %>% 
  select(dataset,n_species, n_obs, no_pc, med_prev)
dataset_selection <- unique(dataset_selection)
row.names(dataset_selection) <- 1:nrow(dataset_selection)


dataset_selection %>%
  dplyr::mutate(n_species_cat = as.numeric(cut(n_species, 3)),
                n_obs_cat = as.numeric(cut(n_obs, 3)),
                no_pc_cat = as.numeric(cut(no_pc, 3)),
                med_prev_cat = as.numeric(cut(med_prev, 3))) -> data


#optional tag row for later identification: 
data$rowid<-1:nrow(data)

# Deciding which datasets to remove from testing datasets. 

# testing data without helminths and fish_parasites 

testing_rm_n_obs <- rbind(bird_parasites_features,
                          norway_vegetation_features,
                          uk_butterflies_features,
                          grassland_birds_features,
                          mulu_birds_features,
                          swiss_birds_features,
                          earthworms_features,
                          vines_features,
                          andean_birds_features,
                          norway_beetles_features)

summary(testing_rm_n_obs$prevalence)


training_rm_n_obs <- rbind(helminths_features,
                           fennoscandia_birds_features,
                           victoria_plants_features,
                           usa_trees_features,
                           eelgrass_features,
                           shrews_features,
                           mussel_parasites_features,
                           lion_infections_features,
                           eucalyptus_features,
                           usa_birds_features,
                           swiss_forest_features,
                           fish_parasites_features,
                           brazil_fish_features,
                           reptiles_features,
                           canopy_ants_features,
                           swissalps_plants_features,
                           buffalo_infections_features,
                           finland_beetles_features,
                           germany_beetles_features,
                           nz_forest_features)

summary(training_rm_n_obs$prevalence)

# testing data without uk_butterflies and swiss_birds 

testing_rm_med_prev <- rbind(bird_parasites_features,
                             norway_vegetation_features,
                             helminths_features,
                             grassland_birds_features,
                             mulu_birds_features,
                             fish_parasites_features,
                             earthworms_features,
                             vines_features,
                             andean_birds_features,
                             norway_beetles_features)

summary(testing_rm_med_prev$prevalence)

training_rm_med_prev <- rbind(fennoscandia_birds_features,
                              uk_butterflies_features,
                              victoria_plants_features,
                              usa_trees_features,
                              eelgrass_features,
                              shrews_features,
                              mussel_parasites_features,
                              lion_infections_features,
                              eucalyptus_features,
                              swiss_birds_features,
                              usa_birds_features,
                              swiss_forest_features,
                              brazil_fish_features,
                              reptiles_features,
                              canopy_ants_features,
                              swissalps_plants_features,
                              buffalo_infections_features,
                              finland_beetles_features,
                              germany_beetles_features,
                              nz_forest_features)

summary(training_rm_med_prev$prevalence)

# based on the similarity between prevalence measures across both training and testing data
# remove helminths and fish_parasites from testing datasets, effectively removing the number 
# of observations as a stratification category (since all datasets in training have low no_obs values)

testing_data_features <- rbind(bird_parasites_features,
                          norway_vegetation_features,
                          uk_butterflies_features,
                          grassland_birds_features,
                          mulu_birds_features,
                          swiss_birds_features,
                          earthworms_features,
                          vines_features,
                          andean_birds_features,
                          norway_beetles_features)

save(testing_data_features, file = "outputs/features/testing_data_features.rda")

training_data_features <- rbind(helminths_features,
                           fennoscandia_birds_features,
                           victoria_plants_features,
                           usa_trees_features,
                           eelgrass_features,
                           shrews_features,
                           mussel_parasites_features,
                           lion_infections_features,
                           eucalyptus_features,
                           usa_birds_features,
                           swiss_forest_features,
                           fish_parasites_features,
                           brazil_fish_features,
                           reptiles_features,
                           canopy_ants_features,
                           swissalps_plants_features,
                           buffalo_infections_features,
                           finland_beetles_features,
                           germany_beetles_features,
                           nz_forest_features)

save(training_data_features, file = "outputs/features/training_data_features.rda")
