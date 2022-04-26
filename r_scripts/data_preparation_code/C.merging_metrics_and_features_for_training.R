library(dplyr)
library(tidyverse)

setwd("C:/.../MEE_feature_ensemble")

load("outputs/training_data_outputs/helminths_metrics.rda")
load("outputs/training_data_outputs/fennoscandia_birds_metrics.rda")
load("outputs/training_data_outputs/victoria_plants_metrics.rda")
load("outputs/training_data_outputs/usa_trees_metrics.rda")
load("outputs/training_data_outputs/eelgrass_metrics.rda")
load("outputs/training_data_outputs/shrews_metrics.rda")
load("outputs/training_data_outputs/mussel_parasites_metrics.rda")
load("outputs/training_data_outputs/lion_infections_metrics.rda")
load("outputs/training_data_outputs/eucalyptus_metrics.rda")
load("outputs/training_data_outputs/usa_birds_metrics.rda")
load("outputs/training_data_outputs/swiss_forest_metrics.rda")
load("outputs/training_data_outputs/fish_parasites_metrics.rda")
load("outputs/training_data_outputs/brazil_fish_metrics.rda")
load("outputs/training_data_outputs/reptiles_metrics.rda")
load("outputs/training_data_outputs/canopy_ants_metrics.rda")
load("outputs/training_data_outputs/swissalps_plants_metrics.rda")
load("outputs/training_data_outputs/buffalo_infections_metrics.rda")
load("outputs/training_data_outputs/finland_beetles_metrics.rda")
load("outputs/training_data_outputs/germany_beetles_metrics.rda")
load("outputs/training_data_outputs/nz_forest_metrics.rda")

mod_weights <- list(helminths_metrics, 
                    fennoscandia_birds_metrics, 
                    victoria_plants_metrics,
                    usa_trees_metrics, 
                    eelgrass_metrics,
                    shrews_metrics,
                    mussel_parasites_metrics,
                    lion_infections_metrics,
                    eucalyptus_metrics, 
                    usa_birds_metrics, 
                    swiss_forest_metrics, 
                    fish_parasites_metrics,
                    brazil_fish_metrics,
                    reptiles_metrics,
                    canopy_ants_metrics,
                    swissalps_plants_metrics, 
                    buffalo_infections_metrics, 
                    finland_beetles_metrics, 
                    germany_beetles_metrics, 
                    nz_forest_metrics) 

names(mod_weights) <- c('helminths', 'fennoscandia_birds', 'victoria_plants', 'usa_trees',
                        'eelgrass', 'shrews', 'mussel_parasites', 'lion_infections', 'eucalyptus', 'usa_birds',
                        'swiss_forest', 'fish_parasites','brazil_fish', 'reptiles', 'canopy_ants', 'swissalps_plants', 
                        'buffalo_infections', 'finland_beetles', 'germany_beetles', 'nz_forest')

all_weights <- rbind(mod_weights$helminths$ens_weights,
                     mod_weights$fennoscandia_birds$ens_weights, 
                     mod_weights$victoria_plants$ens_weights, 
                     mod_weights$usa_trees$ens_weights, 
                     mod_weights$eelgrass$ens_weights,
                     mod_weights$shrews$ens_weights,
                     mod_weights$mussel_parasites$ens_weights,
                     mod_weights$lion_infections$ens_weights,
                     mod_weights$eucalyptus$ens_weights,
                     mod_weights$usa_birds$ens_weights,
                     mod_weights$swiss_forest$ens_weights,
                     mod_weights$fish_parasites$ens_weights, 
                     mod_weights$brazil_fish$ens_weights,
                     mod_weights$reptiles$ens_weights,
                     mod_weights$canopy_ants$ens_weights,
                     mod_weights$swissalps_plants$ens_weights,
                     mod_weights$buffalo_infections$ens_weights,
                     mod_weights$finland_beetles$ens_weights,
                     mod_weights$germany_beetles$ens_weights,
                     mod_weights$nz_forest$ens_weights)

all_weights <- all_weights %>% 
               rename(Weight = value)

dat <- aggregate(all_weights$Weight, by=list(Model=all_weights$Model, Species=all_weights$Species),
          data=all_weights, FUN=mean) 

dat <- dat %>% 
       rename(Weight = x)

data <- dat %>% 
  pivot_wider(names_from = Model, values_from = Weight)

data.try <- data %>% 
  mutate_all(~replace(., is.na(.), 0))

modweight_by_species <- data.try


##### Merging the features to the modweight_by_species dataset ######

load("outputs/features/helminths_features.rda")
load("outputs/features/fennoscandia_birds_features.rda")
load("outputs/features/victoria_plants_features.rda")
load("outputs/features/usa_trees_features.rda")
load("outputs/features/eelgrass_features.rda")
load("outputs/features/shrews_features.rda")
load("outputs/features/mussel_parasites_features.rda")
load("outputs/features/lion_infections_features.rda")
load("outputs/features/eucalyptus_features.rda")
load("outputs/features/usa_birds_features.rda")
load("outputs/features/swiss_forest_features.rda")
load("outputs/features/fish_parasites_features.rda")
load("outputs/features/brazil_fish_features.rda")
load("outputs/features/reptiles_features.rda")
load("outputs/features/canopy_ants_features.rda")
load("outputs/features/swissalps_plants_features.rda")
load("outputs/features/buffalo_infections_features.rda")
load("outputs/features/finland_beetles_features.rda")
load("outputs/features/germany_beetles_features.rda")
load("outputs/features/nz_forest_features.rda")

all_features <- rbind(helminths_features,
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

load("outputs/features/training_data_features.rda")

modweights_by_species <- modweight_by_species %>%
  rename(species = Species)

weights_and_features <- merge(modweights_by_species, training_data_features, by="species", all = T)

weights_and_features <- drop_na(weights_and_features)


save(weights_and_features, file = "outputs/training_data_outputs/weights_and_features.rda" )





