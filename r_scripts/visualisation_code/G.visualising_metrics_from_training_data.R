# Dependencies
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


setwd("C:/Users/uqfpowe1/OneDrive - The University of Queensland/Documents/PhD/Writing Documents/Data Structure Study/MEE_feature_ensemble")

#### Testing the stacked prediction algorithm's performance for out-of-sample prediction. Here
# we run three cross validation folds for each dataset ####
source('functions/stacking_function.R')


#### Visualising results ####

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

# When enough datasets have been run, put all their metric information together for a more
# holistic view of performance based on species prevalence

all_metrics <- data.frame(rbind(helminths_metrics$metric_ranks,
                                fennoscandia_birds_metrics$metric_ranks,
                                victoria_plants_metrics$metric_ranks,
                                usa_trees_metrics$metric_ranks, 
                                eelgrass_metrics$metric_ranks,
                                shrews_metrics$metric_ranks,
                                mussel_parasites_metrics$metric_ranks,
                                lion_infections_metrics$metric_ranks,
                                eucalyptus_metrics$metric_ranks,
                                usa_birds_metrics$metric_ranks,
                                swiss_forest_metrics$metric_ranks,
                                fish_parasites_metrics$metric_ranks, 
                                brazil_fish_metrics$metric_ranks,
                                reptiles_metrics$metric_ranks,
                                canopy_ants_metrics$metric_ranks,
                                swissalps_plants_metrics$metric_ranks,
                                buffalo_infections_metrics$metric_ranks, 
                                finland_beetles_metrics$metric_ranks,
                                germany_beetles_metrics$metric_ranks,
                                nz_forest_metrics$metric_ranks))



all_metrics$Method <- as.character(all_metrics$Method)
all_metrics$Prevalence_category <- factor(all_metrics$Prevalence_category, levels = c('Rare (<10%)', 'Uncommon (10-30%)', 'Common (30-75%)', 'Very common (>75%)'))
all_metrics$Method[all_metrics$Method == "GBM_STACK_PR"] <- "GBM-PR"
all_metrics$Method[all_metrics$Method == "GBM_STACK_DR"] <- "GBM-DR"

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 14),
  strip.text.x = element_text(size = 20))


plot <- plot_recall_changes(all_metrics) + My_Teme 
ggsave('figures/training_data_recall_changes.pdf')
plot_precision_changes(all_metrics) + My_Theme
ggsave('Figures/training_data_precision_changes.pdf')
plot <- plot_F1_changes(all_metrics) + My_Theme
ggsave('Figures/training_data_F1_changes.pdf')



# Summary of species prevalence categories
 all_metrics %>%
  dplyr::group_by(Prevalence_category) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Total_species) %>% dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Prop = Total_species / sum(Total_species))


rare <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, F1) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

rare <- rare[rare$Prevalence_category == "Rare (<10%)", ]

rare <- aggregate(F1 ~ Method + Species + Prevalence_category, rare, mean )

rare %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(F1 > 0.02)),
                   Total_nochange = length(which(F1 < 0.02 & F1 > -0.02)),
                   Total_worse = length(which(F1 < -0.02)))


uncommon <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, F1) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

uncommon <- uncommon[uncommon$Prevalence_category == "Uncommon (10-30%)", ]

uncommon <- aggregate(F1 ~ Method + Species + Prevalence_category, uncommon, mean )

uncommon %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(F1 > 0.02)),
                   Total_nochange = length(which(F1 < 0.02 & F1 > -0.02)),
                   Total_worse = length(which(F1 < -0.02)))


common <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, F1) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

common <- common[common$Prevalence_category == "Common (30-75%)", ]

common <- aggregate(F1 ~ Method + Species + Prevalence_category, common, mean )

common %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(F1 > 0.02)),
                   Total_nochange = length(which(F1 < 0.02 & F1 > -0.02)),
                   Total_worse = length(which(F1 < -0.02)))


very_common <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, F1) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

very_common <- very_common[very_common$Prevalence_category == "Very common (>75%)", ]

very_common <- aggregate(F1 ~ Method + Species + Prevalence_category, very_common, mean )

very_common %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(F1 > 0.02)),
                   Total_nochange = length(which(F1 < 0.02 & F1 > -0.02)),
                   Total_worse = length(which(F1 < -0.02)))


####### PRECISION ######

rare <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, Precision) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

rare <- rare[rare$Prevalence_category == "Rare (<10%)", ]

rare <- aggregate(Precision ~ Method + Species + Prevalence_category, rare, mean )

rare %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Precision > 0.02)),
                   Total_nochange = length(which(Precision < 0.02 & Precision > -0.02)),
                   Total_worse = length(which(Precision < -0.02)))


uncommon <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, Precision) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

uncommon <- uncommon[uncommon$Prevalence_category == "Uncommon (10-30%)", ]

uncommon <- aggregate(Precision ~ Method + Species + Prevalence_category, uncommon, mean )

uncommon %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Precision > 0.02)),
                   Total_nochange = length(which(Precision < 0.02 & Precision > -0.02)),
                   Total_worse = length(which(Precision < -0.02)))


common <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, Precision) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

common <- common[common$Prevalence_category == "Common (30-75%)", ]

common <- aggregate(Precision ~ Method + Species + Prevalence_category, common, mean )

common %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Precision > 0.02)),
                   Total_nochange = length(which(Precision < 0.02 & Precision > -0.02)),
                   Total_worse = length(which(Precision < -0.02)))


very_common <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, Precision) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

very_common <- very_common[very_common$Prevalence_category == "Very common (>75%)", ]

very_common <- aggregate(Precision ~ Method + Species + Prevalence_category, very_common, mean )

very_common %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Precision > 0.02)),
                   Total_nochange = length(which(Precision < 0.02 & Precision > -0.02)),
                   Total_worse = length(which(Precision < -0.02)))

##### RECALL #######

rare <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, Recall) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

rare <- rare[rare$Prevalence_category == "Rare (<10%)", ]

rare <- aggregate(Recall ~ Method + Species + Prevalence_category, rare, mean )

rare %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Recall > 0.02)),
                   Total_nochange = length(which(Recall < 0.02 & Recall > -0.02)),
                   Total_worse = length(which(Recall < -0.02)))


uncommon <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, Recall) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

uncommon <- uncommon[uncommon$Prevalence_category == "Uncommon (10-30%)", ]

uncommon <- aggregate(Recall ~ Method + Species + Prevalence_category, uncommon, mean )

uncommon %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Recall > 0.02)),
                   Total_nochange = length(which(Recall < 0.02 & Recall > -0.02)),
                   Total_worse = length(which(Recall < -0.02)))


common <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, Recall) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

common <- common[common$Prevalence_category == "Common (30-75%)", ]

common <- aggregate(Recall ~ Method + Species + Prevalence_category, common, mean )

common %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Recall > 0.02)),
                   Total_nochange = length(which(Recall < 0.02 & Recall > -0.02)),
                   Total_worse = length(which(Recall < -0.02)))


very_common <- all_metrics %>%
  dplyr::group_by(Prevalence_category, Method, Species) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Species, Prevalence_category, Method, Recall) %>% 
  dplyr::distinct() %>%
  dplyr::ungroup()

very_common <- very_common[very_common$Prevalence_category == "Very common (>75%)", ]

very_common <- aggregate(Recall ~ Method + Species + Prevalence_category, very_common, mean )

very_common %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Recall > 0.02)),
                   Total_nochange = length(which(Recall < 0.02 & Recall > -0.02)),
                   Total_worse = length(which(Recall < -0.02)))

