library(dplyr)
library(tidyverse)

setwd("C:/Users/uqfpowe1/OneDrive - The University of Queensland/Documents/PhD/Writing Documents/Data Structure Study/MEE_feature_ensemble")

load("outputs/test_predictions/bird_parasites_predictions.rda")
load("outputs/test_predictions/uk_butterflies_predictions.rda")
load("outputs/test_predictions/norway_vegetation_predictions.rda")
load("outputs/test_predictions/grassland_birds_predictions.rda")
load("outputs/test_predictions/mulu_birds_predictions.rda")
load("outputs/test_predictions/swiss_birds_predictions.rda")
load("outputs/test_predictions/earthworms_predictions.rda")
load("outputs/test_predictions/vines_predictions.rda")
load("outputs/test_predictions/andean_birds_predictions.rda")
load("outputs/test_predictions/norway_beetles_predictions.rda")


bird_parasites_predictions <- bird_parasites_predictions$all_metrics
uk_butterflies_predictions <- uk_butterflies_predictions$all_metrics
norway_vegetation_predictions <- norway_vegetation_predictions$all_metrics
grassland_birds_predictions <- grassland_birds_predictions$all_metrics
mulu_birds_predictions <- mulu_birds_predictions$all_metrics
swiss_birds_predictions <- swiss_birds_predictions$all_metrics
earthworms_predictions <- earthworms_predictions$all_metrics
vines_predictions <- vines_predictions$all_metrics
andean_birds_predictions <- andean_birds_predictions$all_metrics
norway_beetles_predictions <- norway_beetles_predictions$all_metrics


all_predictions <- rbind(bird_parasites_predictions,
                         uk_butterflies_predictions,
                         norway_vegetation_predictions,
                         grassland_birds_predictions,
                         mulu_birds_predictions,
                         swiss_birds_predictions,
                         earthworms_predictions,
                         vines_predictions,
                         andean_birds_predictions,
                         norway_beetles_predictions)


save(all_predictions, file = "outputs/test_predictions/all_predictions.rda" )


