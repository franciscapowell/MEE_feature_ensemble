
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

source('functions/stacking_function.R')
source('functions/predictions_function.R')
#source('Functions/feature_function.R')

ensemble_model <- readRDS("functions/ensemble_model.rds")


# Global parameters for setting random seeds and the cross-validation split proportion

split_prop <- 0.7



# Dataset 1: bird_parasites ####

load("data/datasets_for_analysis/bird_parasites_ys.rda")
load("data/datasets_for_analysis/bird_parasites_xs.rda")
load("outputs/features/bird_parasites_features.rda")


ys <- bird_parasites_ys
xs <- bird_parasites_xs
features <- bird_parasites_features

all_dat <- cbind(ys, xs)

bird_parasites_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(bird_parasites_predictions, file = "outputs/test_predictions/bird_parasites_predictions.rda")

# Dataset 4: uk_butterflies ####

load("data/datasets_for_analysis/uk_butterflies_ys.rda")
load("data/datasets_for_analysis/uk_butterflies_xs.rda")
load("outputs/features/uk_butterflies_features.rda")

ys <- uk_butterflies_ys
xs <- uk_butterflies_xs
features <- uk_butterflies_features

all_dat <- cbind(ys, xs)

uk_butterflies_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(uk_butterflies_predictions, file = "outputs/test_predictions/uk_butterflies_predictions.rda")

# Dataset 7: norway_vegetation ####

load("data/datasets_for_analysis/norway_vegetation_ys.rda")
load("data/datasets_for_analysis/norway_vegetation_xs.rda")
load("outputs/features/norway_vegetation_features.rda")

ys <- norway_vegetation_ys
xs <- norway_vegetation_xs
features <- norway_vegetation_features

all_dat <- cbind(ys, xs)

norway_vegetation_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(norway_vegetation_predictions, file = "outputs/test_predictions/norway_vegetation_predictions.rda")


# Dataset 13: grassland_birds ####

load("data/datasets_for_analysis/grassland_birds_ys.rda")
load("data/datasets_for_analysis/grassland_birds_xs.rda")
load("outputs/features/grassland_birds_features.rda")

ys <- grassland_birds_ys
xs <- grassland_birds_xs
features <- grassland_birds_features

all_dat <- cbind(ys, xs)

grassland_birds_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(grassland_birds_predictions, file = "outputs/test_predictions/grassland_birds_predictions.rda")
# Dataset 14: mulu_birds ####

load("data/datasets_for_analysis/mulu_birds_ys.rda")
load("data/datasets_for_analysis/mulu_birds_xs.rda")
load("outputs/features/mulu_birds_features.rda")

ys <- mulu_birds_ys
xs <- mulu_birds_xs
features <- mulu_birds_features

all_dat <- cbind(ys, xs)

mulu_birds_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(mulu_birds_predictions, file = "outputs/test_predictions/mulu_birds_predictions.rda")
# Dataset 18: swiss_birds ####

load("data/datasets_for_analysis/swiss_birds_ys.rda")
load("data/datasets_for_analysis/swiss_birds_xs.rda")
load("outputs/features/swiss_birds_features.rda")

ys <- swiss_birds_ys
xs <- swiss_birds_xs
features <- swiss_birds_features

all_dat <- cbind(ys, xs)

swiss_birds_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(swiss_birds_predictions, file = "outputs/test_predictions/swiss_birds_predictions.rda")
# Dataset 23: earthworms ####

load("data/datasets_for_analysis/earthworms_ys.rda")
load("data/datasets_for_analysis/earthworms_xs.rda")
load("outputs/features/earthworms_features.rda")

ys <- earthworms_ys
xs <- earthworms_xs
features <- earthworms_features

all_dat <- cbind(ys, xs)

earthworms_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(earthworms_predictions, file = "outputs/test_predictions/earthworms_predictions.rda")
# Dataset 24: vines ####

load("data/datasets_for_analysis/vines_ys.rda")
load("data/datasets_for_analysis/vines_xs.rda")
load("outputs/features/vines_features.rda")

ys <- vines_ys
xs <- vines_xs
features <- vines_features

all_dat <- cbind(ys, xs)

vines_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(vines_predictions, file = "outputs/test_predictions/vines_predictions.rda")
# Dataset 26: andean_birds ####

load("data/datasets_for_analysis/andean_birds_ys.rda")
load("data/datasets_for_analysis/andean_birds_xs.rda")
load("outputs/features/andean_birds_features.rda")

ys <- andean_birds_ys
xs <- andean_birds_xs
features <- andean_birds_features

all_dat <- cbind(ys, xs)

andean_birds_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(andean_birds_predictions, file = "outputs/test_predictions/andean_birds_predictions.rda")
# Dataset 29: norway_beetles ####

load("data/datasets_for_analysis/norway_beetles_ys.rda")
load("data/datasets_for_analysis/norway_beetles_xs.rda")
load("outputs/features/norway_beetles_features.rda")

ys <- norway_beetles_ys
xs <- norway_beetles_xs
features <- norway_beetles_features

all_dat <- cbind(ys, xs)

norway_beetles_predictions = predictions_function(xs, ys, all_dat, features, split_prop)

save(norway_beetles_predictions, file = "outputs/test_predictions/norway_beetles_predictions.rda")
