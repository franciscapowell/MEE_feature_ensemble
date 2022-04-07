## Feature distribution 

setwd("C:/.../MEE_feature_ensemble")

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

load('outputs/features/all_features.rda')

hist(all_features$prevalence, freq = F)
test_prevs <- runif(100, 0, 0.4)
hist(test_prevs, add = T, col = 'black', freq = F)


histogram(all_features$prevalence_rank)
histogram(all_features$prevalence_sd)
histogram(unique(all_features$n_obs))
histogram(all_features$n_species, breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250))
histogram(unique(all_features$n_species, breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250)))
histogram(all_features$degree)
histogram(all_features$eigen_cent)
histogram(all_features$betweenness)
histogram(all_features$modularity)
histogram(all_features$mean_jaccard)
histogram(all_features$mean_dice)
histogram(all_features$jaccard_sd)
histogram(all_features$dice_sd)
histogram(all_features$no_preds)
histogram(all_features$no_pca)
histogram(all_features$var_exp)


plot(all_features$prevalence)
plot(all_features$prevalence_rank)
plot(all_features$prevalence_sd)
plot(all_features$n_obs)
plot(all_features$n_species)
plot(all_features$degree)
plot(all_features$eigen_cent)
plot(all_features$betweenness)
plot(all_features$modularity)
plot(all_features$mean_jaccard)
plot(all_features$mean_dice)
plot(all_features$jaccard_sd)
plot(all_features$dice_sd)
