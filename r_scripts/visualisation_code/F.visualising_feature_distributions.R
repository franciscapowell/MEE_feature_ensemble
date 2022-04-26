## Feature distribution 

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

load('outputs/features/all_features.rda')
load('outputs/features/testing_data_features.rda')
load('outputs/features/training_data_features.rda')

## Prevalence ####

par(mfrow = c(1, 3))

hist(all_features$prevalence,
     main = "All Datasets", 
     xlab="Prevalence",
     xlim=c(0,1),
     ylim = c(0,7),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$prevalence,
     main = "Training Datasets", 
     xlab="Prevalence",
     xlim=c(0,1),
     ylim = c(0,7),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$prevalence,
     main = "Testing Datasets", 
     xlab="Prevalence",
     xlim=c(0,1),
     ylim = c(0,7),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)


## Prevalence Rank ####


par(mfrow = c(1, 3))

hist(all_features$prevalence_rank,
     main = "All Datasets", 
     xlab="Prevalence Rank",
     xlim=c(0,1),
     ylim = c(0,1.5),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$prevalence_rank,
     main = "Training Datasets", 
     xlab="Prevalence Rank",
     xlim=c(0,1),
     ylim = c(0,1.5),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$prevalence_rank,
     main = "Testing Datasets", 
     xlab="Prevalence Rank",
     xlim=c(0,1),
     ylim = c(0,1.5),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## Prevalence SD ####


par(mfrow = c(1, 3))

hist(all_features$prevalence_sd,
     main = "All Datasets", 
     xlab="Prevalence Standard Deviation",
     xlim=c(0,0.35),
     ylim = c(0,17),
     breaks = 7,
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$prevalence_sd,
     main = "Training Datasets", 
     xlab="Prevalence Standard Deviation",
     xlim=c(0,0.35),
     ylim = c(0,17),
     breaks = 7,
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$prevalence_sd,
     main = "Testing Datasets", 
     xlab="Prevalence Standard Deviation",
     xlim=c(0,0.35),
     ylim = c(0,17),
     breaks = 7,
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)



## Number of Observations ####

par(mfrow = c(1, 3))

hist(all_features$n_obs,
     main = "All Datasets", 
     xlab="Number of Observations",
     xlim=c(0,9000),
     ylim = c(0, 0.001),
     breaks = c(0,75, 100, 500, 1000, 1500, 2000, 3000, 5000,  7000,  9000),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$n_obs,
     main = "Training Datasets", 
     xlab="Number of Observations",
     xlim=c(0,9000),
     ylim = c(0, 0.001),
     breaks = c(0,75, 100, 500, 1000, 1500, 2000, 3000, 5000,  7000,  9000),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$n_obs,
     main = "Testing Datasets", 
     xlab="Number of Observations",
     xlim=c(0,9000),
     ylim = c(0, 0.001),
     breaks = c(0,75, 100, 500, 1000, 1500, 2000, 3000, 5000,  7000,  9000),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)



## Number of Species ####


par(mfrow = c(1, 3))

hist(all_features$n_species,
     main = "All Datasets", 
     xlab="Number of Species",
     xlim=c(0,250),
     ylim = c(0,0.03),
     breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$n_species,
     main = "Training Datasets", 
     xlab="Number of Species",
     xlim=c(0,250),
     ylim = c(0,0.03),
     breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$n_species,
     main = "Testing Datasets", 
     xlab="Number of Species",
     xlim=c(0,250),
     ylim = c(0,0.03),
     breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## Degree ####


par(mfrow = c(1, 3))

hist(all_features$degree,
     main = "All Datasets", 
     xlab="Degree Centrality",
     xlim=c(0,1),
     ylim = c(0,3.5),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$degree,
     main = "Training Datasets", 
     xlab="Degree Centrality",
     xlim=c(0,1),
     ylim = c(0,3.5),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$degree,
     main = "Testing Datasets", 
     xlab="Degree Centrality",
     xlim=c(0,1),
     ylim = c(0,3.5),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)


## Eigenvector Centrality ####


par(mfrow = c(1, 3))

hist(all_features$eigen_cent,
     main = "All Datasets", 
     xlab="Eigenvector Centrality",
     xlim=c(0,1),
     ylim = c(0,5),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$eigen_cent,
     main = "Training Datasets", 
     xlab="Eigenvector Centrality",
     xlim=c(0,1),
     ylim = c(0,5),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$eigen_cent,
     main = "Testing Datasets", 
     xlab="Eigenvector Centrality",
     xlim=c(0,1),
     ylim = c(0,5),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## Betweenness Centrality ####


par(mfrow = c(1, 3))

hist(all_features$betweenness,
     main = "All Datasets", 
     xlab="Betweenness Centrality",
     xlim=c(0,1.5),
     ylim = c(0,10),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$betweenness,
     main = "Training Datasets", 
     xlab="Betweenness Centrality",
     xlim=c(0,1.5),
     ylim = c(0,10),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$betweenness,
     main = "Testing Datasets", 
     xlab="Betweenness Centrality",
     xlim=c(0,1.5),
     ylim = c(0,10),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)



## Modularity ####


par(mfrow = c(1, 3))

hist(all_features$modularity,
     main = "All Datasets", 
     xlab="Modularity",
     xlim=c(-1.5,0.7),
     ylim = c(0,4),
     breaks = c(-1.5, -1.3, -1.1, -0.9, -0.7, -0.5, -0.3, -0.1, 0.1, 0.3, 0.5, 0.7),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$modularity,
     main = "Training Datasets", 
     xlab="Modularity",
     xlim=c(-1.5,0.7),
     ylim = c(0,4),
     breaks = c(-1.5, -1.3, -1.1, -0.9, -0.7, -0.5, -0.3, -0.1, 0.1, 0.3, 0.5, 0.7),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$modularity,
     main = "Testing Datasets", 
     xlab="Modularity",
     xlim=c(-1.5,0.7),
     ylim = c(0,4),
    breaks = c(-1.5, -1.3, -1.1, -0.9, -0.7, -0.5, -0.3, -0.1, 0.1, 0.3, 0.5, 0.7),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)


## Mean Jaccard ####


par(mfrow = c(1, 3))

hist(all_features$mean_jaccard,
     main = "All Datasets", 
     xlab="Mean Jaccard Distance",
     xlim=c(0.6,1),
     ylim = c(0,20),
     breaks = c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$mean_jaccard,
     main = "Training Datasets", 
     xlab="Mean Jaccard Distance",
     xlim=c(0.6,1),
     ylim = c(0,20),
     breaks = c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$mean_jaccard,
     main = "Testing Datasets", 
     xlab="Mean Jaccard Distance",
     xlim=c(0.6,1),
     ylim = c(0,20),
     breaks = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)


## Mean  Dice ####


par(mfrow = c(1, 3))

hist(all_features$mean_dice,
     main = "All Datasets", 
     xlab="Mean Sørensen-Dice Distance",
     xlim=c(0.5,1),
     ylim = c(0,13),
     breaks = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$mean_dice,
     main = "Training Datasets", 
     xlab="Mean Sørensen-Dice Distance",
     xlim=c(0.5,1),
     ylim = c(0,13),
     breaks = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$mean_dice,
     main = "Testing Datasets", 
     xlab="Mean Sørensen-Dice Distance",
     xlim=c(0.5,1),
     ylim = c(0,13),
     breaks = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## Mean Jaccard SD ####


par(mfrow = c(1, 3))

hist(all_features$jaccard_sd,
     main = "All Datasets", 
     xlab="Mean Jaccard Distance Standard Deviation",
     xlim=c(0,0.12),
     ylim = c(0,50),
     breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$jaccard_sd,
     main = "Training Datasets", 
     xlab="Mean Jaccard Distance Standard Deviation",
     xlim=c(0,0.12),
     ylim = c(0,50),
     breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$jaccard_sd,
     main = "Testing Datasets", 
     xlab="Mean Jaccard Distance Standard Deviation",
     xlim=c(0,0.12),
     ylim = c(0,50),
     breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## Mean Dice SD ####


par(mfrow = c(1, 3))

hist(all_features$dice_sd,
     main = "All Datasets", 
     xlab="Mean Sørensen-Dice Distance Standard Deviation",
     xlim=c(0,0.14),
     ylim = c(0,30),
     breaks = c(0, 0.02, 0.04,  0.06, 0.08, 0.1, 0.12,0.14),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$dice_sd,
     main = "Training Datasets", 
     xlab="Mean Sørensen-Dice Distance Standard Deviation",
     xlim=c(0,0.13),
     ylim = c(0,30),
     breaks = c(0, 0.02, 0.04,  0.06, 0.08, 0.1, 0.12,0.14),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$dice_sd,
     main = "Testing Datasets", 
     xlab="Mean Sørensen-Dice Distance Standard Deviation",
     xlim=c(0,0.13),
     ylim = c(0,30),
     breaks = c(0, 0.02, 0.04,  0.06, 0.08, 0.1, 0.12,0.14),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## Mean Sorensen ####


par(mfrow = c(1, 3))

hist(all_features$mean_sorensen,
     main = "All Datasets", 
     xlab="Mean Sørensen Index",
     xlim=c(0.3,1),
     ylim=c(0,5),
     breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$mean_sorensen,
     main = "Training Datasets", 
     xlab="Mean Sørensen Index",
     xlim=c(0.3,1),
     ylim=c(0,5),
     breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$mean_sorensen,
     main = "Testing Datasets", 
     xlab="Mean Sørensen Index",
     xlim=c(0.3,1),
     ylim=c(0,5),
     breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)


## Mean Sorensen SD ####


par(mfrow = c(1, 3))

hist(all_features$sd_sorensen,
     main = "All Datasets", 
     xlab="Sørensen Index Standard Deviation",
     xlim=c(0.09,0.35),
     ylim =c(0,25),
     breaks = c(0.09, 0.11, 0.13, 0.15, 0.17, 0.19, 0.21, 0.23, 0.25, 0.27, 0.29, 0.31, 0.33, 0.35),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$sd_sorensen,
     main = "Training Datasets", 
     xlab="Sørensen Index Standard Deviation",
     xlim=c(0.09,0.35),
     ylim =c(0,25),
     breaks = c(0.09, 0.11, 0.13, 0.15, 0.17, 0.19, 0.21, 0.23, 0.25, 0.27, 0.29, 0.31, 0.33, 0.35),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$sd_sorensen,
     main = "Testing Datasets", 
     xlab="Sørensen Index Standard Deviation",
     xlim=c(0.09,0.35),
     ylim =c(0,25),
     breaks = c(0.09, 0.11, 0.13, 0.15, 0.17, 0.19, 0.21, 0.23, 0.25, 0.27, 0.29, 0.31, 0.33, 0.35),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## MRF Intercept ####


par(mfrow = c(1, 3))

hist(unique(all_features$mrf_intercept),
     main = "All Datasets", 
     xlab="MRF Intercept",
     xlim=c(-60,10),
     ylim=c(0,0.13),
     breaks = 10,
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(unique(training_data_features$mrf_intercept),
     main = "Training Datasets", 
     xlab="MRF Intercept",
     xlim=c(-60,10),
     ylim=c(0,0.13),
     breaks = 10,
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(unique(testing_data_features$mrf_intercept),
     main = "Testing Datasets", 
     xlab="MRF Intercept",
     xlim=c(-60,10),
     ylim=c(0,0.13),
     breaks = 10,
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## MRF Network Information ####


par(mfrow = c(1, 3))

hist(all_features$sum_mrfinfo,
     main = "All Datasets", 
     xlab="MRF Network Information",
     xlim=c(0,90),
     ylim = c(0,0.08),
     breaks = 10,
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$sum_mrfinfo,
     main = "Training Datasets", 
     xlab="MRF Network Information",
     xlim=c(0,90),
     ylim = c(0,0.08),
     breaks = 10,
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$sum_mrfinfo,
     main = "Testing Datasets", 
     xlab="MRF Network Information",
     xlim=c(0,90),
     ylim = c(0,0.08),
     breaks = 10,
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## MRF Network Information Standard Deviation ####


par(mfrow = c(1, 3))

hist(all_features$sd_mrfinfo,
     main = "All Datasets", 
     xlab="MRF Network Information Standard Deviation",
     xlim=c(0,2.4),
     ylim =c(0,2.5),
     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4), 
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$sd_mrfinfo,
     main = "Training Datasets", 
     xlab="MRF Network Information Standard Deviation",
     xlim=c(0,2.4),
     ylim =c(0,2.5),
     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4), 
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$sd_mrfinfo,
     main = "Testing Datasets", 
     xlab="MRF Network Information Standard Deviation",
     xlim=c(0,2.4),
     ylim =c(0,2.5),
     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4), 
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## MRF Trace ####


par(mfrow = c(1, 3))

hist(all_features$mrf_trace,
     main = "All Datasets", 
     xlab="MRF Trace",
     xlim=c(-3,4.5),
     ylim = c(0, 0.8),
     breaks = c(-3, -2.5, -2.0, -1.5, -1.0, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$mrf_trace,
     main = "Training Datasets", 
     xlab="MRF Trace",
     xlim=c(-3,4.5),
     ylim = c(0, 0.8),
     breaks = c(-3, -2.5, -2.0, -1.5, -1.0, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$mrf_trace,
     main = "Testing Datasets", 
     xlab="MRF Trace",
     xlim=c(-3,4.5),
     ylim = c(0, 0.8),
     breaks = c(-3, -2.5, -2.0, -1.5, -1.0, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)


## Log Determinant ####


par(mfrow = c(1, 3))

hist(all_features$log_determinant,
     main = "All Datasets", 
     xlab="Log Determinant",
     xlim=c(-1,0.2),
     ylim = c(0, 10),
     breaks = c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$log_determinant,
     main = "Training Datasets", 
     xlab="Log Determinant",
     xlim=c(-1,0.2),
     ylim = c(0, 10),
     breaks = c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$log_determinant,
     main = "Testing Datasets", 
     xlab="Log Determinant",
     xlim=c(-1,0.2),
     ylim = c(0, 20),
     breaks = c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## Number of Predictors ####


par(mfrow = c(1, 3))

hist(all_features$no_preds,
     main = "All Datasets", 
     xlab="Number of Predictors",
     xlim=c(0,55),
     ylim = c(0, 0.1),
     breaks = 11,
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$no_preds,
     main = "Training Datasets", 
     xlab="Number of Predictors",
     xlim=c(0,55),
     ylim = c(0, 0.1),
     breaks = 11,
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$no_preds,
     main = "Testing Datasets", 
     xlab="Number of Predictors",
     xlim=c(0,55),
     ylim = c(0, 0.1),
     breaks = 11,
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

## Number of PCs ####

par(mfrow = c(1, 3))

hist(all_features$no_pc,
     main = "All Datasets", 
     xlab="Number of PCs",
     xlim=c(0,5),
     ylim = c(0,0.7),
     breaks = c(0, 1, 2, 3, 4, 5),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)


hist(training_data_features$no_pc,
     main = "Training Datasets", 
     xlab="Number of PCs",
     xlim=c(0,5),
     ylim = c(0,0.7),
     breaks = c(0, 1, 2, 3, 4, 5),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$no_pc,
     main = "Testing Datasets", 
     xlab="Number of PCs",
     xlim=c(0,5),
     ylim = c(0,0.7),
     breaks = c(0, 1, 2, 3, 4, 5),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)


## Var Explained ####


par(mfrow = c(1, 3))

hist(all_features$var_exp,
     main = "All Datasets", 
     xlab="Cumulative Variation Explained by PCs",
     xlim=c(0.4,1),
     ylim = c(0,5),
     breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
     col="#b8627dff",
     border="#810541",
     freq = FALSE)

hist(training_data_features$var_exp,
     main = "Training Datasets", 
     xlab="Cumulative Variation Explained by PCs",
     xlim=c(0.4,1),
     ylim = c(0,5),
     breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
     col="#f68f46ff",
     border="#C04000",
     freq = FALSE)

hist(testing_data_features$var_exp,
     main = "Testing Datasets", 
     xlab="Cumulative Variation Explained by PCs",
     xlim=c(0.4,1),
     ylim = c(0,5),
     breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
     col="#403891ff",
     border="#0c2a50ff",
     freq = FALSE)

