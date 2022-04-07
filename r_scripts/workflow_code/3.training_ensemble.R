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

if(!require(tidyr)){
  install.packages('tidyr')
}

setwd("C:/Users/uqfpowe1/OneDrive - The University of Queensland/Documents/PhD/Writing Documents/Data Structure Study/MEE_feature_ensemble")


load("outputs/training_data_outputs/weights_and_features.rda")

ys <- weights_and_features[2:6]
xs <- weights_and_features[8:30]

all_dat <- cbind(ys, xs)

rfsrc_formula = formula(paste0('cbind(',
                               paste0(colnames(ys), 
                                      collapse = ','),') ~ .'))

ensemble <- randomForestSRC::rfsrc(as.formula(paste0('cbind(', 
                                               paste0(colnames(ys), collapse = ','),
                                               ')~.')),
                             data = all_dat,
                             ntree = 1000,
                             nsplit = NULL,
                             forest = TRUE, 
                             importance = TRUE,
                             nodesize = randomForestSRC::tune.nodesize(as.formula(paste0('cbind(',
                                                                                         paste0(colnames(ys), collapse = ','),
                                                                                         ')~.')),
                                                                       data = all_dat,
                                                                       ntree = 1000,
                                                                       nodesizeTry = c(1:9, seq(10, 100, by = 5)),
                                                                       nsplit = NULL)$nsize.opt)



saveRDS(ensemble, "functions/ensemble_model.rds")
save(ensemble, file = "functions/ensemble_model.rda")

