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

if(!require(gjam)){
  install.packages('gjam')
}

library(parallel)

setwd("C:/Users/uqfpowe1/OneDrive - The University of Queensland/Documents/PhD/Writing Documents/Data Structure Study/MEE_feature_ensemble")

#### Testing the stacked prediction algorithm's performance for out-of-sample prediction. Here
# we run three cross validation folds for each dataset ####
source('functions/stacking_function.R')


#### Visualising results ####

load("outputs/test_predictions/all_predictions.rda")

predictions <- all_predictions[all_predictions$Method != "BASE", ]


My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 15))

predictions$Method <- as.character(predictions$Method)
predictions$Method[predictions$Method == "GBM_PR"] <- "GBM-PR"
predictions$Method[predictions$Method == "GBM_DR"] <- "GBM-DR"
predictions$Method[predictions$Method == "NULL_ENS"] <- "NULL-ENS"

ggplot(predictions, aes(x = Recall_adj, y = Method)) + 
  geom_boxplot(aes(fill = Method)) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_bw() + theme(legend.position = 'none') +
  xlab('Improvement in recall over GLM-BASE') + 
  My_Theme


ggplot(predictions, aes(x = Precision_adj, y = Method)) + 
  geom_boxplot(aes(fill = Method)) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_bw() + theme(legend.position = 'none') +
  xlab('Improvement in precision over GLM-BASE')  + 
  My_Theme


ggplot(predictions, aes(x = F1_adj, y = Method)) + 
  geom_boxplot(aes(fill = Method)) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_bw() + theme(legend.position = 'none') +
  xlab('Improvement in F1 over GLM-BASE')  + 
  My_Theme
sessionInfo()


# Summary of species prevalence categories
all_predictions %>%
  dplyr::group_by(Method) %>%
  dplyr::mutate(Total_species = length(unique(Species))) %>%
  dplyr::select(Total_species) %>% dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Prop = Total_species / sum(Total_species))

# Summary of meaningful changes in F1
predictions %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(F1_adj > 0.02)),
                   Total_nochange = length(which(F1_adj < 0.02 & F1_adj > -0.02)),
                   Total_worse = length(which(F1_adj < -0.02)))

# Summary of meaningful changes in Precision
predictions %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Precision_adj > 0.02)),
                   Total_nochange = length(which(Precision_adj < 0.02 & Precision_adj > -0.02)),
                   Total_worse = length(which(Precision_adj < -0.02)))

# Summary of meaningful changes in Recall
predictions %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Total_better = length(which(Recall_adj > 0.02)),
                   Total_nochange = length(which(Recall_adj < 0.02 & Recall_adj > -0.02)),
                   Total_worse = length(which(Recall_adj < -0.02)))


