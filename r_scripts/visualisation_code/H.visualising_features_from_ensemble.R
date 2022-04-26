if(!require(ggplot2)){
  install.packages('ggplot2')
}

if(!require(tibble)){
  install.packages('tibble')
}

if(!require(viridis)){
  install.packages('viridis')
}

if(!require(tidyr)){
  install.packages('tidyr')
}

if(!require(cowplot)){
  install.packages('cowplot')
}

if(!require(gridExtra)){
  install.packages('gridExtra')
}


setwd("C:/.../MEE_feature_ensemble")


ensemble_model <- readRDS("functions/ensemble_model.rds")

###### Visualize variable importance plots #######

# For each of these, take note of the most important variable (ie. the variable at the top) 

base <- plot(ensemble_model, m.target = 'BASE') #prevalence 
gbm_stack_dr <- plot(ensemble_model, m.target = 'GBM_STACK_DR') #eigen_cent
gbm_stack_pr <- plot(ensemble_model, m.target = 'GBM_STACK_PR') # prevalence
hmsc <- plot(ensemble_model, m.target = 'HMSC') # prevalence
mvrf <- plot(ensemble_model, m.target = 'MVRF') # prevalence


#### Relative Importance Heatmap #####

# We will now create a dataframe with the relative importance values of features for each model 

hm_dat <- matrix(NA, nrow = 23, ncol = 5)

models <- c("BASE", "GBM-DR", "GBM-PR", "MVRF", "HMSC")

features <- c("prevalence", "prevalence_rank", "prevalence_sd", 
              "n_obs", "n_species", 
              "degree", "eigen_cent", "betweenness", "modularity",
              "mean_jaccard", "mean_dice","jaccard_sd", "dice_sd",
              "mean_sorensen", "sd_sorensen", "mrf_intercept", "sum_mrfinfo",
              "sd_mrf_info", "mrf_trace", "log_determinant", "no_preds", "no_pc",
              "var_exp")

colnames(hm_dat) <- c(models)
rownames(hm_dat) <- c(features)

hm_dat <- as.data.frame(hm_dat)

#  at this point, this is an empty dataframe. We will need to calculate the relative importance from the importance values. 
# This is done by dividing each importance value by the value of the most important feature (note the column number of the most important
# feature from the position of the variable in the 'features' vector as this is the same order as the importance names)

base_imp <- ensemble_model$regrOutput$BASE$importance
base_ri <- base_imp/base_imp[1] # prevalence in position 1 
hm_dat$'BASE' <- base_ri

gbmdr_imp <- ensemble_model$regrOutput$GBM_STACK_DR$importance
gbmdr_ri <- gbmdr_imp/gbmdr_imp[7]# eigen_cent in position 7
hm_dat$'GBM-DR' <- gbmdr_ri

gbmpr_imp <- ensemble_model$regrOutput$GBM_STACK_PR$importance
gbmpr_ri <- gbmpr_imp/gbmpr_imp[1] # prevalence in position 1 
hm_dat$'GBM-PR' <- gbmpr_ri

mvrf_imp <- ensemble_model$regrOutput$MVRF$importance
mvrf_ri <- mvrf_imp/mvrf_imp[1] # prevalence in position 1 
hm_dat$'MVRF' <- mvrf_ri

hmsc_imp <- ensemble_model$regrOutput$HMSC$importance
hmsc_ri <- hmsc_imp/hmsc_imp[1] # prevalence in position 1 
hm_dat$'HMSC' <- hmsc_ri

hm_dat <- tibble::rownames_to_column(hm_dat, "Features")

hm_dat <- pivot_longer(data = hm_dat, 
                       cols = c(2:6), 
                       names_to = "Method", 
                       values_to = "RI")

hm_dat[hm_dat == "prevalence"] <- "Prevalence"
hm_dat[hm_dat == "prevalence_sd"] <- "Prevalence SD"
hm_dat[hm_dat == "prevalence_rank"] <- "Prevalence Rank"
hm_dat[hm_dat == "jaccard_sd"] <- "Mean Jaccard Distance SD"
hm_dat[hm_dat == "mean_jaccard"] <- "Mean Jaccard Distance"
hm_dat[hm_dat == "mean_dice"] <- "Mean Sørensen-Dice Distance"
hm_dat[hm_dat == "dice_sd"] <- "Mean Sørensen-Dice Distance SD"
hm_dat[hm_dat == "n_species"] <- "Number of Species"
hm_dat[hm_dat == "degree"] <- "Degree Centrality"
hm_dat[hm_dat == "eigen_cent"] <- "Eigenvector Centrality"
hm_dat[hm_dat == "n_obs"] <- "Number of Observations"
hm_dat[hm_dat == "modularity"] <- "Modularity"
hm_dat[hm_dat == "betweenness"] <- "Betweenness Centrality"
hm_dat[hm_dat == "mean_sorensen"] <- "Mean Sørensen Index"
hm_dat[hm_dat == "sd_sorensen"] <- "Sørensen Index Standard Deviation"
hm_dat[hm_dat == "mrf_intercept"] <- "MRF Intercept"
hm_dat[hm_dat == "sum_mrfinfo"] <- "MRF Network Information"
hm_dat[hm_dat == "sd_mrf_info"] <- "MRF Network Information Standard Deviation"
hm_dat[hm_dat == "mrf_trace"] <- "MRF Trace"
hm_dat[hm_dat == "log_determinant"] <- "Log Determinant"
hm_dat[hm_dat == "no_preds"] <- "Number of Predictors"
hm_dat[hm_dat == "no_pc"] <- "Number of PCs"
hm_dat[hm_dat == "var_exp"] <- "Cumulative Variability Explained by PCs"

heatmap <- ggplot(data = hm_dat, mapping = aes(x = Method,
                                               y = reorder(Features, RI),
                                               fill = RI)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_classic() +
  labs(x = "Model", y = "Feature", fill = "  Relative\nImportance") +
  theme(text = element_text(size = 15))  





############### RESPONSE FUNCTIONS ####################

ensemble_model <- readRDS("functions/ensemble_model.rds")
load("outputs/training_data_outputs/weights_and_features.rda")

ys <- weights_and_features[2:6]
xs <- weights_and_features[8:30]

all_dat <- cbind(ys, xs)


#### Visualizing response function 

all_dat$BASE_preds <- ensemble_model$regrOutput$BASE$predicted
all_dat$GBM_DR_preds <- ensemble_model$regrOutput$GBM_STACK_DR$predicted
all_dat$GBM_PR_preds <- ensemble_model$regrOutput$GBM_STACK_PR$predicted
all_dat$MVRF_preds <- ensemble_model$regrOutput$MVRF$predicted
all_dat$HMSC_preds <- ensemble_model$regrOutput$HMSC$predicted


#### Prevalence #####

new_prevalence <- seq(min(all_dat$prevalence), max(all_dat$prevalence), length.out = 1000)

newdata <- data.frame(prevalence = new_prevalence,
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species), 
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = prevalence, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()


test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, prevalence)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

My_Theme = theme(
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  legend.title = element_text(size=15),
  legend.text = element_text(size=14))

plot <- test %>%
  ggplot(aes(x = prevalence, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Prevalence", color = "Model") +
  My_Theme
plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### Prevalence Rank #####

new_prevalence_rank <- seq(min(all_dat$prevalence_rank), max(all_dat$prevalence_rank), length.out = 1000)

newdata <- data.frame(prevalence_rank = new_prevalence_rank,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species), 
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = prevalence_rank, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence_rank, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence_rank, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence_rank, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence_rank, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()


test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, prevalence_rank)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = prevalence_rank, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Prevalence Rank", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))



#### Prevalence SD #####

new_prevalence_sd <- seq(min(all_dat$prevalence_sd), max(all_dat$prevalence_sd), length.out = 1000)

newdata <- data.frame(prevalence_sd = new_prevalence_sd,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species), 
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = prevalence_sd, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence_sd, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence_sd, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence_sd, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = prevalence_sd, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()


test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, prevalence_sd)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot <- test %>%
  ggplot(aes(x = prevalence_sd, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,0.3) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Prevalence Standard Deviation", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))


#### Number of Observations #####

new_n_obs <- seq(min(all_dat$n_obs), max(all_dat$n_obs), length.out = 1000)

newdata <- data.frame(n_obs = new_n_obs,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_species = mean(all_dat$n_species), 
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = n_obs, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = n_obs, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = n_obs, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = n_obs, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = n_obs, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, n_obs)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 


cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot <- test %>%
  ggplot(aes(x = n_obs, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,8958) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Number of Observations", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))




#### Number of Species #####

new_n_species <- seq(min(all_dat$n_species), max(all_dat$n_species), length.out = 1000)

newdata <- data.frame(n_species = new_n_species,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = n_species, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = n_species, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = n_species, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = n_species, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = n_species, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, n_species)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = n_species, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,242) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Number of Species", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))


#### Degree #####

new_degree <- seq(min(all_dat$degree), max(all_dat$degree), length.out = 1000)

newdata <- data.frame(degree = new_degree,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = degree, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = degree, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = degree, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = degree, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = degree, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, degree)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot <- test %>%
  ggplot(aes(x = degree, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Degree Centrality", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))



#### Eigenvector Centrality #####

new_eigen_cent <- seq(min(all_dat$eigen_cent), max(all_dat$eigen_cent), length.out = 1000)

newdata <- data.frame(eigen_cent = new_eigen_cent,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = eigen_cent, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = eigen_cent, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = eigen_cent, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = eigen_cent, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = eigen_cent, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, eigen_cent)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot <- test %>%
  ggplot(aes(x = eigen_cent, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Eigenvector Centrality", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))


#### Betweenness Centrality #####

new_betweenness <- seq(min(all_dat$betweenness), max(all_dat$betweenness), length.out = 1000)

newdata <- data.frame(betweenness = new_betweenness,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = betweenness, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = betweenness, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = betweenness, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = betweenness, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = betweenness, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()


test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, betweenness)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = betweenness, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,0.8) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Betweenness Centrality", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))



#### mean_jaccard #####

new_mean_jaccard <- seq(min(all_dat$mean_jaccard), max(all_dat$mean_jaccard), length.out = 1000)

newdata <- data.frame(mean_jaccard = new_mean_jaccard,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = mean_jaccard, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_jaccard, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_jaccard, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_jaccard, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_jaccard, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, mean_jaccard)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = mean_jaccard, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0.8,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Mean Jaccard Distance", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))




#### mean_dice #####

new_mean_dice <- seq(min(all_dat$mean_dice), max(all_dat$mean_dice), length.out = 1000)

newdata <- data.frame(mean_dice = new_mean_dice,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = mean_dice, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_dice, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_dice, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_dice, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_dice, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, mean_dice)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot <- test %>%
  ggplot(aes(x = mean_dice, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0.65,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Mean Sørensen-Dice Distance", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))



#### mean_jaccard_sd #####

new_jaccard_sd <- seq(min(all_dat$jaccard_sd), max(all_dat$jaccard_sd), length.out = 1000)

newdata <- data.frame(jaccard_sd = new_jaccard_sd,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))
preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = jaccard_sd, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = jaccard_sd, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = jaccard_sd, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = jaccard_sd, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = jaccard_sd, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()


test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, jaccard_sd)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC", GJAM_preds="GJAM")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot <- test %>%
  ggplot(aes(x = jaccard_sd, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,0.05) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Mean Jaccard Distance Standard Deviation", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))



#### mean_dice_sd #####

new_dice_sd <- seq(min(all_dat$dice_sd), max(all_dat$dice_sd), length.out = 1000)

newdata <- data.frame(dice_sd = new_dice_sd,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted


#### plots ####
ggplot(newdata, aes(x = dice_sd, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = dice_sd, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = dice_sd, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = dice_sd, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = dice_sd, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = dice_sd, y = GJAM_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()


test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, dice_sd)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot <- test %>%
  ggplot(aes(x = dice_sd, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,0.075) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Mean Sørensen-Dice Distance Standard Deviation", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))


#### modularity #####

new_modularity <- seq(min(all_dat$modularity), max(all_dat$modularity), length.out = 1000)

newdata <- data.frame(modularity = new_modularity,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = modularity, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = modularity, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = modularity, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = modularity, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = modularity, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, modularity)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = modularity, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(-0.8,0.25) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Modularity", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))



#### mean_sorensen #####

new_mean_sorensen <- seq(min(all_dat$mean_sorensen), max(all_dat$mean_sorensen), length.out = 1000)

newdata <- data.frame(mean_sorensen = new_mean_sorensen,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = mean_sorensen, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_sorensen, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_sorensen, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_sorensen, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mean_sorensen, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, mean_sorensen)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = mean_sorensen, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Mean Sørensen Index", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### sorensen_sd #####

new_sd_sorensen <- seq(min(all_dat$sd_sorensen), max(all_dat$sd_sorensen), length.out = 1000)

newdata <- data.frame(sd_sorensen = new_sd_sorensen,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = sd_sorensen, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sd_sorensen, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sd_sorensen, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sd_sorensen, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sd_sorensen, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, sd_sorensen)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = sd_sorensen, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Sørensen Index Standard Deviation", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### mrf_intercept #####

new_mrf_intercept <- seq(min(all_dat$mrf_intercept), max(all_dat$mrf_intercept), length.out = 1000)

newdata <- data.frame(mrf_intercept = new_mrf_intercept,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = mrf_intercept, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mrf_intercept, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mrf_intercept, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mrf_intercept, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mrf_intercept, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, mrf_intercept)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = mrf_intercept, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="MRF Intercept", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### sum_mrfinfo #####

new_sum_mrfinfo <- seq(min(all_dat$sum_mrfinfo), max(all_dat$sum_mrfinfo), length.out = 1000)

newdata <- data.frame(sum_mrfinfo = new_sum_mrfinfo,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = sum_mrfinfo, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sum_mrfinfo, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sum_mrfinfo, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sum_mrfinfo, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sum_mrfinfo, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, sum_mrfinfo)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = sum_mrfinfo, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="MRF Network Information", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### sd_mrfinfo #####

new_sd_mrfinfo <- seq(min(all_dat$sd_mrfinfo), max(all_dat$sd_mrfinfo), length.out = 1000)

newdata <- data.frame(sd_mrfinfo = new_sd_mrfinfo,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept), 
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = sd_mrfinfo, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sd_mrfinfo, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sd_mrfinfo, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sd_mrfinfo, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = sd_mrfinfo, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, sd_mrfinfo)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = sd_mrfinfo, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="MRF Network Information Standard Deviation", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### mrf_trace #####

new_mrf_trace <- seq(min(all_dat$mrf_trace), max(all_dat$mrf_trace), length.out = 1000)

newdata <- data.frame(mrf_trace = new_mrf_trace,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept), 
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo),
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = mrf_trace, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mrf_trace, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mrf_trace, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mrf_trace, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = mrf_trace, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, mrf_trace)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = mrf_trace, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="MRF Trace", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### log_determinant #####

new_log_determinant <- seq(min(all_dat$log_determinant), max(all_dat$log_determinant), length.out = 1000)

newdata <- data.frame(log_determinant = new_log_determinant,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept), 
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo),
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = log_determinant, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = log_determinant, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = log_determinant, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = log_determinant, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = log_determinant, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, log_determinant)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = log_determinant, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Log Determinant", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### no_preds #####

new_no_preds <- seq(min(all_dat$no_preds), max(all_dat$no_preds), length.out = 1000)

newdata <- data.frame(no_preds = new_no_preds,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept), 
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo),
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace), 
                      log_determinant = mean(all_dat$log_determinant), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = no_preds, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = no_preds, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = no_preds, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = no_preds, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = no_preds, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, no_preds)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = no_preds, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Number of Predictors", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

#### no_pc #####

new_no_pc <- seq(min(all_dat$no_pc), max(all_dat$no_pc), length.out = 5)

newdata <- data.frame(no_pc = new_no_pc,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept), 
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo),
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace), 
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      var_exp = mean(all_dat$var_exp))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = no_pc, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = no_pc, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = no_pc, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = no_pc, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = no_pc, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, no_pc)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = no_pc, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Number of PCs", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))


#### var_exp #####

new_var_exp <- seq(min(all_dat$var_exp), max(all_dat$var_exp), length.out = 1000)

newdata <- data.frame(var_exp = new_var_exp,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept), 
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo),
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace), 
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc))


preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

#### plots ####
ggplot(newdata, aes(x = var_exp, y = BASE_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = var_exp, y = GBM_DR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = var_exp, y = GBM_PR_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = var_exp, y = MVRF_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()

ggplot(newdata, aes(x = var_exp, y = HMSC_preds)) + 
  geom_point() + 
  geom_smooth() + 
  theme_classic()



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, var_exp)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")


plot <- test %>%
  ggplot(aes(x = var_exp, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Cumulative Variation Explained by PCs", color = "Model") +
  My_Theme

plot + guides(color=guide_legend(override.aes=list(fill=NA)))

####### Panel of three response functions: prevalence, eigen_cent, and mean_dice (top three) ############## 

#### Prevalence #####

new_prevalence <- seq(min(all_dat$prevalence), max(all_dat$prevalence), length.out = 1000)

newdata <- data.frame(prevalence = new_prevalence,
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species), 
                      degree = mean(all_dat$degree),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, prevalence)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="GLM-BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

My_Theme = theme(
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  legend.title = element_text(size=25),
  legend.text = element_text(size=20),
  legend.key = element_blank())

plot_1 <- test %>%
  ggplot(aes(x = prevalence, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Prevalence", color = "Model") +
  My_Theme +
  theme(legend.key = element_blank(),
        legend.key.size = unit(1.0, "cm"))

plot_1 <- plot_1 + guides(color=guide_legend(override.aes=list(fill=NA)))

#### Eigenvector Centrality #####

new_eigen_cent <- seq(min(all_dat$eigen_cent), max(all_dat$eigen_cent), length.out = 1000)

newdata <- data.frame(eigen_cent = new_eigen_cent,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      degree = mean(all_dat$degree),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted



test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, eigen_cent)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="GLM-BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot_2 <- test %>%
  ggplot(aes(x = eigen_cent, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Eigenvector Centrality", color = "Model") +
  My_Theme +
  theme(legend.position="none")

plot_2 + guides(color=guide_legend(override.aes=list(fill=NA)))


new_degree <- seq(min(all_dat$degree), max(all_dat$degree), length.out = 1000)

newdata <- data.frame(degree = new_degree,
                      prevalence = mean(all_dat$prevalence), 
                      prevalence_sd = mean(all_dat$prevalence_sd), 
                      prevalence_rank = mean(all_dat$prevalence_rank), 
                      n_obs = mean(all_dat$n_obs), 
                      n_species = mean(all_dat$n_species),
                      eigen_cent = mean(all_dat$eigen_cent),
                      betweenness = mean(all_dat$betweenness),
                      mean_jaccard = mean(all_dat$mean_jaccard),
                      mean_dice = mean(all_dat$mean_dice),
                      jaccard_sd = mean(all_dat$jaccard_sd),
                      dice_sd = mean(all_dat$dice_sd),
                      modularity = mean(all_dat$modularity),
                      mean_sorensen = mean(all_dat$mean_sorensen),
                      sd_sorensen = mean(all_dat$sd_sorensen),
                      mrf_intercept = mean(all_dat$mrf_intercept),
                      sum_mrfinfo = mean(all_dat$sum_mrfinfo), 
                      sd_mrfinfo = mean(all_dat$sd_mrfinfo),
                      mrf_trace = mean(all_dat$mrf_trace),
                      log_determinant = mean(all_dat$log_determinant), 
                      no_preds = mean(all_dat$no_preds), 
                      no_pc = mean(all_dat$no_pc), 
                      var_exp = mean(all_dat$var_exp))

preds <- predict(ensemble_model, newdata = newdata)

newdata$BASE_preds <- preds$regrOutput$BASE$predicted
newdata$GBM_DR_preds <- preds$regrOutput$GBM_STACK_DR$predicted
newdata$GBM_PR_preds <- preds$regrOutput$GBM_STACK_PR$predicted
newdata$MVRF_preds <- preds$regrOutput$MVRF$predicted
newdata$HMSC_preds <- preds$regrOutput$HMSC$predicted

test <- newdata %>% 
  select(BASE_preds, GBM_DR_preds, GBM_PR_preds, MVRF_preds, HMSC_preds, degree)

test <- pivot_longer(test, cols = 1:5)

test <- test %>% 
  rename('weight_pred' = 'value',
         'model' = 'name')

test <- test[, c(2, 3, 1)]

model_names <- c(BASE_preds="BASE", GBM_DR_preds="GBM-DR", GBM_PR_preds="GBM-PR",
                 MVRF_preds="MVRF", HMSC_preds="HMSC")

test$model <- as.character(model_names[test$model])

#so now we have a clean dataset with model name, weight of prediction and prevalence in one column each. 

cp1 <- c("#0c2a50ff", "403891ff", "#b8627dff", "#f68f46ff", "#efe350ff")

plot_3 <- test %>%
  ggplot(aes(x = degree, y = weight_pred, color = model))+
  geom_smooth(size = 1.0) +
  xlim(0,1) +
  scale_colour_manual(values=cp1) + 
  theme_bw() + 
  labs(y="Predicted Model Weight", x="Degree Centrality", color = "Model") +
  My_Theme +
  theme(legend.position="none")

plot_3 + guides(color=guide_legend(override.aes=list(fill=NA)))


g_legend<-function(plot_1){
  tmp <- ggplot_gtable(ggplot_build(plot_1))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend <- get_legend(plot_1)

plot_1 <- plot_1 + theme(legend.position="none")

plot_grid(plot_1, plot_2, plot_3, legend, labels=c("A", "B", "C"), ncol = 2, nrow = 2)



