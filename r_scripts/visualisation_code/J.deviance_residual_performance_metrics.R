# For calculating deviance residuals from each model/species combo
# and ranking models

setwd("C:/.../MEE_feature_ensemble")

source('functions/stacking_function.R')
source('functions/dr_metric_function.R')
############################################ Calculating Deviance Residuals for each Testing Dataset ###########################################

### Dataset 1. bird_parasites #####
load("outputs/test_predictions/bird_parasites_predictions.rda")

predictions <- bird_parasites_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

bird_parasites_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]

  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
  }))

save(bird_parasites_dr, file = "outputs/test_predictions/bird_parasites_dr.rda")



### Dataset 4: uk_butterflies ####

load("outputs/test_predictions/uk_butterflies_predictions.rda")

predictions <- uk_butterflies_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

uk_butterflies_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(uk_butterflies_dr, file = "outputs/test_predictions/uk_butterflies_dr.rda")
### Dataset 7: norway_vegetation #### 

load("outputs/test_predictions/norway_vegetation_predictions.rda")

predictions <- norway_vegetation_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

norway_vegetation_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(norway_vegetation_dr, file = "outputs/test_predictions/norway_vegetation_dr.rda")
### Dataset 13: grassland_birds ####

load("outputs/test_predictions/grassland_birds_predictions.rda")

predictions <- grassland_birds_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

grassland_birds_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(grassland_birds_dr, file = "outputs/test_predictions/grassland_birds_dr.rda")
### Dataset 14: mulu_birds ####

load("outputs/test_predictions/mulu_birds_predictions.rda")

predictions <- mulu_birds_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

mulu_birds_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(mulu_birds_dr, file = "outputs/test_predictions/mulu_birds_dr.rda")
### Dataset 18: swiss_birds ####

load("outputs/test_predictions/swiss_birds_predictions.rda")

predictions <- swiss_birds_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

swiss_birds_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(swiss_birds_dr, file = "outputs/test_predictions/swiss_birds_dr.rda")
### Dataset 23: earthworms #### 

load("outputs/test_predictions/earthworms_predictions.rda")

predictions <- earthworms_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

earthworms_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(earthworms_dr, file = "outputs/test_predictions/earthworms_dr.rda")
### Dataset 24: vines ####

load("outputs/test_predictions/vines_predictions.rda")

predictions <- vines_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

vines_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(vines_dr, file = "outputs/test_predictions/vines_dr.rda")
### Dataset 26: andean_birds ####

load("outputs/test_predictions/andean_birds_predictions.rda")

predictions <- andean_birds_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

andean_birds_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(andean_birds_dr, file = "outputs/test_predictions/andean_birds_dr.rda")
### Dataset 29: norway_beetles ####

load("outputs/test_predictions/norway_beetles_predictions.rda")

predictions <- norway_beetles_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

norway_beetles_dr <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  glm_base_resids <- dr_metric(truth = sp_truth, model_prob_preds$GLM_BASE[,x])
  gbm_dr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_DR[,x])
  gbm_pr_resids <- dr_metric(truth = sp_truth, model_prob_preds$GBM_PR[,x])
  hmsc_resids <- dr_metric(truth = sp_truth, model_prob_preds$HMSC[,x])
  mvrf_resids <- dr_metric(truth = sp_truth, model_prob_preds$MVRF[,x])
  null_ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$NULL_ENS[,x])
  ens_resids <- dr_metric(truth = sp_truth, model_prob_preds$ENS[,x])
  
  mod_ranks <- rank(c(glm_base_resids, gbm_dr_resids, gbm_pr_resids, hmsc_resids, mvrf_resids, null_ens_resids, ens_resids),
                    ties.method = 'average')
  
  data.frame(rank = floor(mod_ranks),
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(norway_beetles_dr, file = "outputs/test_predictions/norway_beetles_dr.rda")




########################################## Summarising Deviance Residual Ranking ###############################################################

load("outputs/test_predictions/bird_parasites_dr.rda")
load("outputs/test_predictions/uk_butterflies_dr.rda")
load("outputs/test_predictions/norway_vegetation_dr.rda")
load("outputs/test_predictions/grassland_birds_dr.rda")
load("outputs/test_predictions/mulu_birds_dr.rda")
load("outputs/test_predictions/swiss_birds_dr.rda")
load("outputs/test_predictions/earthworms_dr.rda")
load("outputs/test_predictions/vines_dr.rda")
load("outputs/test_predictions/andean_birds_dr.rda")
load("outputs/test_predictions/norway_beetles_dr.rda")

all_dr_metrics <- rbind(bird_parasites_dr, 
                        uk_butterflies_dr, 
                        norway_vegetation_dr,
                        grassland_birds_dr,
                        mulu_birds_dr, 
                        swiss_birds_dr,
                        earthworms_dr,
                        vines_dr,
                        andean_birds_dr,
                        norway_beetles_dr)

testing_data_dr_metric <- pivot_wider(all_dr_metrics, id_cols = species, names_from = model, values_from = rank)

save(testing_data_dr_metric, file = "outputs/test_predictions/testing_data_dr_metric.rda")

table(testing_data_dr_metric$GLM_BASE)
table(testing_data_dr_metric$GBM_DR)
table(testing_data_dr_metric$GBM_PR)
table(testing_data_dr_metric$MVRF)
table(testing_data_dr_metric$HMSC)
table(testing_data_dr_metric$ENS)
table(testing_data_dr_metric$NULL_ENS)

summary(testing_data_dr_metric$GLM_BASE)
summary(testing_data_dr_metric$GBM_DR)
summary(testing_data_dr_metric$GBM_PR)
summary(testing_data_dr_metric$MVRF)
summary(testing_data_dr_metric$HMSC)
summary(testing_data_dr_metric$ENS)
summary(testing_data_dr_metric$NULL_ENS)
########################################## Summarising performance against GLM-BASE ##################################################

load("outputs/test_predictions/testing_data_dr_metric.rda")

testing_data_dr_metric$GBM_DR_adj <- testing_data_dr_metric$GBM_DR - testing_data_dr_metric$GLM_BASE
testing_data_dr_metric$GBM_PR_adj <- testing_data_dr_metric$GBM_PR - testing_data_dr_metric$GLM_BASE
testing_data_dr_metric$MVRF_adj <- testing_data_dr_metric$MVRF - testing_data_dr_metric$GLM_BASE
testing_data_dr_metric$HMSC_adj <- testing_data_dr_metric$HMSC - testing_data_dr_metric$GLM_BASE
testing_data_dr_metric$NULL_ENS_adj <- testing_data_dr_metric$NULL_ENS - testing_data_dr_metric$GLM_BASE
testing_data_dr_metric$ENS_adj <- testing_data_dr_metric$ENS - testing_data_dr_metric$GLM_BASE


dr_metric <- testing_data_dr_metric %>% 
  select(GBM_DR_adj, GBM_PR_adj, MVRF_adj, HMSC_adj, NULL_ENS_adj, ENS_adj)

dr_metric <-pivot_longer(dr_metric, cols = 1:6, names_to = "model", values_to = "dr")

dr_metric %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(Total_better = length(which(dr < 0)),
                   Total_nochange = length(which(dr == 0)),
                   Total_worse = length(which(dr > 0)))
