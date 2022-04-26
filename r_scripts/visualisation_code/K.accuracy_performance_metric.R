if(!require(StatMeasures)){
  install.packages('StatMeasures')
}

setwd("C:/.../MEE_feature_ensemble")

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
###################################################### Calculate accuracy ###################################################################

### Dataset 1. bird_parasites #####
load("outputs/test_predictions/bird_parasites_predictions.rda")

predictions <- bird_parasites_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth

rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

bird_parasites_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1

  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1

  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(bird_parasites_accuracy, file = "outputs/test_predictions/bird_parasites_accuracy.rda")


### Dataset 4: uk_butterflies ####

load("outputs/test_predictions/uk_butterflies_predictions.rda")

predictions <- uk_butterflies_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

uk_butterflies_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(uk_butterflies_accuracy, file = "outputs/test_predictions/uk_butterflies_accuracy.rda")

### Dataset 7: norway_vegetation #### 

load("outputs/test_predictions/norway_vegetation_predictions.rda")

predictions <- norway_vegetation_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

norway_vegetation_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(norway_vegetation_accuracy, file = "outputs/test_predictions/norway_vegetation_accuracy.rda")


### Dataset 13: grassland_birds ####

load("outputs/test_predictions/grassland_birds_predictions.rda")

predictions <- grassland_birds_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 


rm_species <- which(colSums(truth[,1:length(truth)]) == 0)
                    
if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

grassland_birds_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))


save(grassland_birds_accuracy, file = "outputs/test_predictions/grassland_birds_accuracy.rda")

### Dataset 14: mulu_birds ####

load("outputs/test_predictions/mulu_birds_predictions.rda")

predictions <- mulu_birds_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 


rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

mulu_birds_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(mulu_birds_accuracy, file = "outputs/test_predictions/mulu_birds_accuracy.rda")

### Dataset 18: swiss_birds ####

load("outputs/test_predictions/swiss_birds_predictions.rda")

predictions <- swiss_birds_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

swiss_birds_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(swiss_birds_accuracy, file = "outputs/test_predictions/swiss_birds_accuracy.rda")

### Dataset 23: earthworms #### 

load("outputs/test_predictions/earthworms_predictions.rda")

predictions <- earthworms_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}
earthworms_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(earthworms_accuracy, file = "outputs/test_predictions/earthworms_accuracy.rda")

### Dataset 24: vines ####

load("outputs/test_predictions/vines_predictions.rda")

predictions <- vines_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

vines_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(vines_accuracy, file = "outputs/test_predictions/vines_accuracy.rda")

### Dataset 26: andean_birds ####

load("outputs/test_predictions/andean_birds_predictions.rda")

predictions <- andean_birds_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

andean_birds_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(andean_birds_accuracy, file = "outputs/test_predictions/andean_birds_accuracy.rda")

### Dataset 29: norway_beetles ####

load("outputs/test_predictions/norway_beetles_predictions.rda")

predictions <- norway_beetles_predictions

model_prob_preds <- predictions$raw_probability_predictions
truth <- predictions$truth 

rm_species <- which(colSums(truth[,1:length(truth)]) == 0)

if(length(rm_species) > 0) {
  warning('species removed as no presences in true binary outcomes')
  truth <- truth[, -(rm_species)]   
}

norway_beetles_accuracy <- do.call(rbind,lapply(seq_len(NCOL(truth)), function(x){ 
  sp_truth <- truth[,x]
  
  if(any(model_prob_preds$GLM_BASE < 0)){
    warning('some probability predictions for GLM-BASE < 0; changing to 0')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE < 0] <- 0
  }  
  
  if(any(model_prob_preds$GLM_BASE > 1)){
    warning('some probability predictions for GLM-BASE > 1; changing to 1')
    model_prob_preds$GLM_BASE[model_prob_preds$GLM_BASE >1] <- 1
  }
  
  glm_base_accuracy <- accuracy(sp_truth, model_prob_preds$GLM_BASE[,x], 0.5)
  
  if(any(model_prob_preds$GBM_DR < 0)){
    warning('some probability predictions for GBM-DR < 0; changing to 0')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_DR > 1)){
    warning('some probability predictions for GBM-DR > 1; changing to 1')
    model_prob_preds$GBM_DR[model_prob_preds$GBM_DR >1] <- 1
  }
  
  gbm_dr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_DR[,x], 0.5)
  
  if(any(model_prob_preds$GBM_PR < 0)){
    warning('some probability predictions for GBM-PR < 0; changing to 0')
    model_prob_preds$GBM_PR [model_prob_preds$GBM_PR < 0] <- 0
  }  
  
  if(any(model_prob_preds$GBM_PR > 1)){
    warning('some probability predictions for GBM-PR > 1; changing to 1')
    model_prob_preds$GBM_PR[model_prob_preds$GBM_PR >1] <- 1
    
  }
  
  gbm_pr_accuracy <- accuracy(sp_truth, model_prob_preds$GBM_PR[,x], 0.5)
  
  if(any(model_prob_preds$HMSC < 0)){
    warning('some probability predictions for HMSC < 0; changing to 0')
    model_prob_preds$HMSC [model_prob_preds$HMSC < 0] <- 0
  }  
  
  if(any(model_prob_preds$HMSC > 1)){
    warning('some probability predictions for HMSC > 1; changing to 1')
    model_prob_preds$HMSC[model_prob_preds$HMSC >1] <- 1
    
  } 
  
  hmsc_accuracy <- accuracy(sp_truth, model_prob_preds$HMSC[,x], 0.5)
  
  if(any(model_prob_preds$MVRF < 0)){
    warning('some probability predictions for MVRF < 0; changing to 0')
    model_prob_preds$MVRF [model_prob_preds$MVRF < 0] <- 0
  }  
  
  if(any(model_prob_preds$MVRF > 1)){
    warning('some probability predictions for MVRF > 1; changing to 1')
    model_prob_preds$MVRF[model_prob_preds$MVRF >1] <- 1
    
  } 
  mvrf_accuracy <- accuracy(sp_truth, model_prob_preds$MVRF[,x], 0.5)
  
  if(any(model_prob_preds$NULL_ENS < 0)){
    warning('some probability predictions for NULL_ENS < 0; changing to 0')
    model_prob_preds$NULL_ENS [model_prob_preds$NULL_ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$NULL_ENS > 1)){
    warning('some probability predictions for NULL_ENS > 1; changing to 1')
    model_prob_preds$NULL_ENS[model_prob_preds$NULL_ENS >1] <- 1
  } 
  
  null_ens_accuracy <- accuracy(sp_truth, model_prob_preds$NULL_ENS[,x], 0.5)
  
  if(any(model_prob_preds$ENS < 0)){
    warning('some probability predictions for ENS < 0; changing to 0')
    model_prob_preds$ENS [model_prob_preds$ENS < 0] <- 0
  }  
  
  if(any(model_prob_preds$ENS > 1)){
    warning('some probability predictions for ENS > 1; changing to 1')
    model_prob_preds$ENS[model_prob_preds$ENS >1] <- 1
    
  }
  
  ens_accuracy <- accuracy(sp_truth, model_prob_preds$ENS[,x], 0.5)
  
  mod_accuracy <- c(glm_base_accuracy$overallAcc, gbm_dr_accuracy$overallAcc, gbm_pr_accuracy$overallAcc, hmsc_accuracy$overallAcc, mvrf_accuracy$overallAcc, null_ens_accuracy$overallAcc, ens_accuracy$overallAcc)
  
  data.frame(accuracy = mod_accuracy,
             model = c('GLM_BASE', 'GBM_DR', 'GBM_PR', 'HMSC','MVRF', 'NULL_ENS','ENS'),
             species = colnames(truth)[x])
  
  
}))

save(norway_beetles_accuracy, file = "outputs/test_predictions/norway_beetles_accuracy.rda")


#################################################### Summarise performance ###################################################################

load("outputs/test_predictions/bird_parasites_accuracy.rda")
load("outputs/test_predictions/uk_butterflies_accuracy.rda")
load("outputs/test_predictions/norway_vegetation_accuracy.rda")
load("outputs/test_predictions/grassland_birds_accuracy.rda")
load("outputs/test_predictions/mulu_birds_accuracy.rda")
load("outputs/test_predictions/swiss_birds_accuracy.rda")
load("outputs/test_predictions/earthworms_accuracy.rda")
load("outputs/test_predictions/vines_accuracy.rda")
load("outputs/test_predictions/andean_birds_accuracy.rda")
load("outputs/test_predictions/norway_beetles_accuracy.rda")


all_accuracy_metrics <- rbind(bird_parasites_accuracy, 
                        uk_butterflies_accuracy, 
                        norway_vegetation_accuracy,
                        grassland_birds_accuracy,
                        mulu_birds_accuracy, 
                        swiss_birds_accuracy,
                        earthworms_accuracy,
                        vines_accuracy,
                        andean_birds_accuracy,
                        norway_beetles_accuracy)
library(tidyverse)
testing_data_accuracy_metric <- pivot_wider(all_accuracy_metrics, id_cols = species, names_from = model, values_from = accuracy)


testing_data_accuracy_metric$GBM_DR_adj <- testing_data_accuracy_metric$GBM_DR - testing_data_accuracy_metric$GLM_BASE
testing_data_accuracy_metric$GBM_PR_adj <- testing_data_accuracy_metric$GBM_PR - testing_data_accuracy_metric$GLM_BASE
testing_data_accuracy_metric$MVRF_adj <- testing_data_accuracy_metric$MVRF - testing_data_accuracy_metric$GLM_BASE
testing_data_accuracy_metric$HMSC_adj <- testing_data_accuracy_metric$HMSC - testing_data_accuracy_metric$GLM_BASE
testing_data_accuracy_metric$NULL_ENS_adj <- testing_data_accuracy_metric$NULL_ENS - testing_data_accuracy_metric$GLM_BASE
testing_data_accuracy_metric$ENS_adj <- testing_data_accuracy_metric$ENS - testing_data_accuracy_metric$GLM_BASE


accuracy_metric <- testing_data_accuracy_metric %>% 
  select(GBM_DR_adj, GBM_PR_adj, MVRF_adj, HMSC_adj, NULL_ENS_adj, ENS_adj)

accuracy_metric <-pivot_longer(accuracy_metric, cols = 1:6, names_to = "model", values_to = "accuracy")

accuracy_metric %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(Total_better = length(which(accuracy > 0)),
                   Total_nochange = length(which(accuracy == 0)),
                   Total_worse = length(which(accuracy < 0)))
