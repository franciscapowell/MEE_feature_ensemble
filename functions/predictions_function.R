predictions_function <- function(xs, ys, all_dat, features, split_prop =.7) {
    library(parallel)
    in.train <- sample(1:nrow(ys), 
                       length(1:round(nrow(ys) * split_prop)), 
                       replace = F)
    
    train_data <- all_dat[in.train, ]
    test_data <- all_dat[-in.train, ]
    
    rm_species <- which(colSums(train_data[,1:length(ys)]) == 0 |
                          colSums(train_data[,1:length(ys)]) / nrow(train_data) > 0.95)
    
    if(length(rm_species) > 0) {
    train_data <- train_data[, -(rm_species)]
    test_data <- test_data[, -(rm_species)]
    ys <- ys[, -(rm_species)]
    sp_names <- colnames(ys)
    } else{
      sp_names <- colnames(ys)
    }
    yhats <- lapply(colnames(ys), function(species){
      mod1 <- glm(formula(paste0(species,'~', paste0(colnames(xs), collapse = '+'))), 
                  family = 'binomial', data = train_data)
      yhat <- predict(mod1, type = 'response')
      
      # Calculate residuals and return
      list(mod1 = mod1, yhat = yhat, 
           deviance_resid = deviance_resids(yhat, train_data[, species]), 
           pearson_resid = pearson_resids(yhat, train_data[, species]))
    })
    names(yhats) <- colnames(sp_names)
    
    output <- compare_all_models(yhats, train_data, test_data, 
                                            rfsrc_formula = formula(paste0('cbind(',
                                                                           paste0(colnames(ys), 
                                                                                  collapse = ','),') ~ .')), 
                                           preds_only = TRUE)

 #  train_features = feature_function(train_data[1:length(sp_names)])
   
 train_features <- features[features$species %in% sp_names, ]
 
    pred_weight <- predict(ensemble_model, newdata = train_features)
    pred_weight <- do.call(rbind,purrr::map(pred_weight$regrOutput, 'predicted'))
    colnames(pred_weight) <- sp_names
    rownames(pred_weight) <- c("BASE" ,
                               "GBM_DR",
                               "GBM_PR",
                               "HMSC"   ,
                               "MVRF" )
    pred_weight <- pred_weight[names(output),]
    
    pred_weight_norm <- matrix(NA, nrow = nrow(pred_weight), ncol = ncol(pred_weight))
    for(i in 1:ncol(pred_weight)){
      pred_weight_norm[,i] <- pred_weight[, i] / sum(pred_weight[,i])
    }
    pred_weight <- pred_weight_norm
  
    # Set up for loop to calculate ensemble predictions
    ens_preds <- matrix(NA, nrow = nrow(test_data), ncol = ncol(output$BASE))
    for(i in 1:length(sp_names)){
      ens_preds[,i] <- output$BASE[,i] * pred_weight[1,i] +
        output$GBM_DR[,i] * pred_weight[2,i] +
        output$GBM_PR[,i] * pred_weight[3,i] +
        output$MVRF[,i] * pred_weight[4,i] +
        output$HMSC[,i] * pred_weight[5,i]
    }
    colnames(ens_preds) <- sp_names
    
    # Null ensemble predictions 
    null_ens_preds <- matrix(NA, nrow = nrow(test_data), ncol = ncol(output$BASE))
    for(i in 1:length(sp_names)){
      null_ens_preds[,i] <- output$BASE[,i] * 1/6 +
        output$GBM_DR[,i] * 1/6 +
        output$GBM_PR[,i] * 1/6 +
        output$MVRF[,i] * 1/6 +
        output$HMSC[,i] * 1/6 
    }
    colnames(null_ens_preds) <- sp_names
    
    # BASE predictions 
    base_preds <- matrix(NA, nrow = nrow(test_data), ncol = ncol(output$BASE))
    for(i in 1:length(sp_names)){
      base_preds[,i] <- output$BASE[,i]
    }
    colnames(base_preds) <- sp_names
    
    # GBM_DR predictions 
    gbm_dr_preds <- matrix(NA, nrow = nrow(test_data), ncol = ncol(output$BASE))
    for(i in 1:length(sp_names)){
      gbm_dr_preds[,i] <- output$GBM_DR[,i]
}
      colnames(gbm_dr_preds) <- sp_names
      
      # GBM_PR predictions 
      gbm_pr_preds <- matrix(NA, nrow = nrow(test_data), ncol = ncol(output$BASE))
      for(i in 1:length(sp_names)){
        gbm_pr_preds[,i] <- output$GBM_PR[,i]
      }
      colnames(gbm_pr_preds) <- sp_names
      
      # MVRF predictions 
      mvrf_preds <- matrix(NA, nrow = nrow(test_data), ncol = ncol(output$BASE))
      for(i in 1:length(sp_names)){
        mvrf_preds[,i] <- output$MVRF[,i]
      }
      colnames(mvrf_preds) <- sp_names
      
      # HMSC predictions 
      hmsc_preds <- matrix(NA, nrow = nrow(test_data), ncol = ncol(output$BASE))
      for(i in 1:length(sp_names)){
        hmsc_preds[,i] <- output$HMSC[,i]
      }
      colnames(hmsc_preds) <- sp_names
      

    # Binarize each set of preds and calculate F score, sensitivity etc...
    # make sure the binary vectors are factors with levels 0 and 1
    ens_preds_binary <- ens_preds
    ens_preds_binary[ens_preds_binary>=0.5] <-  1
    ens_preds_binary[ens_preds_binary<0.5] <-  0
      
    null_ens_preds_binary <- null_ens_preds
    null_ens_preds_binary[null_ens_preds_binary>=0.5] <-  1
    null_ens_preds_binary[null_ens_preds_binary<0.5] <-  0
    
    
    base_preds_binary <- base_preds
    base_preds_binary[base_preds_binary>=0.5] <-  1
    base_preds_binary[base_preds_binary<0.5] <-  0
    
    
    gbm_dr_preds_binary <- gbm_dr_preds
    gbm_dr_preds_binary[gbm_dr_preds_binary>=0.5] <-  1
    gbm_dr_preds_binary[gbm_dr_preds_binary<0.5] <-  0
    
    gbm_pr_preds_binary <- gbm_pr_preds
    gbm_pr_preds_binary[gbm_pr_preds_binary>=0.5] <-  1
    gbm_pr_preds_binary[gbm_pr_preds_binary<0.5] <-  0
    
    mvrf_preds_binary <- mvrf_preds
    mvrf_preds_binary[mvrf_preds_binary>=0.5] <-  1
    mvrf_preds_binary[mvrf_preds_binary<0.5] <-  0
    
    hmsc_preds_binary <- hmsc_preds
    hmsc_preds_binary[hmsc_preds_binary>=0.5] <-  1
    hmsc_preds_binary[hmsc_preds_binary<0.5] <-  0
    
    n_species <- length(sp_names)
    
    # Calculate metrics for ensemble
    
    ens_perf <- lapply(seq(1, n_species), function(species){
      ens_metrics <- caret::confusionMatrix(reference = factor(test_data[,species],
                                                        levels = c('1', '0')),
                                          data = factor(ens_preds_binary[,species],
                                                 levels = c('1', '0')))
      
      list(ens_metrics = round(ens_metrics$byClass[c(5:7)], 4))
      
    })
    
    names(ens_perf) <- c(sp_names)
  
    ens_metrics <- data.frame(do.call(rbind, purrr::map(ens_perf, 'ens_metrics')))
  
    ens_metrics$Method <- 'ENS'
    ens_metrics$Species <- rownames(ens_metrics)
    ens_metrics[is.na(ens_metrics)] <- 0
    
    
    # Calculate metrics for equal-weights ensemble
    
    null_ens_perf <- lapply(seq(1, n_species), function(species){
      null_ens_metrics <- caret::confusionMatrix(reference = factor(test_data[,species],
                                                          levels = c('1', '0')),
                                            data = factor(null_ens_preds_binary[,species],
                                                   levels = c('1', '0')))
      
      list(null_ens_metrics = round(null_ens_metrics$byClass[c(5:7)], 4))
      
    })
    
    names(null_ens_perf) <- c(sp_names)
    
    null_ens_metrics <- data.frame(do.call(rbind, purrr::map(null_ens_perf, 'null_ens_metrics')))
    
    null_ens_metrics$Method <- 'NULL_ENS'
    null_ens_metrics$Species <- rownames(null_ens_metrics)
    null_ens_metrics[is.na(null_ens_metrics)] <- 0
    
    
    # Calculate metrics for base model
    
    base_perf <- lapply(seq(1, n_species), function(species){
      base_metrics <- caret::confusionMatrix(reference = factor(test_data[,species],
                                                           levels = c('1', '0')),
                                             data = factor(base_preds_binary[,species],
                                                    levels = c('1', '0')))
      
      list(base_metrics = round(base_metrics$byClass[c(5:7)], 4))
      
    })
    
    names(base_perf) <- c(sp_names)
    
    base_metrics <- data.frame(do.call(rbind, purrr::map(base_perf, 'base_metrics')))
    
    base_metrics$Method <- 'BASE'
    base_metrics$Species <- rownames(base_metrics)
    base_metrics[is.na(base_metrics)] <- 0
    
    # Calculate metrics for gbm_dr model
    
    gbm_dr_perf <- lapply(seq(1, n_species), function(species){
      gbm_dr_metrics <- caret::confusionMatrix(reference = factor(test_data[,species],
                                                               levels = c('1', '0')),
                                                 data = factor(gbm_dr_preds_binary[,species],
                                                        levels = c('1', '0')))
      
      list(gbm_dr_metrics = round(gbm_dr_metrics$byClass[c(5:7)], 4))
      
    })
    
    names(gbm_dr_perf) <- c(sp_names)
    
    gbm_dr_metrics <- data.frame(do.call(rbind, purrr::map(gbm_dr_perf, 'gbm_dr_metrics')))
    
    gbm_dr_metrics$Method <- 'GBM_DR'
    gbm_dr_metrics$Species <- rownames(gbm_dr_metrics)
    gbm_dr_metrics[is.na(gbm_dr_metrics)] <- 0
    
    # Calculate metrics for gbm_pr model
    
    gbm_pr_perf <- lapply(seq(1, n_species), function(species){
      gbm_pr_metrics <- caret::confusionMatrix(reference = factor(test_data[,species],
                                                             levels = c('1', '0')),
                                               data = factor(gbm_pr_preds_binary[,species],
                                                      levels = c('1', '0')))
      
      list(gbm_pr_metrics = round(gbm_pr_metrics$byClass[c(5:7)], 4))
      
    })
    
    names(gbm_pr_perf) <- c(sp_names)
    
    gbm_pr_metrics <- data.frame(do.call(rbind, purrr::map(gbm_pr_perf, 'gbm_pr_metrics')))
    
    gbm_pr_metrics$Method <- 'GBM_PR'
    gbm_pr_metrics$Species <- rownames(gbm_pr_metrics)
    gbm_pr_metrics[is.na(gbm_pr_metrics)] <- 0
    
    # Calculate metrics for mvrf model
    
    mvrf_perf <- lapply(seq(1, n_species), function(species){
      mvrf_metrics <- caret::confusionMatrix(reference = factor(test_data[,species],
                                                             levels = c('1', '0')),
                                               data = factor(mvrf_preds_binary[,species],
                                                      levels = c('1', '0')))
      
      list(mvrf_metrics = round(mvrf_metrics$byClass[c(5:7)], 4))
      
    })
    
    names(mvrf_perf) <- c(sp_names)
    
    mvrf_metrics <- data.frame(do.call(rbind, purrr::map(mvrf_perf, 'mvrf_metrics')))
    
    mvrf_metrics$Method <- 'MVRF'
    mvrf_metrics$Species <- rownames(mvrf_metrics)
    mvrf_metrics[is.na(mvrf_metrics)] <- 0
    
    # Calculate metrics for hmsc model
    
    hmsc_perf <- lapply(seq(1, n_species), function(species){
      hmsc_metrics <- caret::confusionMatrix(reference = factor(test_data[,species],
                                                           levels = c('1', '0')),
                                             data = factor(hmsc_preds_binary[,species],
                                                    levels = c('1', '0')))
      
      list(hmsc_metrics = round(hmsc_metrics$byClass[c(5:7)], 4))
      
    })
    
    names(hmsc_perf) <- c(sp_names)
    
    hmsc_metrics <- data.frame(do.call(rbind, purrr::map(hmsc_perf, 'hmsc_metrics')))
    
    hmsc_metrics$Method <- 'HMSC'
    hmsc_metrics$Species <- rownames(hmsc_metrics)
    hmsc_metrics[is.na(hmsc_metrics)] <- 0
    
##### Adding standardized metrics 
    ens_metrics$Recall_adj <- ens_metrics$Recall - base_metrics$Recall
    ens_metrics$Precision_adj <- ens_metrics$Precision - base_metrics$Precision
    ens_metrics$F1_adj <- ens_metrics$F1 - base_metrics$F1

    null_ens_metrics$Recall_adj <- null_ens_metrics$Recall - base_metrics$Recall
    null_ens_metrics$Precision_adj <- null_ens_metrics$Precision - base_metrics$Precision
    null_ens_metrics$F1_adj <- null_ens_metrics$F1 - base_metrics$F1
    
    base_metrics$Recall_adj <- base_metrics$Recall - base_metrics$Recall
    base_metrics$Precision_adj <- base_metrics$Precision - base_metrics$Precision
    base_metrics$F1_adj <- base_metrics$F1 - base_metrics$F1
    
    gbm_dr_metrics$Recall_adj <- gbm_dr_metrics$Recall - base_metrics$Recall
    gbm_dr_metrics$Precision_adj <- gbm_dr_metrics$Precision - base_metrics$Precision
    gbm_dr_metrics$F1_adj <- gbm_dr_metrics$F1 - base_metrics$F1
    
    gbm_pr_metrics$Recall_adj <- gbm_pr_metrics$Recall - base_metrics$Recall
    gbm_pr_metrics$Precision_adj <- gbm_pr_metrics$Precision - base_metrics$Precision
    gbm_pr_metrics$F1_adj <- gbm_pr_metrics$F1 - base_metrics$F1
    
    mvrf_metrics$Recall_adj <- mvrf_metrics$Recall - base_metrics$Recall
    mvrf_metrics$Precision_adj <- mvrf_metrics$Precision - base_metrics$Precision
    mvrf_metrics$F1_adj <- mvrf_metrics$F1 - base_metrics$F1
    
    hmsc_metrics$Recall_adj <- hmsc_metrics$Recall - base_metrics$Recall
    hmsc_metrics$Precision_adj <- hmsc_metrics$Precision - base_metrics$Precision
    hmsc_metrics$F1_adj <- hmsc_metrics$F1 - base_metrics$F1
    
    all_metrics <- rbind(ens_metrics, null_ens_metrics, base_metrics, gbm_dr_metrics, gbm_pr_metrics, mvrf_metrics, hmsc_metrics)
    raw_probability_predictions <- list('GLM_BASE' = base_preds,
                                        'GBM_DR' =  gbm_dr_preds,
                                        'GBM_PR' = gbm_pr_preds, 
                                        'MVRF' = mvrf_preds,
                                        'HMSC' = hmsc_preds,
                                        'NULL_ENS' = null_ens_preds, 
                                        'ENS' = ens_preds) 
                                        
    
    return(list(rm_species = rm_species, all_metrics = all_metrics, raw_probability_predictions = raw_probability_predictions, truth = test_data[1:n_species]))
}  
    
 