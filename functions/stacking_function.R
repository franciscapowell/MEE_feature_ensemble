#### A function for calculating deviance residuals ####
deviance_resids = function(yhat, truth){
  # Check for appropriate data structures
  if(any(class(truth) %in% 'tbl')){
    truth <- dplyr::pull(truth, 1)
  }
  
  if(any(class(yhat) %in% 'tbl')){
    yhat <- dplyr::pull(yhat, 1)
  }
  
  if(any(is.na(yhat))){
    stop('NAs detected in yhat')
  }
  
  if(any(is.na(truth))){
    stop('NAs detected in truth')
  }
  
  is.binary <- function(v) {
    x <- unique(v)
    if(length(x) == 1){
    
      if(x == 0){
      
        output <- TRUE
   
         } else if(x == 1){
      
           output <- TRUE
    
         } else {
             output <- FALSE
           }
      } else {
      output <- length(x) - sum(is.na(x)) == 2L && all(sort(x[1:2]) == 0:1)
    }
    output
  }
  
  if(!is.binary(truth)){
    stop('truth must be a binary vector (1s and 0s only)')
  }
  
  # To calculate deviance residuals consistently for binomial models, we need a simple formula
  # truth equals binary observations; yhat equals model probability predictions
  resid <- ifelse(truth == 1, 
                  sqrt((-2 * log(yhat))), 
                  -1 * (sqrt((-2*log(1 - yhat)))))
  resid <- as.vector(resid)
  return(resid)
}

#### A function for calculating pearson residuals ####
pearson_resids = function(yhat, truth){
  # Check for appropriate data structures
  if(any(class(truth) %in% 'tbl')){
    truth <- dplyr::pull(truth, 1)
  }
  
  if(any(class(yhat) %in% 'tbl')){
    yhat <- dplyr::pull(yhat, 1)
  }
  
  if(any(is.na(yhat))){
    stop('NAs detected in yhat')
  }
  
  if(any(is.na(truth))){
    stop('NAs detected in truth')
  }
  
  is.binary <- function(v) {
    x <- unique(v)
    if(length(x) == 1){
      
      if(x == 0){
        
        output <- TRUE
        
      } else if(x == 1){
        
        output <- TRUE
        
      } else {
        output <- FALSE
      }
    } else {
      output <- length(x) - sum(is.na(x)) == 2L && all(sort(x[1:2]) == 0:1)
    }
    output
  }
  
  if(!is.binary(truth)){
    stop('truth must be a binary vector (1s and 0s only)')
  }
  
  # To calculate pearson residuals consistently for binomial models, we need a simple formula
  # truth equals binary observations; yhat equals model probability predictions
  resid <- (truth - yhat) / sqrt(yhat * (1 - yhat))
  resid <- as.vector(resid)
  return(resid)
}


#### A function for generating stacked predictions using the Xing et al methodology ####
stack_predictions = function(yhats, test_data, covariance_mod = 'gbm',
                             resid = 'deviance',
                             cores = max(parallel::detectCores() - 1, 12)){
  
  # Determine the total number of outcome variables in the yhat data
  n_variables <- length(yhats)
  
  # Check that the covariance_mod specification is allowed
  if(!(covariance_mod %in% c('gbm', 'gam', 'lm')))
    stop('Please select one of the three covariance model options:
         "gbm", "gam", "lm"')
  
  # For each outcome, use model 1 residuals (resid from the yhats list) as outcomes and 
  # predictions from each other outcome's mod 1 (yhat) as predictors to learn the covariance matrix
  cat('Fitting', covariance_mod, 'models to learn the multivariate covariance matrix...\n')
  if(cores > 1){
    cl <- makePSOCKcluster(cores)
    setDefaultCluster(cl)
    clusterExport(NULL, c('yhats', 'covariance_mod', 'n_variables', 'resid'),
                  envir = environment())
    clusterEvalQ(cl, library(gbm))
    clusterEvalQ(cl, library(mgcv))

  rhats <- parLapply(cl, seq(1, n_variables), function(species){
    if(resid == 'deviance'){
      outcome <- yhats[[species]]$deviance_resid
    } else{
      outcome <- yhats[[species]]$pearson_resid
    }
   
    predictors <- do.call(cbind, purrr::map(yhats, 'yhat'))[, -species]
    colnames(predictors) <- paste0('pred_', seq(1, ncol(predictors)))
    
    if(covariance_mod == 'gbm'){
      interaction.depth <- 1
      if(NCOL(predictors) > 2 && NCOL(predictors) < 4){
        interaction.depth <- 2
      } 
      
      # Use a boosted regression tree to learn complex, nonlinear covariance relationships
      # This needs to be a weak learner for generalisation, but strong enough to detect some
      # patterns
      mod2 <- gbm::gbm(Y ~., data = cbind(data.frame(Y = outcome),
                                          as.matrix(predictors)),
                       distribution = 'gaussian',
                       n.trees = 50, shrinkage = 0.1, 
                       interaction.depth = interaction.depth,
                       n.minobsinnode = 5,
                       keep.data = FALSE, verbose = FALSE,
                       n.cores = 1)
    }
    
    if(covariance_mod == 'gam'){
      # Use a GAM to learn the covariance matrix
      # Include each other outcome as a smooth predictor and use penalisation to
      # force coefficients to zero if support is limited. No interactions here
      max_knots <- apply(predictors, 2, function(x) length(unique(x)))
      
      # Need to set terms with few max_knots to be linear terms
      predictor_formula <- matrix(NA, nrow = 1, ncol = ncol(predictors))
      for(i in 1:ncol(predictors)){
        predictor_formula[1, i] <- ifelse(max_knots[i] < 6, 
                                          colnames(predictors)[i],
                                          paste0("s(", colnames(predictors)[i],", 
                                                 bs = 'cs', k = 4",
                                                 ")"))
      }
      
      predictor_terms <- paste0(predictor_formula[1,],
                                collapse = '+')
      gam_formula <- as.formula(paste0("Y~", 
                                       predictor_terms))
      mod2 <- mgcv::gam(gam_formula, data = cbind(data.frame(Y = outcome),
                                                  as.matrix(predictors)))
    }
    
    if(covariance_mod == 'lm'){
      # Use a simple linear model to learn the covariance matrix
      lm_formula <- as.formula(paste0("Y~", 
                                      paste0(colnames(predictors), collapse = "+")))
      mod2 <- lm(lm_formula, data = cbind(data.frame(Y = outcome),
                                          as.matrix(predictors)))
    }
    
    # Return the models
    gc()
    list(mod2 = mod2, pred_names = colnames(predictors))
    })
  stopCluster(cl)
  
  } else {
    rhats <- lapply(seq(1, n_variables), function(species){
      print(species)
      if(resid == 'deviance'){
        outcome <- yhats[[species]]$deviance_resid
      } else{
        outcome <- yhats[[species]]$pearson_resid
      }
      predictors <- do.call(cbind, purrr::map(yhats, 'yhat'))[, -species]
      colnames(predictors) <- paste0('pred_', seq(1, ncol(predictors)))
      
      if(covariance_mod == 'gbm'){
        interaction.depth <- 1
        if(NCOL(predictors) > 2 && NCOL(predictors) < 4){
          interaction.depth <- 2
        } 
        
        # Use a boosted regression tree to learn complex, nonlinear covariance relationships
        # This needs to be a weak learner for generalisation, but strong enough to detect some
        # patterns
        mod2 <- gbm::gbm(Y ~., data = cbind(data.frame(Y = outcome),
                                            as.matrix(predictors)),
                         distribution = 'gaussian',
                         n.trees = 50, shrinkage = 0.1, 
                         interaction.depth = interaction.depth,
                         n.minobsinnode = 5,
                         keep.data = FALSE, verbose = FALSE)
      }
      
      if(covariance_mod == 'gam'){
        # Use a GAM to learn the covariance matrix
        # Include each other outcome as a smooth predictor and use penalisation to
        # force coefficients to zero if support is limited. No interactions here
        max_knots <- apply(predictors, 2, function(x) length(unique(x)))
        
        # Need to set terms with few max_knots to be linear terms
        predictor_formula <- matrix(NA, nrow = 1, ncol = ncol(predictors))
        for(i in 1:ncol(predictors)){
          predictor_formula[1, i] <- ifelse(max_knots[i] < 6, 
                                            colnames(predictors)[i],
                                            paste0("s(", colnames(predictors)[i],", 
                                                 bs = 'cs', k = 4",
                                                   ")"))
        }
        
        predictor_terms <- paste0(predictor_formula[1,],
                                  collapse = '+')
        gam_formula <- as.formula(paste0("Y~", 
                                         predictor_terms))
        mod2 <- mgcv::gam(gam_formula, data = cbind(data.frame(Y = outcome),
                                                    as.matrix(predictors)))
      }
      
      if(covariance_mod == 'lm'){
        # Use a simple linear model to learn the covariance matrix
        lm_formula <- as.formula(paste0("Y~", 
                                        paste0(colnames(predictors), collapse = "+")))
        mod2 <- lm(lm_formula, data = cbind(data.frame(Y = outcome),
                                            as.matrix(predictors)))
      }
      
      # Return the models
      gc()
      list(mod2 = mod2, pred_names = colnames(predictors))
    })
  }
  
  
  # Predict mod 1 for each species using the out-of-sample test data
  mod1_test_preds <- do.call(cbind, lapply(seq(1, n_variables), function(y){
    # may need to find class of mod1 in order to specify the predict function: class(yhats[[1]]$mod1)
    predict(yhats[[y]]$mod1, newdata = test_data, type = 'response') 
  }))
  
  # Now to generate the stacked predictions
  cat('Adjusting original predictions using stacked information...\n')

    # Predict mod 2 'adjustments' using the test mod 1's predictions
   combiner_preds <- lapply(seq(1, n_variables), function(y){
      test_preds <- data.frame(mod1_test_preds[,-y])
      colnames(test_preds) <- rhats[[y]]$pred_names
      
      if(covariance_mod == 'gbm'){
        invisible(capture.output(adj_preds <- suppressMessages(gbm::predict.gbm(rhats[[y]]$mod2, 
                                                                                newdata = test_preds, 
                                                                                n.trees = 50))))
      }
      if(covariance_mod == 'gam'){
        adj_preds <- mgcv::predict.gam(rhats[[y]]$mod2, newdata = test_preds)
      }
      if(covariance_mod == 'lm'){
        adj_preds <- predict(rhats[[y]]$mod2, newdata = test_preds)
      }
      gc()
      
      #resid <- yhats[[y]]$deviance_resid
      if(resid == 'deviance'){
        # Adjust predictions using the Xing et al 'proportion of the inverted distance' metric
        # and convert them to the outcome probability scale from the paper:
        # Simultaneous prediction of multiple outcomes using revised stacking algorithms
        d1 <- abs(1 / (adj_preds - sqrt((-2 * log(mod1_test_preds[,y])))))
        d0 <- abs(1 / (adj_preds + sqrt((-2 * log(mod1_test_preds[,y])))))
        final_preds <- d1 / (d1 + d0)
      } else {
        final_preds <- (adj_preds * sqrt(mod1_test_preds[,y] * (1 - mod1_test_preds[,y]))) +
          mod1_test_preds[,y]
      }
      
      # Return both the probability predictions and the binary predictions
      final_preds_binary <- ifelse(final_preds >= 0.5, 1, 0)
      rm(adj_preds)
      gc()
      list(probability_preds = final_preds,
           binary_preds = final_preds_binary)
    })
   rm(rhats, yhats)
   gc()
  
  # Use purr::map to pull out the prediction elements and put them into a more useful
  # final list
  list(probability_preds = do.call(cbind, purrr::map(combiner_preds, 'probability_preds')),
       binary_preds = do.call(cbind, purrr::map(combiner_preds, 'binary_preds')))

  
}


#### Function to generate binary predictions from univariate base models ####
yhat_preds = function(yhats, test_data, return_binary = T){
  n_species <- length(yhats)
  yhat_test_preds <- lapply(seq(1, n_species), function(species){
    single_preds <- predict(yhats[[species]]$mod1, newdata = test_data, type = 'response')
    if(return_binary){
    single_preds <- ifelse(single_preds > 0.4999, 1, 0)
    }
    single_preds
  })
  names(yhat_test_preds) <- names(yhats)

  return(yhat_test_preds)
}

#### Function to calculate prediction accuracies for the stacked models and compare to the 
# simpler models and multivariate random forests for each species ####
compare_accuracies = function(test_data, yhats, predictions){
  
  n_species <- length(yhats)

  comparison_stats <- lapply(seq(1, n_species), function(species){
  stacked_stats <- caret::confusionMatrix(factor(predictions[, species],
                                                     levels = c('1', '0')), 
                                              factor(as.matrix(test_data)[, species],
                                                     levels = c('1', '0')))
  
  # Return quantities of interest:
  # Precision = the proportion of the data points predicted as positive 
  # that actually are positive
  # Recall = the ability of a model to find all the positive cases within a dataset
  # F1 score = harmonic mean of precision and recall (probably the most important metric here)
  list(prevalence = stacked_stats$byClass[c(8)],
       stacked_stats = round(stacked_stats$byClass[c(5:7, 11)], 4))
})
names(comparison_stats) <- names(yhats)
 return(comparison_stats)
}

#### General function to calculate metrics for all models ####
compare_all_models = function(yhats, train_data, test_data, rfsrc_formula,
                              preds_only = FALSE){
  
  # Generate univariate predictions from the base models in yhats
  yhat_preds_binary <- do.call(cbind, yhat_preds(yhats = yhats, test_data = test_data))
  
  # Generate univariate predictions from the base models in yhats
  yhat_prob_preds <- do.call(cbind, yhat_preds(yhats = yhats, test_data = test_data, return_binary = FALSE))
  
  # Use the gbm stacking option for learning the covariance matrix
  gbm_stack_pearson <- stack_predictions(yhats, test_data = test_data, covariance_mod = 'gbm', resid = 'pearson')
  gbm_stack_deviance <- stack_predictions(yhats, test_data = test_data, covariance_mod = 'gbm', resid = 'deviance')
  
  GBMSP_preds <- gbm_stack_pearson$probability_preds
  GBMSD_preds <- gbm_stack_deviance$probability_preds
  
  # Fit a multivariate random forest model for comparison
  cat('Fitting the multivariate random forest...\n')
  rf <- rfsrc(rfsrc_formula,
              data = train_data, nodesize = 8)
  rf_preds <- predict(rf, newdata = test_data)
  rf_preds <- do.call(cbind, purrr::map(rf_preds$regrOutput, 'predicted'))
  rf_preds_binary <- ifelse(rf_preds >= 0.5, 1, 0) 
 
  MVRF_preds <- rf_preds 
  
  # Compare accuracy metrics
  deviance_gbm_metrics <- data.frame(do.call(rbind, 
                         purrr::map(compare_accuracies(test_data, yhats, gbm_stack_deviance$binary_preds),
                                    'stacked_stats')))
  deviance_gbm_metrics$Method <- 'GBM_STACK_DR'
  deviance_gbm_metrics$Species <- rownames(deviance_gbm_metrics)
  deviance_gbm_metrics[is.na(deviance_gbm_metrics)] <- 0
  
  pearson_gbm_metrics <- data.frame(do.call(rbind, 
                                    purrr::map(compare_accuracies(test_data, yhats, gbm_stack_pearson$binary_preds),
                                               'stacked_stats')))
  pearson_gbm_metrics$Method <- 'GBM_STACK_PR'
  pearson_gbm_metrics$Species <- rownames(pearson_gbm_metrics)
  pearson_gbm_metrics[is.na(pearson_gbm_metrics)] <- 0
  
  mvrf_metrics <- data.frame(do.call(rbind, 
                          purrr::map(compare_accuracies(test_data, yhats, rf_preds_binary),
                                     'stacked_stats')))
  mvrf_metrics$Method <- 'MVRF'
  mvrf_metrics$Species <- rownames(mvrf_metrics)
  mvrf_metrics[is.na(mvrf_metrics)] <- 0
  
  uni_metrics <- data.frame(do.call(rbind, 
                         purrr::map(compare_accuracies(test_data, yhats, yhat_preds_binary),
                                    'stacked_stats')))
  uni_metrics$Method <- 'BASE'
  uni_metrics$Species <- rownames(uni_metrics)
  uni_metrics[is.na(uni_metrics)] <- 0
  
  deviance_gbm_metrics$Recall <- deviance_gbm_metrics$Recall - uni_metrics$Recall
  pearson_gbm_metrics$Recall <- pearson_gbm_metrics$Recall - uni_metrics$Recall
  mvrf_metrics$Recall <- mvrf_metrics$Recall - uni_metrics$Recall
  
  deviance_gbm_metrics$Precision <- deviance_gbm_metrics$Precision - uni_metrics$Precision
  pearson_gbm_metrics$Precision <- pearson_gbm_metrics$Precision - uni_metrics$Precision
  mvrf_metrics$Precision <- mvrf_metrics$Precision - uni_metrics$Precision
  
  deviance_gbm_metrics$F1 <- deviance_gbm_metrics$F1 - uni_metrics$F1
  pearson_gbm_metrics$F1 <- pearson_gbm_metrics$F1 - uni_metrics$F1
  mvrf_metrics$F1 <- mvrf_metrics$F1 - uni_metrics$F1
  
  gc()
  # Fit a multivariate Hmsc model for comparison
  cat('Fitting the Hmsc model...\n')
  testy_hmsc <- data.frame(matrix(NA, ncol = ncol(yhat_prob_preds),
                                  nrow = nrow(test_data)))
  colnames(testy_hmsc) <- colnames(train_data)[1:ncol(yhat_prob_preds)]
  hmsc_ys <- dplyr::bind_rows(train_data[,1:ncol(yhat_prob_preds)],
                              testy_hmsc)
  testx_hmsc <- data.frame(test_data[,-c(1:ncol(yhat_prob_preds))])
  colnames(testx_hmsc) <- colnames(train_data)[-c(1:ncol(yhat_prob_preds))]
  hmsc_xs <- rbind(data.frame(train_data[,-c(1:ncol(yhat_prob_preds)), drop = FALSE]),
                   testx_hmsc)
  studyDesign <- data.frame(sample = as.factor(1:nrow(hmsc_ys)))
  rl = Hmsc::HmscRandomLevel(units = studyDesign$sample)
  m <- Hmsc::Hmsc(Y = as.matrix(hmsc_ys), 
                  X = as.matrix(hmsc_xs),
                  distr = 'probit', studyDesign = studyDesign, ranLevels = list(sample = rl))
  m <- Hmsc::sampleMcmc(m, thin = 2, samples = 1000, 
                        transient = 2000,
                        nChains = 2, verbose = 0, nParallel = 2)
  mpost <- Hmsc::convertToCodaObject(m)
  
  es.beta = effectiveSize(mpost$Beta)
  gelrub = gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
  
  # make sure this returns a vector of convergence diagnostics 
  
  if(any(gelrub) > 1.1){
    m <- Hmsc::sampleMcmc(m, thin = 2, samples = 10000, 
                          transient = 2000,
                          nChains = 2, verbose = 0, nParallel = 2)
    mpost <- Hmsc::convertToCodaObject(m)
    
    es.beta = effectiveSize(mpost$Beta)
    gelrub = gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
    
    if(any(gelrub) > 1.1) print('Warning: HMSC MCMC chain did not converge')
  }
  
  hmsc_preds <- Hmsc::computePredictedValues(m)
  hmsc_preds <- rowMeans(hmsc_preds, dims = 2)
  hmsc_preds <- tail(hmsc_preds, nrow(testx_hmsc))
  hmsc_preds_binary <- ifelse(hmsc_preds >= 0.5, 1, 0)
  
  hmsc_metrics <- data.frame(do.call(rbind, 
                                     purrr::map(compare_accuracies(test_data, yhats, hmsc_preds_binary),
                                                'stacked_stats')))
  hmsc_metrics$Method <- 'HMSC'
  hmsc_metrics$Species <- rownames(hmsc_metrics)
  hmsc_metrics[is.na(hmsc_metrics)] <- 0
  
  hmsc_metrics$Recall <- hmsc_metrics$Recall - uni_metrics$Recall
  hmsc_metrics$Precision <- hmsc_metrics$Precision - uni_metrics$Precision
  hmsc_metrics$F1 <- hmsc_metrics$F1 - uni_metrics$F1
  
  HMSC_preds <- hmsc_preds
  
  metrics <- rbind(deviance_gbm_metrics, pearson_gbm_metrics, mvrf_metrics, hmsc_metrics)
  rownames(metrics) <- NULL
  

    
  # Calculate method metric distributions based on species prevalence
  metrics <-  metrics %>%
      dplyr::select(Species, Method, Recall, Precision, F1) %>% 
      dplyr::distinct() %>%
      dplyr::left_join(get_prevalences(yhats, test_data), by = 'Species') %>%
      dplyr::select(Species, Prevalence, Method, Recall, Precision, F1) %>%
      dplyr::distinct() -> ranks

  # Create lists for storing predictions and truths for each species in the data
  predictions <- vector(mode = 'list')
  truth <- vector(mode = 'list')
  for(i in 1:NCOL(yhat_prob_preds)){
    predictions[[i]] <- cbind(yhat_prob_preds[,i],
                              GBMSD_preds[,i],
                              GBMSP_preds[,i],
                              MVRF_preds[,i],
                              HMSC_preds[,i])
    truth[[i]] <- data.frame(test_data[,i])[,1]
  }

  gc()
 optim_func = function(predictions, truth, weights){
   
   # Set any zero or negative predictions to a small positive offset so that weighted mean preds
   # cannot be absolute zero (causes problems when logging)
   if(any(predictions <= 0)){
     warning('some probability predictions <= 0; changing to 0.01 for ensembling')
     predictions[predictions <= 0] <- 0.01
   }
   
   # Likewise, any predictions at a perfect 1 (or greater) will cause problems
   if(any(predictions >=1)){
     warning('some probability predictions >=1; changing to 0.99 for ensembling')
     predictions[predictions >= 1] <- 0.99
   }
   
   # Unlikely to have any NA or infinite predictions, but if so then set to 0.5
   if(any(is.na(predictions))){
     warning('some probability predictions are NA; changing to 0.5 for ensembling')
     predictions[is.na(predictions)] <- 0.5
   }

   if(any(is.infinite(predictions))){
     warning('some probability predictions are Inf; changing to 0.5 for ensembling')
     predictions[is.infinite(predictions)] <- 0.5
   }

   # Separate positive and negative truths
   positives <- which(truth == 1)
   if(length(positives) == 0){
     negatives <- seq_len(length(truth))
   } else {
     negatives <- seq_len(length(truth))[-positives]
   }
   
   # Calculate squared deviance residuals (include soft constraint to ensure no Infinite values in the log)
   if(length(positives) == 0){
     neg_resids <- unlist(lapply(negatives, function(h){
       (-1 * (sqrt((-2*log(1 - min(0.99, weighted.mean(predictions[h,], weights)))))))^2
     }))
     out <- mean(neg_resids)
   } else if(length(negatives) == 0){
     pos_resids <- unlist(lapply(positives, function(h){
       (sqrt((-2 * log(max(0.01, weighted.mean(predictions[h,], weights))))))^2
     }))
     out <- mean(pos_resids)
   } else {
     pos_resids <- unlist(lapply(positives, function(h){
       (sqrt((-2 * log(max(0.01, weighted.mean(predictions[h,], weights))))))^2
     }))
     neg_resids <- unlist(lapply(negatives, function(h){
       (-1 * (sqrt((-2*log(1 - min(0.99, weighted.mean(predictions[h,], weights)))))))^2
     }))
     
   # Return mean squared deviance residual, but give higher weight to the class that is
   # rarer to give a more balanced view of performance
   out <- weighted.mean(c(mean(pos_resids), mean(neg_resids)),
                 # weights: if fewer positives exist in truth then they get more weight and vice versa
                 c(length(negatives)/length(truth),
                   length(positives)/length(truth)))
   }
   return(out)
 }
 
 # Get mean weights across 5 optimisations with different starting values, as different values can 
 # sometimes lead to slightly different solutions
 # Set starting weights and optimise
 all_weights <- do.call(cbind, lapply(seq_len(NCOL(yhat_prob_preds)), function(species){
   cat('species is', species, '\n\n')
  ens_weights <- do.call(rbind, lapply(seq_len(5), function(x){
     weights <- rbeta(ncol(predictions[[species]]), 1, 1)
     opt <- optim(weights,
                  optim_func,
                  truth = as.vector(truth[[species]]),
                  predictions = predictions[[species]],
                  # Weights at zero can cause problems in gradient optimisation,
                  # so set minimum to 0.01
                  method = 'L-BFGS-B', lower = 0.01, upper = 1)
     
     # Set the 0.01s back to zero and return final weights
     opt$par[opt$par == 0.01] <- 0
     opt$par
     
   }))
colMeans(ens_weights)
 }))
colnames(all_weights) <- colnames(test_data[,1:length(yhats)])
all_weights <- data.frame(all_weights)
all_weights$Model <- c('BASE', 'GBM_STACK_DR', 'GBM_STACK_PR', 'MVRF', 'HMSC')[1:NCOL(predictions[[1]])] 
all_weights %>%
  tidyr::pivot_longer(cols = -Model, names_to = 'Species') -> all_weights

if(preds_only){
  output <- list(BASE = yhat_prob_preds, GBM_DR = GBMSD_preds, GBM_PR = GBMSP_preds, MVRF = MVRF_preds, HMSC = HMSC_preds)
} else{
  output <- list(metrics = metrics, metric_ranks = ranks, ens_weights = all_weights)
}
  return(output)
  
}

#### Function to return prevalences in the test data ####
get_prevalences = function(yhats, test_data){
  yhat_test_preds <- do.call(cbind, yhat_preds(yhats = yhats, test_data = test_data))
  prevs <- data.frame(do.call(rbind, 
                                    purrr::map(compare_accuracies(test_data, yhats, yhat_test_preds),
                                               'prevalence')))
  prevs <- data.frame(prevs)
  prevs$Species <- rownames(prevs)
  rownames(prevs) <- NULL
  return(prevs)
}

#### Functions to plot metrics for all species x method combinations ####
plot_species_recall = function(metrics){
  metrics$metrics %>%
    dplyr::mutate(Method = factor(Method, 
                                  levels = c('BASE', 'MVRF','HMSC', 'LM_STACK_DR', 'GAM_STACK_DR',
                                             'GBM_STACK_DR', 'LM_STACK_PR', 'GAM_STACK_PR', 'GBM_STACK_PR'))) -> metrics
  ggplot(metrics, aes(x = Recall, y = Method)) + 
    geom_boxplot(aes(fill = Method)) + 
    geom_vline(xintercept = 0, linetype = 'dashed') +
    facet_wrap(~Species) + theme_bw() + 
    xlab('Improvement in recall compared to BASE-GLM') + theme(legend.position = 'none')
}

plot_species_precision = function(metrics){
  metrics$metrics %>%
    dplyr::mutate(Method = factor(Method, 
                                  levels = c('BASE', 'MVRF','HMSC','LM_STACK_DR', 'GAM_STACK_DR',
                                             'GBM_STACK_DR', 'LM_STACK_PR', 'GAM_STACK_PR', 'GBM_STACK_PR'))) -> metrics
  ggplot(metrics, aes(x = Precision, y = Method)) + 
    geom_boxplot(aes(fill = Method)) + 
    geom_vline(xintercept = 0, linetype = 'dashed') +
    facet_wrap(~Species) + theme_bw() + 
    xlab('Improvement in precision compared to BASE-GLM') + theme(legend.position = 'none')
}

plot_species_F1 = function(metrics){
  metrics$metrics %>%
    dplyr::mutate(Method = factor(Method, 
                                  levels = c('BASE', 'MVRF','HMSC','LM_STACK_DR', 'GAM_STACK_DR',
                                             'GBM_STACK_DR', 'LM_STACK_PR', 'GAM_STACK_PR', 'GBM_STACK_PR'))) -> metrics
  ggplot(metrics, aes(x = F1, y = Method)) + 
    geom_boxplot(aes(fill = Method)) + 
    geom_vline(xintercept = 0, linetype = 'dashed') +
    facet_wrap(~Species) + theme_bw() + 
    xlab('Improvement in F1 compared to BASE-GLM') + theme(legend.position = 'none')
}

#### Function to plot aggregate metric changes ####
plot_recall_changes = function(metrics, type = 'boxplot'){

  if(!is.data.frame(metrics)){
    ranks <- metrics$metric_ranks
  } else {
    ranks <- metrics
  }
  
  if(type == 'boxplot'){
    ggplot(ranks, aes(x = Recall, y = Method)) + 
      geom_boxplot(aes(fill = Method)) + 
      geom_vline(xintercept = 0, linetype = 'dashed') +
      facet_wrap(~ Prevalence_category) +
      theme_bw() + theme(legend.position = 'none') +
      xlab('Improvement in recall over GLM-BASE') + 
      ylab('Model')
  } else {
    ggplot(ranks, aes(x = Recall, y = Method)) + 
      geom_violin(aes(fill = Method)) + 
      geom_vline(xintercept = 0, linetype = 'dashed') +
      facet_wrap(~ Prevalence_category) +
      theme_bw() + theme(legend.position = 'none') +
      xlab('Improvement in recall over GLM-BASE') + 
      ylab('Model')
  }
  
}

plot_precision_changes = function(metrics, type = 'boxplot'){
  
  if(!is.data.frame(metrics)){
    ranks <- metrics$metric_ranks
  } else {
    ranks <- metrics
  }
  
  if(type == 'boxplot'){
    ggplot(ranks, aes(x = Precision, y = Method)) + 
      geom_boxplot(aes(fill = Method)) + 
      geom_vline(xintercept = 0, linetype = 'dashed') +
      facet_wrap(~ Prevalence_category) +
      theme_bw() + theme(legend.position = 'none') +
      xlab('Improvement in precision over GLM-BASE')+ 
      ylab('Model')
  } else {
    ggplot(ranks, aes(x = Precision, y = Method)) + 
      geom_violin(aes(fill = Method)) + 
      geom_vline(xintercept = 0, linetype = 'dashed') +
      facet_wrap(~ Prevalence_category) +
      theme_bw() + theme(legend.position = 'none') +
      xlab('Improvement in precision over GLM-BASE')+ 
      ylab('Model')
  }
  
}

plot_F1_changes = function(metrics, type = 'boxplot'){
  
  if(!is.data.frame(metrics)){
    ranks <- metrics$metric_ranks
  } else {
    ranks <- metrics
  }
  
  if(type == 'boxplot'){
    ggplot(ranks, aes(x = F1, y = Method)) + 
      geom_boxplot(aes(fill = Method)) + 
      geom_vline(xintercept = 0, linetype = 'dashed') +
      facet_wrap(~ Prevalence_category) +
      theme_bw() + theme(legend.position = 'none') +
      xlab('Improvement in F1 over GLM-BASE')+ 
      ylab('Model')
  } else {
    ggplot(ranks, aes(x = F1, y = Method)) + 
      geom_violin(aes(fill = Method)) + 
      geom_vline(xintercept = 0, linetype = 'dashed') +
      facet_wrap(~ Prevalence_category) +
      theme_bw() + theme(legend.position = 'none') +
      xlab('Improvement in F1 over GLM-BASE')+ 
      ylab('Model')
  }
  
}


