model_weights_function <- function(xs, ys, all_dat) {
  
  output <- vector('list', 3)
  
  for(i in 1:3){
    set.seed(seeds[i])
    in.train <- sample(1:nrow(ys), 
                       length(1:round(nrow(ys) * split_prop)), 
                       replace = F)
    train_data <- all_dat[in.train, ]
    test_data <- all_dat[-in.train, ]
    
    yhats <- lapply(colnames(ys), function(species){
      mod1 <- glm(formula(paste0(species,'~', paste0(colnames(xs), collapse = '+'))), 
                  family = 'binomial', data = train_data)
      yhat <- predict(mod1, type = 'response')
      
      # Calculate residuals and return
      list(mod1 = mod1, yhat = yhat, 
           deviance_resid = deviance_resids(yhat, train_data[, species]), 
           pearson_resid = pearson_resids(yhat, train_data[, species]))
    })
    names(yhats) <- colnames(ys)
    
    output[[i]] <- compare_all_models(yhats, train_data, test_data, 
                                                       rfsrc_formula = formula(paste0('cbind(',
                                                                                      paste0(colnames(ys), 
                                                                                             collapse = ','),') ~ .')),
                                      preds_only = FALSE)
  }
  
 output <- list(do.call(rbind, purrr::map(output, 'metrics')),
                         do.call(rbind, purrr::map(output, 'metric_ranks')),
                         do.call(rbind, purrr::map(output, 'ens_weights')))
 
  names(output) <- c('metrics', 'metric_ranks', 'ens_weights')
  
  output$metrics <- output$metrics %>%
    group_by(Species, Method) %>%
    summarise_at(vars(Prevalence, Recall, Precision, F1), list(mean))
  
  output$metric_ranks <- output$metric_ranks %>%
    group_by(Species, Method) %>%
    summarise_at(vars(Prevalence, Recall, Precision, F1), list(mean))
  
  output$ens_weights <- output$ens_weights %>%
    group_by(Species, Model) %>%
    summarise_at(vars(value), list(mean))
  
  output$metrics <-  output$metrics %>%
    dplyr::select(Species, Prevalence, Method, Recall, Precision, F1) %>% 
    dplyr::mutate(Prevalence_category = dplyr::case_when(
      Prevalence <= 0.1 ~ 'Rare (<10%)',
      Prevalence >0.1 & Prevalence <=0.3 ~ 'Uncommon (10-30%)',
      Prevalence >0.3 & Prevalence <=0.75 ~ 'Common (30-75%)',
      TRUE ~ 'Very common (>75%)'
    )) %>% 
    dplyr::select(Species, Prevalence_category, Prevalence, Method, Recall, Precision, F1) 
  
  output$metric_ranks <-  output$metric_ranks %>%
    dplyr::select(Species, Prevalence, Method, Recall, Precision, F1) %>% 
    dplyr::mutate(Prevalence_category = dplyr::case_when(
      Prevalence <= 0.1 ~ 'Rare (<10%)',
      Prevalence >0.1 & Prevalence <=0.3 ~ 'Uncommon (10-30%)',
      Prevalence >0.3 & Prevalence <=0.75 ~ 'Common (30-75%)',
      TRUE ~ 'Very common (>75%)'
    )) %>% 
    dplyr::select(Species, Prevalence_category, Prevalence, Method, Recall, Precision, F1) 

  return(output)
  }
  
 
