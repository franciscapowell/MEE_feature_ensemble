dr_metric = function(model_prob_preds, truth){
  
  if(any(model_prob_preds <= 0)){
    warning('some probability predictions <= 0; changing to 0.01 for ensembling')
    model_prob_preds[model_prob_preds <= 0] <- 0.01
  }  
  
  if(any(model_prob_preds >=1)){
    warning('some probability predictions >=1; changing to 0.99 for ensembling')
    model_prob_preds[model_prob_preds >= 1] <- 0.99
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
      (-1 * (sqrt((-2*log(1 - min(0.99, model_prob_preds))))))^2
    }))
    #### Here is the addition ####
    out <- mean(neg_resids)
    ####                                     ####
    
  } else if(length(negatives) == 0){
    pos_resids <- unlist(lapply(positives, function(h){
      (sqrt((-2 * log(max(0.01, model_prob_preds)))))^2
    }))
    out <- mean(pos_resids)
  } else {
    pos_resids <- unlist(lapply(positives, function(h){
      (sqrt((-2 * log(max(0.01, model_prob_preds)))))^2
    }))
    neg_resids <- unlist(lapply(negatives, function(h){
      (-1 * (sqrt((-2*log(1 - min(0.99, model_prob_preds))))))^2
    }))
    
    # Return mean squared deviance residual, but give higher weight to the class that is
    # rarer to give a more balanced view of performance
    out <- weighted.mean(c(mean(pos_resids), mean(neg_resids)),
                         # weights: if fewer positives exist in truth then they get more weight and vice versa
                         c(length(negatives)/length(truth),
                           length(positives)/length(truth)))
  }
  
  # Return the mean residuals
  out
  
}


