feature_function_without_PCA <- function(ys, xs) {
  
  mod <- MRFcov(ys, family = 'binomial')
  
  adj.matrix <- igraph::graph.adjacency(abs(mod$graph),
                                        weighted = T,
                                        mode = "undirected")
  
  features_dat <- data.frame(species = rownames(mod$graph),
                             prevalence = apply(ys, 2, function(x) sum(x) / length(x)),
                             prevalence_rank = rank(apply(ys, 2, 
                                                          function(x) sum(x) / length(x))) / 
                               ncol(ys),
                             prevalence_sd = sd(apply(ys, 2, function(x) sum(x) / length(x))), 
                             n_obs = nrow(ys),
                             n_species = NCOL(ys),
                             degree = igraph::degree(adj.matrix, normalized = T),
                             eigen_cent = igraph::eigen_centrality(adj.matrix)$vector,
                             betweenness = igraph::betweenness(adj.matrix, nobigint = FALSE, 
                                                               normalized = TRUE), 
                             modularity = mean(igraph::modularity_matrix(adj.matrix, membership = 1:ncol(ys), weights = NULL)))
  
  jac_dist <- as.matrix(ade4::dist.binary(t(ys), 1))
  diag(jac_dist) <- NA
  features_dat$mean_jaccard <- colMeans(jac_dist, na.rm = T)
  
  dice_dist <- as.matrix(ade4::dist.binary(t(ys), 7))
  diag(dice_dist) <- NA
  features_dat$mean_dice <- colMeans(dice_dist, na.rm = T)
  
  
  jaccard_sd <- sd(features_dat$mean_jaccard)
  features_dat$jaccard_sd <- jaccard_sd
  
  dice_sd <- sd(features_dat$mean_dice)
  features_dat$dice_sd <- dice_sd
  
  if(!require(vegan)){
    install.packages('vegan')
  }
  
  dsim <- vegan::vegdist(ys, binary = TRUE)
  features_dat$mean_sorensen <- mean(dsim, na.rm = T)
  features_dat$sd_sorensen <- sd(dsim, na.rm = T)
  
  features_dat$mrf_intercept <- mod$intercepts
  View(features_dat)
  
  features_dat$sum_mrfinfo <- sum(abs(mod$graph[upper.tri(mod$graph)])) / NCOL(ys)
  features_dat$sd_mrfinfo <- sd(abs(mod$graph[upper.tri(mod$graph)]))
  
  diag(mod$graph) <- -1
  estimated_sigma <- -solve(mod$graph)
  
  features_dat$mrf_trace <- sum(diag(estimated_sigma)) / NCOL(ys)
  
  features_dat$log_determinant <- det(estimated_sigma)
  
  features_dat$no_pc <- length(xs)
  
  names <- c(1:length(xs))
  names <- as.character(names)  
  colnames(xs) <- paste("PC" ,names, sep = '')
  
  covariates <- xs
  
  output <- list(features_dat, covariates)
  names(output) <- c('features_dat', 'covariates')
  
  return(output)
} 
