
if(!require(Rcpp)){
  install.packages('Rcpp')
}

if(!require(factoextra)){
  install.packages('factoextra')
}

if(!require(devtools)){
  install.packages('devtools')
}

if(!require(dplyr)){
  install.packages('dplyr')
}

if(!require(tidyverse)){
  install.packages('tidyverse')
}

setwd("C:/.../MEE_feature_ensemble")

# Looking at the similarity between features 

load("outputs/features/all_features.rda")

cols <- c(1,3:25)
features <- all_features[, cols]


median_features <- features %>% 
  group_by(dataset) %>%  
  summarise_all(.funs = median)

median_features <- median_features %>% 
  remove_rownames %>% 
  column_to_rownames(var="dataset")

cols <- c(22)
medain_features <- median_features[cols]
train <- c("brazil_fish", "buffalo_infection", "canopy_ants", "eelgrass", "eucalyptus", "fennoscandia_birds", "finland_beetles", "fish_parasites", "germany_beetles", "helminths",
           "lion_infections", "mussel_parasites", "nz_forest", "reptiles", "shrews", "swiss_forest", "swissalps_plants", "usa_birds", "usa_trees", "victoria_plants")
train <- as.data.frame(train)
test <- c("andean_birds", "bird_parasites", "earthworms", "grassland_birds", "mulu_birds", "norway_beetles", "norway_vegetation", "swiss_birds", "uk_butterflies", "vines")
test <- as.data.frame(test)

type <- cbind(test, train)

type <- pivot_longer(data = type, 
                       cols = c(1:2), 
                       names_to = "type", 
                       values_to = "dataset")

type <- distinct(type)

type <- type[order(type$dataset), ]

type <- type %>% 
  remove_rownames %>% 
  column_to_rownames(var="dataset")



pca <- prcomp(median_features, scale = TRUE)
fviz_eig(pca)


fviz_pca_ind(pca,
             habillage = type$type,
             palette = c("#0c2a50ff", "#f68f46ff"),
             repel = TRUE     # Avoid text overlapping
)
