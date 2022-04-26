
############################################# Data Preparation #############################################

Note: This section explains how the 30 datasets used in this analysis were prepared from the raw data, prior to conducting analysis. Follow these steps to obtain response variables (binary co-occurrence data for each community) and the predictor variables (prior to running PCA analysis for standaradising across all communities, but in a format that is readable by the PCA function). 

***** STEP A: Preparing datasets for analysis from raw *****

#1. Step A: Open the data preparation script for preparing datasets (Step A): MEE_feature_ensemble/r_scripts/data_preparation_code/A.preparation_of_datasets.R   

#2. Load packages required: 'caret'; 'dplyr'; 'gbm'; 'mgcv'; 'purrr'; 'randomForestSRC'; 'ggplot2'; 'Hmsc'; 'parallel'; 'MRFcov'

#3. Run entire script. The output from this script will be 2 datasets for each community - one dataset containing the response variables (i.e. binary species occurrences where 1 is presence and 0 is absence) which will be stored as datasets that are ready to be used in the analysis (MEE_feature_ensemble/data/datasets_for_analysis); and one containing the predictors to be included in PCA for standardising covariates (i.e. raw predictors all converted to numeric variables without ID columns, columns with unique values for each row or columns that are identical for every row). 
> For some communities, the number of species may be fewer than in the original dataset. The reason for exclusion for each community can be found in the dataset information file (MEE_feature_ensemble/data/dataset_information.xlsx). 
  

############################################# Guide for Workflow #############################################



#### STEP 1: Calculate features for all datasets #### 

#1. Open the workflow script for Step 1: MEE_feature_ensemble/r_scripts/workflow_code/1.calculating_features.R

#2. Load packages required: 'caret'; 'dplyr'; 'gbm'; 'mgcv'; 'purrr'; 'randomForestSRC'; 'ggplot2'; 'Hmsc'; 'parallel'; 'MRFcov'

#3.  Load the Functions required from 'functions' folder: 'feature_function.R'; 'feature_function_without_PCA.R' (MEE_feature_ensemble/functions/...) 
> these functions calculate the features for each dataset and stores them as a dataset within the 'features' folder (MEE_feature_ensemble/metrics/features).The 'feature_function.R' function runs PCA analysis within the function to prepare covariates for analysis in a standardised form (i.e. extracts covariates), and saves features from this analysis as features as well (i.e. x variable features used to predict model weights). The 'feature_function_without_PCA.R' is a function that also calculates the features and extracts information for the number of PCs used as covariates, but does NOT perform the PCA analysis. This function is used for the five datasets from Norberg et al. 2019 (fennoscandia_birds, uk_butterflies, victoria_plants, usa_trees and norway_vegetation) as the covariates used are already as PCs. The information regarding the variability explained by the PCs and the original number of predictors for these datasets is added after the function has been run in the "1.calculating_features.R' workflow script to use them as features for predicting model weights. 

#4. Run entire code. Output will be the features that will be used to train and test the ensemble. 

Note:  While this script is labelled as "calculating_features", it also contains the functions and code that were used to obtain the PCA covariates (xs) from the raw covariates (saved from the STEP A. in the 'MEE_feature_ensemble/data/data_prep'folder). These new PCA covariates can be found in the'MEE_feature_ensemble/data/datasets_for_analysis' folder.  

Note: This step is performed prior to obtaining the model weights from the training data as some of the features as our analysis used three of these features (median prevalence, number of species, number of observations) for stratification to select datasets for training (20) and for testing (10). This was done to ensure that the datasets within both groups were diverse in terms of these characteristics. 
  

***** STEP B: Stratifying variables for dataset selection *****

#1. Open the  data preparation script for selecting datasets for training/testing (Step B): MEE_feature_ensemble/r_scripts/data_preparation_code/B.stratifying_variables_for_dataset_selection.R 

#2. Load packages required: 'caret'; 'dplyr'; 'gbm'; 'mgcv'; 'purrr'; 'randomForestSRC'; 'ggplot2'; 'Hmsc'; 'parallel'; 'MRFcov'

#3. Load the features for all datasets calculated in STEP 1 (These can be found in the 'features' folder: 'MEE_feature_ensemble/outputs/features'). When openning these datasets, create two new columns: one for the dataset name, and one for the median prevalence of the community. 

#4. Merge all features together. 

#5. From the all_features dataset, select the features that you wish to use for stratification. In this analysis, we selected number of species (n_species), number of observations (n_obs), number of PCs (no_pc) and median prevalence (med_prev). Each of these variables were then cut into three groups based on data range. As these are characteristics at the community level, select rows based on unique dataset (i.e. should obtain a dataframe with 30 rows, 1 for each dataset == 'data').  

#6. MANUAL PROCESSING. To ensure that diversity among the training and testing datasets, the next step was conducted manually. Refer to the Excel file 'selection_of_testing_datasets': 'MEE_feature_ensemble/outputs/manual_processing/selection_of_testing_datasets.xlsx'.
> #6.1. The dataframe 'data' was copied into excel
> #6.2. The four stratified categorical variables(n_species_cat, n_obs_cat, no_pc_cat, med_prev_cat) were colour coded ( 1 = yellow, 2 = green, 3 = blue)
> #6.3. The datasets were grouped based on the combination of these four variables
> #6.4. One row of each combination was selected and moved to the testing group, including any unique combination datasets. This resulted in a total of 12 datasets for testing (only 10 datasets required to ensure sufficient data for training) 
> #6.5. To select which two datasets should be removed from testing group, two combinations were tested. Given the limited diversity in the stratification categories for the number of observations and the median prevalence, these combinations included: one which removed the two datasets with values other than 1 in the med_prev column (swiss_birds & uk_butterflies), effectively removing median prevalence as a category for selecting datasets; and one which removed the two datasets with values other than 1 in the n_obs_cat column (fish_parasites & helminths), effectively removing the number of observations as a category for selecting datasets.

#7. The training and testing data when removing number of observations category (training_rm_n_obs; testing_rm_n_obs) and when removing removing median prevalence category (training_rm_med_prev; testing_rm_med_prev) were compared for each group using summary statistics. The min, 1st quartile, median, 3rd quartile and max values for prevalence were used to decide which training and testing combination were more similar. All of these metrics were more similar between testing and training when removing the fish_parasites & helminths datasets from the testing group, and so the datasets selected based on number of species, number of PCs and median prevalence were selected for the testing group. The features by training and testing datasets were saved as 'training_data_features' and testing_data_features', respectively in the folder: 'MEE_feature_ensemble/outputs/features/...'

> Training: helminths; fennoscandia_birds; victoria_plants; usa_trees; eelgrass; shrews; mussel_parasites; lion_infections; eucalyptus; usa_birds; swiss_forest; fish_parasites; brazil_fish; reptiles; canopy_ants; swissalps_plants; buffalo_infections; finland_beetles; germany_beetles; nz_forest 

> Testing: bird_parasites; uk_butterflies; norway_vegetation; grassland_birds; mulu_birds; swiss_birds; earthworms; vines; andean_birds; norway_beetles 
    


#### STEP 2: Obtaining model weights from training data ####

#1. Open the workflow script for Step 2: MEE_feature_ensemble/r_scripts/workflow_code/2.getting_model_weights.R
> This script contains the code required to load datasets, set seeds and split proportion for the training datasets, standardises the names of the datasets to feed through the workflow_function (see #3); and saves the output to a folder. 

#2. Load packages required: 'caret'; 'dplyr'; 'gbm'; 'mgcv'; 'purrr'; 'randomForestSRC'; 'ggplot2'; 'Hmsc'; 'parallel'; 'MRFcov'

#3. Load the Functions required from 'functions' folder: 'stacking_function.R'; 'workflow_function.R' (MEE_feature_ensemble/functions/...) 
> The 'stacking_function.R' function is used to fit individual models, obtain probability of occurrence; binarise probability of occurrence; optimises model weights (model weight as output); obtain metrics (F1, Recall and Precision as output) & contains code required for graphing boxplots
> The 'workflow_function.R' function is used to facilitate and standardise the process of feeding the datasets to the stacking_function, and returns the output from the stacking_function. 

#4. run entire code. Output will be metrics for training datasets, including prevalence, model weights, Recall, Precision and F1. Save in 'training_data_outputs' folder: MEE_feature_ensemble/training_data_outputs/...

Note: Each dataset in the '1.getting_model_weights.R' script is numbered based on the number assigned across all 30 datasets used in this study (See 'dataset_information.xlsx' for all dataset descriptions: MEE_feature_ensemble/data/dataset_information.xlsx). Only the 20 training datasets are included in this script. For training/testing dataset selection process, see 'stratifying_variables.R' code (MEE_feature_ensemble/r_scripts/data_preparation_code/stratifying_variables.R) and 'selection_of_testing_datasets.xlsx' file (MEE_feature_ensemble/outputs/manual_processing/selection_of_testing_datasets.xlsx) 


***** STEP C: Merging model weights and features for training ensemble *****

#1. Open the data preparation script for merging weights and features (Step C): MEE_feature_ensemble/r_scripts/data_preparation_code/C.merging_metrics_and_features_for_training.R

#2. Load packages required: 'dplyr'; 'tidyverse' 

#3. Load all metrics for the training data: MEE_feature_ensemble/outputs/training_data_outputs/...

#4. Follow the workflow to prepare the data. This will create a dataframe containing the weights and features for each species in the training dataset (saved as weights_and_features.rda in MEE_feature_ensemble/outputs/training_data_outputs/weights_and_features.rda) 

Note: Simply merging the model weights from training and the features will contain some species with no features. This is because the training model weights will contain information for all species, while the features data will have fewer species, since some were removed in the workflow. To account for this, NAs should be removed prior to saving the 'weights_and_feautures' file. 




#### STEP 3: Training the Ensemble Model #### 

#1. Open the workflow script for Step 3: MEE_feature_ensemble/r_scripts/workflow_code/3.training_ensemble.R

#2. Load packages required: 'caret'; 'dplyr'; 'gbm'; 'mgcv'; 'purrr'; 'randomForestSRC'; 'ggplot2'; 'Hmsc'; 'parallel'; 'MRFcov'; 'tidyr' 

#3. Load required data (from training datasets) containing information on model weights and features: MEE_feature_ensemble/training_data_outputs/weights_and_features.rda

#4. Split this dataset into ys (model weights) and xs (features). Note that some variables are excluded - these are the species names and the median prevalence value (med_prev) used for stratification (Step B). 

#5. Fit the random forest model as ensemble using specifications in the code. 

#6. Save ensemble model as RDS and rda file in the 'functions' folder: MEE_feature_ensemble/functions/ensemble_model.(rds OR rda) 



#### STEP 4: Testing ensemble performance against individual models ####

#1. Open the workflow script for Step 3: MEE_feature_ensemble/r_scripts/workflow_code/4.testing_ensemble.R

#2. Load packages required: 'caret'; 'dplyr'; 'gbm'; 'mgcv'; 'purrr'; 'randomForestSRC'; 'ggplot2'; 'Hmsc'; 'parallel'; 'MRFcov'; 'tidyr' 

#3. Load the Functions required from 'functions' folder: 'stacking_function.R'; "predictions_function.R' (MEE_feature_ensemble/functions/...) 

#4. Load ensemble model 

#5. Specify the split proportion for cross-validation 

#6. Run through workflow script for each dataset. This includes loading datasets from 'data/datasets_for_analysis' folder and loading features from 'outputs/features' folder and running these through the prediction function

#7. Save the output as test_predictions (MEE_feature_ensemble/outputs/test_predictions/...) 

Note: The difference between the prediction_function and the workflow_function used in STEP 2 is that the prediction_function will only return probability predictions of species occurrence for each species, whereas the workflow_function will return model weights, metrics and metric ranks. 



***** STEP D: Merging Predictions ***** 

Note: The purpose of this script is to merge the predictions for visualising results. 



############################################# Visualising Results #############################################

STEP E: use to visualise dataset similarity using PCA analysis 
STEP F: visualise feature distributions (histograms and scatterplots) 
STEP G: visualise boxplots and net improvement tables for the training data after Step 2 
STEP H: visualise feature relative importance (heatmaps) and response functions after training ensemble (Step 3) 
STEP I: visualise the boxplots and net improvement tables for the testing data after Step 4
STEP J: calculating deviance residuals as performance metric from probability predictions after Step 4 
STEP K: calculating accuracy as performance metric from binary predictions with a 0.5 threshold after Step 4 


############################################# R Packages and versions used #############################################

> sessionInfo()
R version 4.0.2 (2020-06-22)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252    LC_MONETARY=English_Australia.1252
[4] LC_NUMERIC=C                       LC_TIME=English_Australia.1252    

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] StatMeasures_1.0       gridExtra_2.3          cowplot_1.1.1          viridis_0.6.2          viridisLite_0.4.0     
 [6] devtools_2.3.2         usethis_2.0.1          factoextra_1.0.7       Rcpp_1.0.8.3           forcats_0.5.1         
[11] stringr_1.4.0          readr_1.4.0            tidyr_1.1.3            tibble_3.0.6           tidyverse_1.3.0       
[16] gjam_2.3.6             dplyr_1.0.5            MRFcov_1.0.38          glmnet_4.1             Matrix_1.2-18         
[21] Hmsc_3.0-11            coda_0.19-4            randomForestSRC_2.11.0 purrr_0.3.4            mgcv_1.8-35           
[26] nlme_3.1-152           gbm_2.1.8              caret_6.0-86           ggplot2_3.3.5          lattice_0.20-41       

loaded via a namespace (and not attached):
  [1] colorspace_2.0-0     ellipsis_0.3.1       class_7.3-17         rprojroot_2.0.2      fs_1.5.0             rstudioapi_0.13     
  [7] remotes_2.2.0        MatrixModels_0.4-1   ggrepel_0.9.1        prodlim_2019.11.13   fansi_0.4.2          lubridate_1.7.9.2   
 [13] xml2_1.3.2           codetools_0.2-16     splines_4.0.2        cachem_1.0.3         pkgload_1.2.0        spam_2.6-0          
 [19] jsonlite_1.7.2       pROC_1.17.0.1        mcmc_0.9-7           broom_0.7.5          dbplyr_2.1.0         data.tree_1.0.0     
 [25] DiagrammeR_1.0.6.1   compiler_4.0.2       httr_1.4.2           backports_1.2.1      fastmap_1.1.0        assertthat_0.2.1    
 [31] cli_2.3.1            prettyunits_1.1.1    visNetwork_2.0.9     htmltools_0.5.1.1    quantreg_5.83        tools_4.0.2         
 [37] dotCall64_1.0-1      gtable_0.3.0         glue_1.4.2           RANN_2.6.1           reshape2_1.4.4       maps_3.3.0          
 [43] tinytex_0.29         cellranger_1.1.0     vctrs_0.3.6          ape_5.4-1            conquer_1.0.2        iterators_1.0.13    
 [49] timeDate_3043.102    gower_0.2.2          xfun_0.21            ps_1.6.0             testthat_3.0.2       rvest_0.3.6         
 [55] lifecycle_1.0.0      statmod_1.4.35       MASS_7.3-51.6        scales_1.1.1         ipred_0.9-9          hms_1.0.0           
 [61] SparseM_1.78         RColorBrewer_1.1-2   fields_11.6          memoise_2.0.0        rpart_4.1-15         stringi_1.5.3       
 [67] desc_1.2.0           foreach_1.5.1        pkgbuild_1.2.0       lava_1.6.8.1         truncnorm_1.0-8      shape_1.4.5         
 [73] rlang_0.4.10         pkgconfig_2.0.3      matrixStats_0.58.0   pracma_2.3.3         recipes_0.1.15       htmlwidgets_1.5.3   
 [79] processx_3.4.5       tidyselect_1.1.0     plyr_1.8.6           magrittr_2.0.1       R6_2.5.0             generics_0.1.0      
 [85] BayesLogit_2.1       DBI_1.1.1            pillar_1.6.0         haven_2.3.1          withr_2.4.1          survival_3.1-12     
 [91] abind_1.4-5          sp_1.4-5             nnet_7.3-14          modelr_0.1.8         crayon_1.4.1         utf8_1.1.4          
 [97] grid_4.0.2           readxl_1.3.1         data.table_1.14.0    callr_3.5.1          FNN_1.1.3            ModelMetrics_1.2.2.2
[103] reprex_1.0.0         digest_0.6.27        MCMCpack_1.5-0       stats4_4.0.2         munsell_0.5.0        sessioninfo_1.1.1 

