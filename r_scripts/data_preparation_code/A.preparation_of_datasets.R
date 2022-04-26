

if(!require(caret)){
  install.packages('caret')
}

if(!require(dplyr)){
  install.packages('dplyr')
}

if(!require(gbm)){
  install.packages('gbm')
}

if(!require(mgcv)){
  install.packages('mgcv')
}

if(!require(purrr)){
  install.packages('purrr')
}

if(!require(randomForestSRC)){
  install.packages('randomForestSRC')
}

if(!require(ggplot2)){
  install.packages('ggplot2')
}

if(!require(Hmsc)){
  install.packages('Hmsc')
}


if(!require(parallel)){
  install.packages('parallel')
}

if(!require(MRFcov)){
  install.packages('MRFcov')
}

setwd("C:/.../MEE_feature_ensemble")

## 1. bird_parasites ####

bird_parasites <- read.csv('data/raw_data/1_raw_bird_parasites_all.csv', as.is = T)

# remove NAs 
bird_parasites <- na.omit(bird_parasites)

## separating ys and xs 

bird_parasites_ys <- bird_parasites[1:4]
bird_parasites_prep_xs <- bird_parasites[5]

names <- c(1:4)
names <- as.character(names)       

## recode count data to 1s and 0s 
bird_parasites_ys <- bird_parasites_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(bird_parasites_ys) <- paste("bird_parasite" ,names, sep="_")

save(bird_parasites_ys, file = "data/datasets_for_analysis/bird_parasites_ys.rda")
save(bird_parasites_prep_xs, file = "data/data_prep/bird_parasites_prep_xs.rda") 

## 2. helminths ####

helminths <- read.csv('data/raw_data/2_raw_helminths_all.csv', as.is = T)

# remove NAs 
helminths <- na.omit(helminths)

## separating ys and xs 

helminths_ys <- helminths[1:4]
helminths_prep_xs <- helminths[5:23]

names <- c(1:4)
names <- as.character(names)       

## recode count data to 1s and 0s 
helminths_ys <- helminths_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(helminths_ys) <- paste("helminth" ,names, sep="_")

save(helminths_ys, file = "data/datasets_for_analysis/helminths_ys.rda")
save(helminths_prep_xs, file = "data/data_prep/helminths_prep_xs.rda")


## 3. fennoscandia_birds ####

# open datasets 

fennoscandia_1_birds_ys <- read.csv('data/raw_data/3_raw_fennoscandia_birds_ys(1).csv', as.is = T, header = F)
fennoscandia_1_birds_xs <- read.csv('data/raw_data/3_raw_fennoscandia_birds_xs(1).csv', as.is = T, header = F)

fennoscandia_2_birds_ys <- read.csv('data/raw_data/3_raw_fennoscandia_birds_ys(2).csv', as.is = T, header = F)
fennoscandia_2_birds_xs <- read.csv('data/raw_data/3_raw_fennoscandia_birds_xs(2).csv', as.is = T, header = F)

fennoscandia_3_birds_ys <- read.csv('data/raw_data/3_raw_fennoscandia_birds_ys(3).csv', as.is = T, header = F)
fennoscandia_3_birds_xs <- read.csv('data/raw_data/3_raw_fennoscandia_birds_xs(3).csv', as.is = T, header = F)

# bind datasets to form one dataset 

fennoscandia_birds_ys <- rbind(fennoscandia_1_birds_ys, fennoscandia_2_birds_ys, fennoscandia_3_birds_ys)
fennoscandia_birds_prep_xs <- rbind(fennoscandia_1_birds_xs, fennoscandia_2_birds_xs, fennoscandia_3_birds_xs)

#remove NAs
fennoscandia_birds_ys <- na.omit(fennoscandia_birds_ys)
fennoscandia_birds_prep_xs <- na.omit(fennoscandia_birds_prep_xs)


# rename each species (ys)

names <- c(1:141)
names <- as.character(names)  

colnames(fennoscandia_birds_ys) <- paste("fennoscandia_birds" ,names, sep="_")

# save ys 

save(fennoscandia_birds_ys, file = "data/datasets_for_analysis/fennoscandia_birds_ys.rda" )

# rename covariates 

names <- c(1:5)
names <- as.character(names)  

colnames(fennoscandia_birds_prep_xs) <- paste("predictor" ,names, sep="_")


# save xs  

save(fennoscandia_birds_prep_xs, file = "data/data_prep/fennoscandia_birds_prep_xs.rda" )

## 4. uk_butterflies####

# open datasets 

uk_1_butterflies_ys <- read.csv('data/raw_data/4_raw_uk_butterflies_ys(1).csv', as.is = T, header = F)
uk_1_butterflies_xs <- read.csv('data/raw_data/4_raw_uk_butterflies_xs(1).csv', as.is = T, header = F)

uk_2_butterflies_ys <- read.csv('data/raw_data/4_raw_uk_butterflies_ys(2).csv', as.is = T, header = F)
uk_2_butterflies_xs <- read.csv('data/raw_data/4_raw_uk_butterflies_xs(2).csv', as.is = T, header = F)

uk_3_butterflies_ys <- read.csv('data/raw_data/4_raw_uk_butterflies_ys(3).csv', as.is = T, header = F)
uk_3_butterflies_xs <- read.csv('data/raw_data/4_raw_uk_butterflies_xs(3).csv', as.is = T, header = F)

# bind datasets to form one dataset 

uk_butterflies_ys <- rbind(uk_1_butterflies_ys, uk_2_butterflies_ys, uk_3_butterflies_ys)
uk_butterflies_prep_xs <- rbind(uk_1_butterflies_xs, uk_2_butterflies_xs, uk_3_butterflies_xs)

#remove NAs
uk_butterflies_ys <- na.omit(uk_butterflies_ys)
uk_butterflies_prep_xs <- na.omit(uk_butterflies_prep_xs)

# rename each species (ys)

names <- c(1:50)
names <- as.character(names)  

colnames(uk_butterflies_ys) <- paste("uk_butterfly" ,names, sep="_")

# save ys 

save(uk_butterflies_ys, file = "data/datasets_for_analysis/uk_butterflies_ys.rda" )

# rename covariates 

names <- c(1:5)
names <- as.character(names)  

colnames(uk_butterflies_prep_xs) <- paste("predictor" ,names, sep="_")


# save xs  

save(uk_butterflies_prep_xs, file = "data/data_prep/uk_butterflies_prep_xs.rda" )

## 5. victoria_plants ####

# open datasets 

victoria_1_plants_ys <- read.csv('data/raw_data/5_raw_victoria_plants_ys(1).csv', as.is = T, header = F)
victoria_1_plants_xs <- read.csv('data/raw_data/5_raw_victoria_plants_xs(1).csv', as.is = T, header = F)

victoria_2_plants_ys <- read.csv('data/raw_data/5_raw_victoria_plants_ys(2).csv', as.is = T, header = F)
victoria_2_plants_xs <- read.csv('data/raw_data/5_raw_victoria_plants_xs(2).csv', as.is = T, header = F)

victoria_3_plants_ys <- read.csv('data/raw_data/5_raw_victoria_plants_ys(3).csv', as.is = T, header = F)
victoria_3_plants_xs <- read.csv('data/raw_data/5_raw_victoria_plants_xs(3).csv', as.is = T, header = F)

# bind datasets to form one dataset 

victoria_plants_ys <- rbind(victoria_1_plants_ys, victoria_2_plants_ys, victoria_3_plants_ys)
victoria_plants_prep_xs <- rbind(victoria_1_plants_xs, victoria_2_plants_xs, victoria_3_plants_xs)

#remove NAs
victoria_plants_ys <- na.omit(victoria_plants_ys)
victoria_plants_prep_xs <- na.omit(victoria_plants_prep_xs)

# rename each species (ys)

names <- c(1:162)
names <- as.character(names)  

colnames(victoria_plants_ys) <- paste("victoria_plant" ,names, sep="_")

# save ys 

save(victoria_plants_ys, file = "data/datasets_for_analysis/victoria_plants_ys.rda" )

# rename covariates 

names <- c(1:5)
names <- as.character(names)  

colnames(victoria_plants_prep_xs) <- paste("predictor" ,names, sep="_")

# save xs  

save(victoria_plants_prep_xs, file = "data/data_prep/victoria_plants_prep_xs.rda" )

## 6. usa_trees ####

# open datasets 

usa_1_trees_ys <- read.csv('data/raw_data/6_raw_usa_trees_ys(1).csv', as.is = T, header = F)
usa_1_trees_xs <- read.csv('data/raw_data/6_raw_usa_trees_xs(1).csv', as.is = T, header = F)

usa_2_trees_ys <- read.csv('data/raw_data/6_raw_usa_trees_ys(2).csv', as.is = T, header = F)
usa_2_trees_xs <- read.csv('data/raw_data/6_raw_usa_trees_xs(2).csv', as.is = T, header = F)

usa_3_trees_ys <- read.csv('data/raw_data/6_raw_usa_trees_ys(3).csv', as.is = T, header = F)
usa_3_trees_xs <- read.csv('data/raw_data/6_raw_usa_trees_xs(3).csv', as.is = T, header = F)

# bind datasets to form one dataset 

usa_trees_ys <- rbind(usa_1_trees_ys, usa_2_trees_ys, usa_3_trees_ys)
usa_trees_prep_xs <- rbind(usa_1_trees_xs, usa_2_trees_xs, usa_3_trees_xs)

#remove NAs
usa_trees_ys <- na.omit(usa_trees_ys)
usa_trees_prep_xs <- na.omit(usa_trees_prep_xs)

# rename each species (ys)

names <- c(1:63)
names <- as.character(names)  

colnames(usa_trees_ys) <- paste("usa_tree" ,names, sep="_")

# save ys 

save(usa_trees_ys, file = "data/datasets_for_analysis/usa_trees_ys.rda" )

# rename covariates 

names <- c(1:3)
names <- as.character(names)  

colnames(usa_trees_prep_xs) <- paste("predictor" ,names, sep="_")

# save xs  

save(usa_trees_prep_xs, file = "data/data_prep/usa_trees_prep_xs.rda" )
## 7. norway_vegetation ####

# open datasets 

norway_1_vegetation_ys <- read.csv('data/raw_data/7_raw_norway_vegetation_ys(1).csv', as.is = T, header = F)
norway_1_vegetation_xs <- read.csv('data/raw_data/7_raw_norway_vegetation_xs(1).csv', as.is = T, header = F)

norway_2_vegetation_ys <- read.csv('data/raw_data/7_raw_norway_vegetation_ys(2).csv', as.is = T, header = F)
norway_2_vegetation_xs <- read.csv('data/raw_data/7_raw_norway_vegetation_xs(2).csv', as.is = T, header = F)

norway_3_vegetation_ys <- read.csv('data/raw_data/7_raw_norway_vegetation_ys(3).csv', as.is = T, header = F)
norway_3_vegetation_xs <- read.csv('data/raw_data/7_raw_norway_vegetation_xs(3).csv', as.is = T, header = F)

# bind datasets to form one dataset 

norway_vegetation_ys <- rbind(norway_1_vegetation_ys, norway_2_vegetation_ys, norway_3_vegetation_ys)
norway_vegetation_prep_xs <- rbind(norway_1_vegetation_xs, norway_2_vegetation_xs, norway_3_vegetation_xs)

#remove NAs
norway_vegetation_ys <- na.omit(norway_vegetation_ys)
norway_vegetation_prep_xs <- na.omit(norway_vegetation_prep_xs)

# rename each species (ys)

names <- c(1:242)
names <- as.character(names)  

colnames(norway_vegetation_ys) <- paste("norway_vegetation" ,names, sep="_")

# save ys 

save(norway_vegetation_ys, file = "data/datasets_for_analysis/norway_vegetation_ys.rda" )

# rename covariates 

names <- c(1:4)
names <- as.character(names)  

colnames(norway_vegetation_prep_xs) <- paste("predictor" ,names, sep="_")

# save xs  

save(norway_vegetation_prep_xs, file = "data/data_prep/norway_vegetation_prep_xs.rda" )

## 8. eelgrass #### 

eelgrass_ys <- read.csv('data/raw_data/8_raw_eelgrass_ys.csv', as.is = T)
eelgrass_xs_1 <- read.csv('data/raw_data/8_raw_eelgrass_xs(1).csv', as.is = T)
eelgrass_xs_2 <- read.csv('data/raw_data/8_raw_eelgrass_xs(2).csv', as.is = T)

# change name of site ID for xs_2 and ys

eelgrass_xs_2 <- rename(eelgrass_xs_2, Name = quad) 
eelgrass_ys <- rename(eelgrass_ys, Name = X)


# bind the x datasets together 
eelgrass_xs <- inner_join(x=eelgrass_xs_1, y=eelgrass_xs_2, by="Name")


#remove NAs from ys
eelgrass_ys <- na.omit(eelgrass_ys)

# bind all datasets together 

eelgrass <- inner_join(x=eelgrass_ys, y=eelgrass_xs, by="Name")

#remove NAs
eelgrass <- na.omit(eelgrass)

## separating ys and xs from swiss forest data 

cols <- c(1, 35:49)

eelgrass_ys <- eelgrass[2:34]
eelgrass_prep_xs <- eelgrass[,cols]

names <- c(1:33)
names <- as.character(names)       

## recode count data to 1s and 0s 
eelgrass_ys <- eelgrass_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(eelgrass_ys) <- paste("eelgrass" ,names, sep="_")

save(eelgrass_ys, file = "data/datasets_for_analysis/eelgrass_ys.rda")

# removing ID from xs (readu for PCA analysis)

eelgrass_prep_xs <- eelgrass_prep_xs %>% 
  select(-Name)

save(eelgrass_prep_xs, file = "data/data_prep/eelgrass_prep_xs.rda")

## 9. shrews #### 

shrews <- read.csv('data/raw_data/9_raw_shrews_all.csv', as.is = T)

shrews <- na.omit(shrews)

# non-consecutive columns for covariates 
cols <- c(1,2,10:15)

## separating ys and xs 

shrews_ys <- shrews[3:9]
shrews_prep_xs <- shrews[,cols]

names <- c(1:7)
names <- as.character(names)       

## recode count data to 1s and 0s 
shrews_ys <- shrews_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(shrews_ys) <- paste("shrews" ,names, sep="_")

save(shrews_ys, file = "data/datasets_for_analysis/shrews_ys.rda")
save(shrews_prep_xs, file = "data/data_prep/shrews_prep_xs.rda")

## 10. mussel parasites #### 

mussel_parasites_ys <- read.csv('data/raw_data/10_raw_mussel_parasites_ys.csv', as.is = T)
mussel_parasites_xs <- read.csv('data/raw_data/10_raw_mussel_parasites_xs.csv', as.is = T)

mussel_parasites <- cbind(mussel_parasites_ys, mussel_parasites_xs)

mussel_parasites <- na.omit(mussel_parasites)

## separating ys and xs 

cols <- c(1,2,17:21)

mussel_parasites_ys <- mussel_parasites[3:16]
mussel_parasites_prep_xs <- mussel_parasites[,cols]

names <- c(1:14)
names <- as.character(names)       

## recode count data to 1s and 0s 
mussel_parasites_ys <- mussel_parasites_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(mussel_parasites_ys) <- paste("mussel_parasite" ,names, sep="_")

save(mussel_parasites_ys, file = "data/datasets_for_analysis/mussel_parasites_ys.rda")

# Remove ID category from x variables (Ready for PCA analysis)

mussel_parasites_prep_xs <- mussel_parasites_prep_xs %>% 
  select(-mussel)

# data has 3 character variables. Convert to numeric.(Ready for PCA analysis)

mussel_parasites_prep_xs$gravid <- as.factor(mussel_parasites_prep_xs$gravid)
mussel_parasites_prep_xs$gravid <- as.numeric(mussel_parasites_prep_xs$gravid)

mussel_parasites_prep_xs$zebra <- as.factor(mussel_parasites_prep_xs$zebra)
mussel_parasites_prep_xs$zebra <- as.numeric(mussel_parasites_prep_xs$zebra)

mussel_parasites_prep_xs$month.1 <- as.factor(mussel_parasites_prep_xs$month.1)
mussel_parasites_prep_xs$month.1 <- as.numeric(mussel_parasites_prep_xs$month.1)

save(mussel_parasites_prep_xs, file = "data/data_prep/mussel_parasites_prep_xs.rda")

## 11. lion_infections #### 

lion_infections_ys <- read.csv('data/raw_data/11_lion_infections_ys.csv', as.is = T)
lion_infections_xs <- read.csv('data/raw_data/11_lion_infections_xs.csv', as.is = T)

lion_infections_ys <- rename(lion_infections_ys, ID = ï..Lion_ID) 
lion_infections_xs <- rename(lion_infections_xs, ID = ï..ID) 

lion_infections <- inner_join(x=lion_infections_ys, y=lion_infections_xs, by="ID")

lion_infections <- na.omit(lion_infections) 

## separating ys and xs 
cols <- c(1,7:17)

lion_infections_ys <- lion_infections[2:6]
lion_infections_prep_xs <- lion_infections[, cols]


names <- c(1:5)
names <- as.character(names)       

## recode count data to 1s and 0s 
lion_infections_ys <- lion_infections_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(lion_infections_ys) <- paste("lion_infection" ,names, sep="_")

save(lion_infections_ys, file = "data/datasets_for_analysis/lion_infections_ys.rda")

#remove ID 

lion_infections_prep_xs <- lion_infections_prep_xs %>% 
  select(-ID)

save(lion_infections_prep_xs, file = "data/data_prep/lion_infections_prep_xs.rda")

## 12. eucalyptus #### 

eucalyptus <- read.csv('data/raw_data/12_raw_eucalyptus_all.csv', as.is = T)

eucalyptus <- eucalyptus %>% mutate_all(na_if,"")
eucalyptus <- na.omit(eucalyptus)

## separating ys and xs 

eucalyptus_ys <- eucalyptus[36:55]
eucalyptus_prep_xs <- eucalyptus[1:35]


names <- c(1:20)
names <- as.character(names)       

## recode count data to 1s and 0s 
eucalyptus_ys <- eucalyptus_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(eucalyptus_ys) <- paste("eucalyptus" ,names, sep="_")

save(eucalyptus_ys, file = "data/datasets_for_analysis/eucalyptus_ys.rda")


# remove ID columns (Ready for PCA)

eucalyptus_prep_xs <- eucalyptus_prep_xs %>% 
  select(-ï..PLOTID, -IDENT)

# converting character variables to numeric (Ready for PCA)

eucalyptus_prep_xs$GeolClass <- as.factor(eucalyptus_prep_xs$GeolClass)
eucalyptus_prep_xs$GeolClass <- as.numeric(eucalyptus_prep_xs$GeolClass)

eucalyptus_prep_xs$SOIL <- as.factor(eucalyptus_prep_xs$SOIL)
eucalyptus_prep_xs$SOIL <- as.numeric(eucalyptus_prep_xs$SOIL)


save(eucalyptus_prep_xs, file = "data/data_prep/eucalyptus_prep_xs.rda")

## 13. grassland birds #### 

grassland_birds <- read.csv('data/raw_data/13_raw_grassland_birds_all.csv', as.is = T)

grassland_birds <- na.omit(grassland_birds)

## separating ys and xs 

grassland_birds_ys <- grassland_birds[6:35]
grassland_birds_prep_xs <- grassland_birds[1:5]


names <- c(1:30)
names <- as.character(names)       

## recode count data to 1s and 0s 
grassland_birds_ys <- grassland_birds_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(grassland_birds_ys) <- paste("grassland_birds" ,names, sep="_")

save(grassland_birds_ys, file = "data/datasets_for_analysis/grassland_birds_ys.rda")

# remove columns with unique values for each row (Ready for PCA)

grassland_birds_prep_xs <- grassland_birds_prep_xs %>% 
  select(-sample)

save(grassland_birds_prep_xs, file = "data/data_prep/grassland_birds_prep_xs.rda")

## 14. mulu birds #### 

mulu_birds_ys <- read.csv('data/raw_data/14_raw_mulu_birds_ys.csv', as.is = T)
mulu_birds_xs <- read.csv('data/raw_data/14_raw_mulu_birds_xs.csv', as.is = T)


mulu_birds_ys <- rename(mulu_birds_ys, ID = ï..) 
mulu_birds_xs <- rename(mulu_birds_xs, ID = ï..PointName) 

mulu_birds <- inner_join(x=mulu_birds_ys, y=mulu_birds_xs, by="ID")


mulu_birds <- mulu_birds %>% mutate_all(na_if,"unkn")
mulu_birds <- na.omit(mulu_birds)

## separating ys and xs 
cols <- c(1, 86:88)

mulu_birds_ys <- mulu_birds[2:85]
mulu_birds_prep_xs <- mulu_birds[, cols]


names <- c(1:84)
names <- as.character(names)       

## recode count data to 1s and 0s 
mulu_birds_ys <- mulu_birds_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(mulu_birds_ys) <- paste("mulu_bird" ,names, sep="_")

save(mulu_birds_ys, file = "data/datasets_for_analysis/mulu_birds_ys.rda")

# remove ID (Ready for PCA)

mulu_birds_prep_xs <- mulu_birds_prep_xs %>%
  select(-ID)


# longitude and latidude in dataset are characters. swich to numeric. (Ready for PCA)

mulu_birds_prep_xs$Lon.WGS84. <- as.numeric(mulu_birds_prep_xs$Lon.WGS84.)
mulu_birds_prep_xs$Lat.WGS84. <- as.numeric(mulu_birds_prep_xs$Lat.WGS84.)

save(mulu_birds_prep_xs, file = "data/data_prep/mulu_birds_prep_xs.rda")

## 15. usa_birds ####

usa_birds <- read.csv('data/raw_data/15_raw_usa_birds_all.csv', as.is = T)

usa_birds <- na.omit(usa_birds)

## separating ys and xs from swiss forest data 

usa_birds_ys <- usa_birds[31:132]
usa_birds_prep_xs <- usa_birds[1:30]

names <- c(1:102)
names <- as.character(names)       

## recode count data to 1s and 0s 
usa_birds_ys <- usa_birds_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(usa_birds_ys) <- paste("usa_birds" ,names, sep="_")

save(usa_birds_ys, file = "data/datasets_for_analysis/usa_birds_ys.rda")

# remove columns with unique values for each row or the same for each row (Ready for PCA)

usa_birds_prep_xs <- usa_birds_prep_xs %>% 
  select(-duration_minutes, -distance)

save(usa_birds_prep_xs, file = "data/data_prep/usa_birds_prep_xs.rda")

## 16. swiss birds ####

swiss_birds <- read.csv('data/raw_data/16_raw_swiss_birds_all.csv', as.is = T)

swiss_birds <- na.omit(swiss_birds)

## separating ys and xs 

swiss_birds_ys <- swiss_birds[1:56]
swiss_birds_prep_xs <- swiss_birds[57:110]

names <- c(1:56)
names <- as.character(names)       

## recode count data to 1s and 0s 
swiss_birds_ys <- swiss_birds_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(swiss_birds_ys) <- paste("swiss_bird" ,names, sep="_")

save(swiss_birds_ys, file = "data/datasets_for_analysis/swiss_birds_ys.rda")

# remove columns with unique values for each row or the same for each row (Ready for PCA)

swiss_birds_prep_xs <- swiss_birds_prep_xs %>% 
  select(-data_type)

save(swiss_birds_prep_xs, file = "data/data_prep/swiss_birds_prep_xs.rda")

## 17. swiss forest ####

swiss_forest <- read.csv('data/raw_data/17_raw_swiss_forest_all.csv', as.is = T)

swiss_forest <- na.omit(swiss_forest)

## separating ys and xs 

swiss_forest_ys <- swiss_forest[1:63]
swiss_forest_prep_xs <- swiss_forest[64:109]

names <- c(1:63)
names <- as.character(names)       

## recode count data to 1s and 0s 
swiss_forest_ys <- swiss_forest_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

colnames(swiss_forest_ys) <- paste("swiss_forest" ,names, sep="_")

save(swiss_forest_ys, file = "data/datasets_for_analysis/swiss_forest_ys.rda")

# remove columns with unique values for each row or the same for each row (Ready for PCA)

swiss_forest_prep_xs <- swiss_forest_prep_xs %>% 
  select(-dat_type)

save(swiss_forest_prep_xs, file = "data/data_prep/swiss_forest_prep_xs.rda")

## 18. fish parasites ####

fish_parasites <- read.csv('data/raw_data/18_raw_fish_parasites_all.csv', as.is = T)

# remove NAs 
fish_parasites <- na.omit(fish_parasites)

## separating ys and xs 
cols <- c(1:7, 50)

fish_parasites_ys <- fish_parasites[8:49]
fish_parasites_prep_xs <- fish_parasites[, cols]

names <- c(1:42)
names <- as.character(names)       

## recode count data to 1s and 0s 
fish_parasites_ys <- fish_parasites_ys %>% mutate_if(is.numeric, ~1 * (. > 0))


colnames(fish_parasites_ys) <- paste("fish_parasite" ,names, sep="_")

save(fish_parasites_ys, file = "data/datasets_for_analysis/fish_parasites_ys.rda")

# convert character variables to numeric (ready for PCA)

fish_parasites_prep_xs$site_name <- as.factor(fish_parasites_prep_xs$site_name)
fish_parasites_prep_xs$site_name <- as.numeric(fish_parasites_prep_xs$site_name)

fish_parasites_prep_xs$habitat_type <- as.factor(fish_parasites_prep_xs$habitat_type)
fish_parasites_prep_xs$habitat_type <- as.numeric(fish_parasites_prep_xs$habitat_type)

fish_parasites_prep_xs$watershed <- as.factor(fish_parasites_prep_xs$watershed)
fish_parasites_prep_xs$watershed <- as.numeric(fish_parasites_prep_xs$watershed)

fish_parasites_prep_xs$sex <- as.factor(fish_parasites_prep_xs$sex)
fish_parasites_prep_xs$sex <- as.numeric(fish_parasites_prep_xs$sex)

save(fish_parasites_prep_xs, file = "data/data_prep/fish_parasites_prep_xs.rda")

## 19. brazil fish ####

brazil_fish <- read.csv('data/raw_data/19_raw_brazil_fish_all.csv', as.is = T)

# remove NAs 
brazil_fish <- na.omit(brazil_fish)

## separating ys and xs 

brazil_fish_ys <- brazil_fish[6:97]
cols <- c(1:5, 98:105)
brazil_fish_prep_xs <- brazil_fish[, cols]


names <- c(1:92)
names <- as.character(names)       

colnames(brazil_fish_ys) <- paste("brazil_fish" ,names, sep="_")


## convert characters to numbers 
chars <- sapply(brazil_fish_ys, is.character)
brazil_fish_ys[ , chars] <- as.data.frame(apply(brazil_fish_ys[ , chars], 2, as.numeric))


## recode count data to 1s and 0s 
brazil_fish_ys <- brazil_fish_ys %>% mutate_if(is.numeric, ~1 * (. > 0))


save(brazil_fish_ys, file = "data/datasets_for_analysis/brazil_fish_ys.rda")

# removing ID column (ready for PCA)

brazil_fish_prep_xs <- brazil_fish_prep_xs %>% 
  select(-Sample.Unit)

# change character variables to numeric (ready for PCA)

brazil_fish_prep_xs$ï..Co.autor <- as.factor(brazil_fish_prep_xs$ï..Co.autor)
brazil_fish_prep_xs$ï..Co.autor <- as.numeric(brazil_fish_prep_xs$ï..Co.autor)

brazil_fish_prep_xs$Original.Spot <- as.factor(brazil_fish_prep_xs$Original.Spot)
brazil_fish_prep_xs$Original.Spot <- as.numeric(brazil_fish_prep_xs$Original.Spot)

save(brazil_fish_prep_xs, file = "data/data_prep/brazil_fish_prep_xs.rda")

## 20. reptiles ####

reptiles <- read.csv('data/raw_data/20_raw_reptiles_all.csv', as.is = T)

# remove NAs 
reptiles <- na.omit(reptiles)

## separating ys and xs 

reptiles_ys <- reptiles[15:118]
reptiles_prep_xs <- reptiles[1:14]


names <- c(1:104)
names <- as.character(names)       

colnames(reptiles_ys) <- paste("reptile" ,names, sep="_")


## recode count data to 1s and 0s 
reptiles_ys <- reptiles_ys %>% mutate_if(is.numeric, ~1 * (. > 0))


save(reptiles_ys, file = "data/datasets_for_analysis/reptiles_ys.rda")

#removing ID columns and columns with all the same value (ready for PCA)

reptiles_prep_xs <- reptiles_prep_xs %>%
  select(-ï..Island,-Island.Characteristics, -Species)

# transforming character variable to numeric (ready for PCA)

reptiles_prep_xs$Biogeographical.regIonian <- as.factor(reptiles_prep_xs$Biogeographical.regIonian)
reptiles_prep_xs$Biogeographical.regIonian <- as.numeric(reptiles_prep_xs$Biogeographical.regIonian)

save(reptiles_prep_xs, file = "data/data_prep/reptiles_prep_xs.rda")

## 21. canopy ants ####

canopy_ants <- read.csv('data/raw_data/21_raw_canopy_ants_all.csv', as.is = T)

# remove NAs 
canopy_ants <- na.omit(canopy_ants)

## separating ys and xs 

canopy_ants_ys <- canopy_ants[7:134]
canopy_ants_prep_xs <- canopy_ants[1:6]


names <- c(1:128)
names <- as.character(names)       

colnames(canopy_ants_ys) <- paste("canopy_ant" ,names, sep="_")


## recode count data to 1s and 0s 
canopy_ants_ys <- canopy_ants_ys %>% mutate_if(is.numeric, ~1 * (. > 0))


save(canopy_ants_ys, file = "data/datasets_for_analysis/canopy_ants_ys.rda")

# remove ID column 

canopy_ants_prep_xs <- canopy_ants_prep_xs %>% 
  select(-ï..Sample)

# convert character variable to numeric. 

canopy_ants_prep_xs$Tree.genera <- as.factor(canopy_ants_prep_xs$Tree.genera)
canopy_ants_prep_xs$Tree.genera <- as.numeric(canopy_ants_prep_xs$Tree.genera)

save(canopy_ants_prep_xs, file = "data/data_prep/canopy_ants_prep_xs.rda")

## 22. swissalps_plants ####

swissalps_plants <- read.csv('data/raw_data/22_raw_swissalps_plants_all.csv', as.is = T)

# remove NAs 
swissalps_plants <- na.omit(swissalps_plants)

## separating ys and xs 

swissalps_plants_ys <- swissalps_plants[9:183]
swissalps_plants_prep_xs <- swissalps_plants[1:8]


names <- c(1:175)
names <- as.character(names)       

colnames(swissalps_plants_ys) <- paste("swissalps_plant" ,names, sep="_")


## recode count data to 1s and 0s 
swissalps_plants_ys <- swissalps_plants_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

save(swissalps_plants_ys, file = "data/datasets_for_analysis/swissalps_plants_ys.rda")

#remove ID column (ready for PCA)

swissalps_plants_prep_xs <- swissalps_plants_prep_xs %>% 
  select(-ï..Plot_ID)

save(swissalps_plants_prep_xs, file = "data/data_prep/swissalps_plants_prep_xs.rda")

## 23. earthworms ####

earthworms <- read.csv('data/raw_data/23_raw_earthworms_all.csv', as.is = T)

# remove NAs 
earthworms <- na.omit(earthworms)

## separating ys and xs 

earthworms_ys <- earthworms[6:110]
earthworms_prep_xs <- earthworms[1:5]


names <- c(1:105)
names <- as.character(names)       

colnames(earthworms_ys) <- paste("earthworm" ,names, sep="_")


## recode count data to 1s and 0s 
earthworms_ys <- earthworms_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

save(earthworms_ys, file = "data/datasets_for_analysis/earthworms_ys.rda")


## remove ID (ready for PCA)

earthworms_prep_xs <- earthworms_prep_xs %>% 
  select(-ï..site)

# change landcover to numeric (ready for PCA)

earthworms_prep_xs$land_cover <- as.factor(earthworms_prep_xs$land_cover)
earthworms_prep_xs$land_cover <- as.numeric(earthworms_prep_xs$land_cover)

save(earthworms_prep_xs, file = "data/data_prep/earthworms_prep_xs.rda")

## 24. vines ####

vines_ys <- read.csv('data/raw_data/24_raw_vines_ys.csv', as.is = T)
vines_xs <- read.csv('data/raw_data/24_raw_vines_xs.csv', as.is = T)

# joining datasets 
vines_ys <- rename(vines_ys, ID = ï..VinePatch_ID) 
vines_xs <- rename(vines_xs, ID = ï..VinePatch_ID) 

vines <- inner_join(x=vines_ys, y=vines_xs, by="ID")

# remove NAs 
vines <- na.omit(vines)

## separating ys and xs 
cols <- c(1, 51:66)

vines_ys <- vines[2:50]
vines_prep_xs <- vines[, cols]


names <- c(1:49)
names <- as.character(names)       

colnames(vines_ys) <- paste("vine" ,names, sep="_")


## recode count data to 1s and 0s 
vines_ys <- vines_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

save(vines_ys, file = "data/datasets_for_analysis/vines_ys.rda")

## remove ID column (for PCA)

vines_prep_xs <- vines_prep_xs %>% 
  select(-ID)

save(vines_prep_xs, file = "data/data_prep/vines_prep_xs.rda")

## 25. buffalo infections ####

buffalo_infections <- read.csv('data/raw_data/25_raw_buffalo_infections_all.csv', as.is = T)

buffalo_infections <- buffalo_infections[buffalo_infections$bolus != "bolus", ] 

# remove NAs 
buffalo_infections <- buffalo_infections %>% mutate_all(na_if,"unknown")
buffalo_infections <- na.omit(buffalo_infections)

## separating ys and xs 

buffalo_infections_ys <- buffalo_infections[13:18]
buffalo_infections_prep_xs <- buffalo_infections[1:12]


names <- c(1:6)
names <- as.character(names)       

colnames(buffalo_infections_ys) <- paste("buffalo_infection" ,names, sep="_")


## recode count data to 1s and 0s 
buffalo_infections_ys <- buffalo_infections_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

save(buffalo_infections_ys, file = "data/datasets_for_analysis/buffalo_infections_ys.rda")

## remove ID columns and columns that are the same for all rows (ready for PCA)

buffalo_infections_prep_xs <- buffalo_infections_prep_xs %>% 
  select(-capture.id, -bolus)

# convert characters to numeric 
buffalo_infections_prep_xs$animal.id <- as.factor(buffalo_infections_prep_xs$animal.id)
buffalo_infections_prep_xs$animal.id <- as.numeric(buffalo_infections_prep_xs$animal.id)

buffalo_infections_prep_xs$capture.season.2 <- as.factor(buffalo_infections_prep_xs$capture.season.2)
buffalo_infections_prep_xs$capture.season.2 <- as.numeric(buffalo_infections_prep_xs$capture.season.2)

buffalo_infections_prep_xs$herd <- as.factor(buffalo_infections_prep_xs$herd)
buffalo_infections_prep_xs$herd <- as.numeric(buffalo_infections_prep_xs$herd)

buffalo_infections_prep_xs$pregnant <- as.factor(buffalo_infections_prep_xs$pregnant)
buffalo_infections_prep_xs$pregnant <- as.numeric(buffalo_infections_prep_xs$pregnant)

buffalo_infections_prep_xs$milk <- as.factor(buffalo_infections_prep_xs$milk)
buffalo_infections_prep_xs$milk <- as.numeric(buffalo_infections_prep_xs$milk)

buffalo_infections_prep_xs$calf.at.heel <- as.factor(buffalo_infections_prep_xs$calf.at.heel)
buffalo_infections_prep_xs$calf.at.heel <- as.numeric(buffalo_infections_prep_xs$calf.at.heel)

save(buffalo_infections_prep_xs, file = "data/data_prep/buffalo_infections_prep_xs.rda")

## 26. andean birds ####

andean_birds <- read.csv('data/raw_data/26_raw_andean_birds_all.csv', as.is = T)

# remove NAs 
andean_birds <- na.omit(andean_birds)

## separating ys and xs 
cols <- c(1, 166:167) 

andean_birds_ys <- andean_birds[2:165]
andean_birds_prep_xs <- andean_birds[, cols]


names <- c(1:164)
names <- as.character(names)       

colnames(andean_birds_ys) <- paste("andean_bird" ,names, sep="_")


## recode count data to 1s and 0s 
andean_birds_ys <- andean_birds_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

save(andean_birds_ys, file = "data/datasets_for_analysis/andean_birds_ys.rda")

## remove ID column (ready for PCA)

andean_birds_prep_xs <- andean_birds_prep_xs %>% 
  select(-Flock)

# convert characters to numeric 
andean_birds_prep_xs$Elevation.band <- as.factor(andean_birds_prep_xs$Elevation.band)
andean_birds_prep_xs$Elevation.band <- as.numeric(andean_birds_prep_xs$Elevation.band)

save(andean_birds_prep_xs, file = "data/data_prep/andean_birds_prep_xs.rda")

## 27. finland beetles ####

finland_beetles <- read.csv('data/raw_data/27_raw_finland_beetles_all.csv', as.is = T)

# remove NAs 
finland_beetles <- na.omit(finland_beetles)

## separating ys and xs 

finland_beetles_ys <- finland_beetles[19:259]
finland_beetles_prep_xs <- finland_beetles[1:18]


names <- c(1:241)
names <- as.character(names)       

colnames(finland_beetles_ys) <- paste("finland_beetle" ,names, sep="_")


## recode count data to 1s and 0s 
finland_beetles_ys <- finland_beetles_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

save(finland_beetles_ys, file = "data/datasets_for_analysis/finland_beetles_ys.rda")

# remove ID columns and columns that are the same for all rows

finland_beetles_prep_xs <- finland_beetles_prep_xs %>% 
  select(-Index, -TRAP_SIZE)

# convert characters to numeric 
finland_beetles_prep_xs$OBS_UNIT <- as.factor(finland_beetles_prep_xs$OBS_UNIT)
finland_beetles_prep_xs$OBS_UNIT <- as.numeric(finland_beetles_prep_xs$OBS_UNIT)

finland_beetles_prep_xs$DATA_STRUCT <- as.factor(finland_beetles_prep_xs$DATA_STRUCT)
finland_beetles_prep_xs$DATA_STRUCT <- as.numeric(finland_beetles_prep_xs$DATA_STRUCT)

save(finland_beetles_prep_xs, file = "data/data_prep/finland_beetles_prep_xs.rda")

## 28. germany beetles ####

germany_beetles <- read.csv('data/raw_data/28_raw_germany_beetles_all.csv', as.is = T)


# remove NAs 
germany_beetles <- na.omit(germany_beetles)

## separating ys and xs 

germany_beetles_ys <- germany_beetles[14:264]
germany_beetles_prep_xs <- germany_beetles[1:13]


names <- c(1:251)
names <- as.character(names)       

colnames(germany_beetles_ys) <- paste("germany_beetle" ,names, sep="_")


## recode count data to 1s and 0s 
germany_beetles_ys <- germany_beetles_ys %>% mutate_if(is.numeric, ~1 * (. > 0))


# n_species > 250. randomly selecting 75 species. 

germany_beetles_ys <- sample(germany_beetles_ys, 75)

save(germany_beetles_ys, file = "data/datasets_for_analysis/germany_beetles_ys.rda")

# remove ID columns and columns that are the same for all rows (ready for PCA)

germany_beetles_prep_xs <- germany_beetles_prep_xs %>% 
  select(-Index, -OBS_UNIT)

# convert characters to numeric 
germany_beetles_prep_xs$DATA_STRUCT <- as.factor(germany_beetles_prep_xs$DATA_STRUCT)
germany_beetles_prep_xs$DATA_STRUCT <- as.numeric(germany_beetles_prep_xs$DATA_STRUCT)

save(germany_beetles_prep_xs, file = "data/data_prep/germany_beetles_prep_xs.rda")

## 29. norway beetles ####

norway_beetles <- read.csv('data/raw_data/29_raw_norway_beetles_all.csv', as.is = T)


# remove NAs 
norway_beetles <- na.omit(norway_beetles)

## separating ys and xs 

norway_beetles_ys <- norway_beetles[16:399]
norway_beetles_prep_xs <- norway_beetles[1:15]


names <- c(1:384)
names <- as.character(names)       

colnames(norway_beetles_ys) <- paste("norway_beetle" ,names, sep="_")


## recode count data to 1s and 0s 
norway_beetles_ys <- norway_beetles_ys %>% mutate_if(is.numeric, ~1 * (. > 0))


# n_species > 250. randomly selecting 125 species. 

norway_beetles_ys <- sample(norway_beetles_ys, 125)

save(norway_beetles_ys, file = "data/datasets_for_analysis/norway_beetles_ys.rda")

## remove ID columns

norway_beetles_prep_xs <- norway_beetles_prep_xs %>% 
  select(-Index)

# convert characters to numeric 
norway_beetles_prep_xs$DATA_STRUCT <- as.factor(norway_beetles_prep_xs$DATA_STRUCT)
norway_beetles_prep_xs$DATA_STRUCT <- as.numeric(norway_beetles_prep_xs$DATA_STRUCT)

norway_beetles_prep_xs$OBS_UNIT <- as.factor(norway_beetles_prep_xs$OBS_UNIT)
norway_beetles_prep_xs$OBS_UNIT <- as.numeric(norway_beetles_prep_xs$OBS_UNIT)

save(norway_beetles_prep_xs, file = "data/data_prep/norway_beetles_prep_xs.rda")

## 30. new zealand forest ####

nz_forest <- read.csv('data/raw_data/30_raw_nzforest_all.csv', as.is = T)


# remove NAs 
nz_forest <- na.omit(nz_forest)


## separating ys and xs 

nz_forest_ys <- nz_forest[4:1834]
nz_forest_prep_xs <- nz_forest[1:3]

names <- c(1:1831)
names <- as.character(names)       

colnames(nz_forest_ys) <- paste("nz_forest" ,names, sep="_")


## recode count data to 1s and 0s 
nz_forest_ys <- nz_forest_ys %>% mutate_if(is.numeric, ~1 * (. > 0))

# removing columns with no presences 

nz_forest_ys <- nz_forest_ys[, colSums(nz_forest_ys != 0) > 0]

# n_species > 250. randomly selecting 205 species. 

nz_forest_ys <- sample(nz_forest_ys, 205)

save(nz_forest_ys, file = "data/datasets_for_analysis/nz_forest_ys.rda")

## remove ID columns (Ready for PCA)

nz_forest_prep_xs <- nz_forest_prep_xs %>% 
  select(-sites)

save(nz_forest_prep_xs, file = "data/data_prep/nz_forest_prep_xs.rda")