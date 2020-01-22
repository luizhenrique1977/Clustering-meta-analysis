### MEASURING COMPLEXITY OF CLUSTERING PROBLEMS ###

library(data.table)
library(ggplot2)
library(mlbench)
library(tseries)
library("ECoL", lib.loc="~/R/win-library/3.5")
setwd('C:/Users/Luiz Henrique/Desktop/Luiz Henrique/Doutorado/Pesquisas de Tese/R/Clustering')
source('meta_features_functions.R')


## Importing datasets

# Importing gaussian datasets (80 datasets)
setwd('C:/Users/Luiz Henrique/Desktop/Luiz Henrique/Doutorado/Pesquisas de Tese/R/Clustering/Datasets/Gaussian')
gauss_ds <- list()
names1 = list.files(pattern="*.dat")
for(i in 1:length(names1)){
  gauss_ds[[i]] <- fread(names1[i])
}

# Importing ellipsoidal datasets (80 datasets)
setwd('C:/Users/Luiz Henrique/Desktop/Luiz Henrique/Doutorado/Pesquisas de Tese/R/Clustering/Datasets/Ellipsoidal')
ellips_ds <- list()
names2 = list.files(pattern="*.dat")
for(i in 1:length(names2)){
  ellips_ds[[i]] <- fread(names2[i])
}

# Importing other datasets - small (90 datasets)
setwd('C:/Users/Luiz Henrique/Desktop/Luiz Henrique/Doutorado/Pesquisas de Tese/R/Clustering/Datasets/CSV/Small')
small_ds <- list()
names3 = list.files(pattern="*.csv")
for(i in 1:length(names3)){
  small_ds[[i]] <- fread(names3[i])
}

# Importing other datasets - medium (35 datasets)
setwd('C:/Users/Luiz Henrique/Desktop/Luiz Henrique/Doutorado/Pesquisas de Tese/R/Clustering/Datasets/CSV/Medium')
medium_ds <- list()
names4 = list.files(pattern="*.csv")
for(i in 1:length(names4)){
  medium_ds[[i]] <- fread(names4[i])
}

# Importing other datasets - large (10 datasets)
setwd('C:/Users/Luiz Henrique/Desktop/Luiz Henrique/Doutorado/Pesquisas de Tese/R/Clustering/Datasets/CSV/Large')
large_ds <- list()
names5 = list.files(pattern="*.csv")
for(i in 1:length(names5)){
  large_ds[[i]] <- fread(names5[i])
}

# Importing other datasets - xlarge (1 dataset)
setwd('C:/Users/Luiz Henrique/Desktop/Luiz Henrique/Doutorado/Pesquisas de Tese/R/Clustering/Datasets/CSV/XLarge')
xlarge_ds <- list()
names6 = list.files(pattern="*.csv")
for(i in 1:length(names6)){
  xlarge_ds[[i]] <- fread(names6[i])
}


## Calculating meta-features

# gaussian 
mds1 <- data.frame()
for(i in 1:length(gauss_ds)){
  mds1[i, 1] <- log_number_ex(gauss_ds[[i]])
  mds1[i, 2] <- log_number_ftr(gauss_ds[[i]])
  mds1[i, 3] <- ratio_ex_ftr(gauss_ds[[i]])
  mds1[i, 4] <- mvn_skewness(gauss_ds[[i]])
  mds1[i, 5] <- mvn_kurtosis(gauss_ds[[i]])
  mds1[i, 6] <- multi_norm(gauss_ds[[i]])
  mds1[i, 7] <- perc_out(gauss_ds[[i]])
  mds1[i, 8] <- avg_pca(gauss_ds[[i]])
  mds1[i, 9] <- ratio_pca(gauss_ds[[i]])
  mds1[i, 10] <- avg_abs_cor(gauss_ds[[i]])
  mds1[i, 11] <- per_low_dist(gauss_ds[[i]])
  mds1[i, 12] <- per_high_dist(gauss_ds[[i]])
  mds1[i, 13] <- eigen_cent_mst(gauss_ds[[i]])
  mds1[i, 14] <- net_dens(gauss_ds[[i]])
  mds1[i, 15] <- clust_coef(gauss_ds[[i]])
}
write.csv(mds1, "mds1.csv")

# ellipsoidal 
mds2 <- data.frame()
for(i in 1:length(ellips_ds)){
  mds2[i, 1] <- log_number_ex(ellips_ds[[i]])
  mds2[i, 2] <- log_number_ftr(ellips_ds[[i]])
  mds2[i, 3] <- ratio_ex_ftr(ellips_ds[[i]])
  mds2[i, 4] <- mvn_skewness(ellips_ds[[i]])
  mds2[i, 5] <- mvn_kurtosis(ellips_ds[[i]])
  mds2[i, 6] <- multi_norm(ellips_ds[[i]])
  mds2[i, 7] <- perc_out(ellips_ds[[i]])
  mds2[i, 8] <- avg_pca(ellips_ds[[i]])
  mds2[i, 9] <- ratio_pca(ellips_ds[[i]])
  mds2[i, 10] <- avg_abs_cor(ellips_ds[[i]])
  mds2[i, 11] <- per_low_dist(ellips_ds[[i]])
  mds2[i, 12] <- per_high_dist(ellips_ds[[i]])
  mds2[i, 13] <- eigen_cent_mst(ellips_ds[[i]])
  mds2[i, 14] <- net_dens(ellips_ds[[i]])
  mds2[i, 15] <- clust_coef(ellips_ds[[i]])
}
write.csv(mds2, "mds2.csv")

# CSV small 
mds3 <- data.frame()
for(i in 1:length(small_ds)){
  mds3[i, 1] <- log_number_ex(small_ds[[i]])
  mds3[i, 2] <- log_number_ftr(small_ds[[i]])
  mds3[i, 3] <- ratio_ex_ftr(small_ds[[i]])
  mds3[i, 4] <- mvn_skewness(small_ds[[i]])
  mds3[i, 5] <- mvn_kurtosis(small_ds[[i]])
  mds3[i, 6] <- multi_norm(small_ds[[i]])
  mds3[i, 7] <- perc_out(small_ds[[i]])
  mds3[i, 8] <- avg_pca(small_ds[[i]])
  mds3[i, 9] <- ratio_pca(small_ds[[i]])
  mds3[i, 10] <- avg_abs_cor(small_ds[[i]])
  mds3[i, 11] <- per_low_dist(small_ds[[i]])
  mds3[i, 12] <- per_high_dist(small_ds[[i]])
  mds3[i, 13] <- eigen_cent_mst(small_ds[[i]])
  mds3[i, 14] <- net_dens(small_ds[[i]])
  mds3[i, 15] <- clust_coef(small_ds[[i]])
}
write.csv(mds3, "mds3.csv")

# CSV medium 
mds4 <- data.frame()
for(i in 1:length(medium_ds)){
  mds4[i, 1] <- log_number_ex(medium_ds[[i]])
  mds4[i, 2] <- log_number_ftr(medium_ds[[i]])
  mds4[i, 3] <- ratio_ex_ftr(medium_ds[[i]])
  mds4[i, 4] <- mvn_skewness(medium_ds[[i]])
  mds4[i, 5] <- mvn_kurtosis(medium_ds[[i]])
  mds4[i, 6] <- multi_norm(medium_ds[[i]])
  mds4[i, 7] <- perc_out(medium_ds[[i]])
  mds4[i, 8] <- avg_pca(medium_ds[[i]])
  mds4[i, 9] <- ratio_pca(medium_ds[[i]])
  mds4[i, 10] <- avg_abs_cor(medium_ds[[i]])
  mds4[i, 11] <- per_low_dist(medium_ds[[i]])
  mds4[i, 12] <- per_high_dist(medium_ds[[i]])
  mds4[i, 13] <- eigen_cent_mst(medium_ds[[i]])
  mds4[i, 14] <- net_dens(medium_ds[[i]])
  mds4[i, 15] <- clust_coef(medium_ds[[i]])
}
write.csv(mds4, "mds4.csv")

# CSV large 
mds5 <- data.frame()
for(i in 1:length(large_ds)){
  mds5[i, 1] <- log_number_ex(large_ds[[i]])
  mds5[i, 2] <- log_number_ftr(large_ds[[i]])
  mds5[i, 3] <- ratio_ex_ftr(large_ds[[i]])
  mds5[i, 4] <- mvn_skewness(large_ds[[i]])
  mds5[i, 5] <- mvn_kurtosis(large_ds[[i]])
  mds5[i, 6] <- multi_norm(large_ds[[i]])
  mds5[i, 7] <- perc_out(large_ds[[i]])
  mds5[i, 8] <- avg_pca(large_ds[[i]])
  mds5[i, 9] <- ratio_pca(large_ds[[i]])
  mds5[i, 10] <- avg_abs_cor(large_ds[[i]])
  mds5[i, 11] <- per_low_dist(large_ds[[i]])
  mds5[i, 12] <- per_high_dist(large_ds[[i]])
  mds5[i, 13] <- eigen_cent_mst(large_ds[[i]])
  mds5[i, 14] <- net_dens(large_ds[[i]])
  mds5[i, 15] <- clust_coef(large_ds[[i]])
}
write.csv(mds5, "mds5.csv")

# CSV xlarge 
mds6 <- data.frame()
for(i in 1:length(xlarge_ds)){
  mds6[i, 1] <- log_number_ex(xlarge_ds[[i]])
  mds6[i, 2] <- log_number_ftr(xlarge_ds[[i]])
  mds6[i, 3] <- ratio_ex_ftr(xlarge_ds[[i]])
  mds6[i, 4] <- mvn_skewness(xlarge_ds[[i]])
  mds6[i, 5] <- mvn_kurtosis(xlarge_ds[[i]])
  mds6[i, 6] <- multi_norm(xlarge_ds[[i]])
  mds6[i, 7] <- perc_out(xlarge_ds[[i]])
  mds6[i, 8] <- avg_pca(xlarge_ds[[i]])
  mds6[i, 9] <- ratio_pca(xlarge_ds[[i]])
  mds6[i, 10] <- avg_abs_cor(xlarge_ds[[i]])
  mds6[i, 11] <- per_low_dist(xlarge_ds[[i]])
  mds6[i, 12] <- per_high_dist(xlarge_ds[[i]])
  mds6[i, 13] <- eigen_cent_mst(xlarge_ds[[i]])
  mds6[i, 14] <- net_dens(xlarge_ds[[i]])
  mds6[i, 15] <- clust_coef(xlarge_ds[[i]])
}
write.csv(mds6, "mds6.csv")











