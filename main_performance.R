### ASSESSING THE PERFORMANCE OF CLUSTERING ALGORITHMS ###

# Loading packages --------------------------------------------------------

require(data.table)
#require(ggplot2)
#require(hrbrthemes) 
require(MixGHD) # ARI
require(EMCluster) # probabilistic model-based clustering
#require(dbscan)
require(Gmedian) # k-medians
require(kmed) # k-medoids
#require(fmsb) # radar graphs
#require(kernlab) # Spectral Clustering
require(ClusterR)
#require(xtable)


# Importing datasets ------------------------------------------------------


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

# Importing other datasets - small (77 datasets)
setwd('C:/Users/Luiz Henrique/Desktop/Luiz Henrique/Doutorado/Pesquisas de Tese/R/Clustering/Datasets/CSV/Small')
small_ds <- list()
names3 = list.files(pattern="*.csv")
for(i in 1:length(names3)){
  small_ds[[i]] <- fread(names3[i])
}

# Importing other datasets - medium (32 datasets)
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


# Treating cluster labels to start from 1 ---------------------------------

# gaussian datasets
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  if(min(gauss_ds[[i]][, n:n]) == 0){
    gauss_ds[[i]][, n:n] <- gauss_ds[[i]][, n:n] + 1
  }
}

# ellipsoidal datasets
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  if(min(ellips_ds[[i]][, n:n]) == 0){
    ellips_ds[[i]][, n:n] <- ellips_ds[[i]][, n:n] + 1
  }
}

# small datasets
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  if(min(small_ds[[i]][, n:n]) == 0){
    small_ds[[i]][, n:n] <- small_ds[[i]][, n:n] + 1
  }
}

# medium datasets
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  if(min(medium_ds[[i]][, n:n]) == 0){
    medium_ds[[i]][, n:n] <- medium_ds[[i]][, n:n] + 1
  }
}

# large datasets
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  if(min(large_ds[[i]][, n:n]) == 0){
    large_ds[[i]][, n:n] <- large_ds[[i]][, n:n] + 1
  }
}


# Generating clusters and ARI scores -----------------------------------------------------
# 1) K-MEANS -----------------------------------------------------------------

# gaussian datasets
mds1.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- kmeans(gauss_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds1.1[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmeans_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds1.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  kmeans_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds1.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- kmeans(ellips_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds1.2[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmeans_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds1.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  kmeans_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds1.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- kmeans(small_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds1.3[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmeans_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds1.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  kmeans_small[i] <- ARI(v1, v2)
}

# medium datasets
mds1.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- kmeans(medium_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds1.4[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmeans_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds1.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  kmeans_medium[i] <- ARI(v1, v2)
}

# large datasets
mds1.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- kmeans(large_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds1.5[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmeans_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds1.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  kmeans_large[i] <- ARI(v1, v2)
}

# 2) K-MEDIANS ---------------------------------------------------------------

# gaussian datasets
mds2.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- kGmedian(gauss_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds2.1[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedians_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds2.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  kmedians_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds2.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- kGmedian(ellips_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds2.2[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedians_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds2.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  kmedians_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds2.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- kGmedian(small_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds2.3[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedians_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds2.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  kmedians_small[i] <- ARI(v1, v2)
}

# medium datasets
mds2.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- kGmedian(medium_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds2.4[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedians_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds2.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  kmedians_medium[i] <- ARI(v1, v2)
}

# large datasets
mds2.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- kGmedian(large_ds[[i]][, -(n[i]:n[i])], as.numeric(k[i]))
  mds2.5[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedians_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds2.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  kmedians_large[i] <- ARI(v1, v2)
}

# 3) K-MEDOIDS ---------------------------------------------------------------
# gaussian datasets
mds3.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- fastkmed(dist(gauss_ds[[i]][, -(n[i]:n[i])]), as.numeric(k[i]))
  mds3.1[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedoids_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds3.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  kmedoids_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds3.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- fastkmed(dist(ellips_ds[[i]][, -(n[i]:n[i])]), as.numeric(k[i]))
  mds3.2[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedoids_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds3.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  kmedoids_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds3.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- fastkmed(dist(small_ds[[i]][, -(n[i]:n[i])]), as.numeric(k[i]))
  mds3.3[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedoids_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds3.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  kmedoids_small[i] <- ARI(v1, v2)
}

# medium datasets
mds3.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- fastkmed(dist(medium_ds[[i]][, -(n[i]:n[i])]), as.numeric(k[i]))
  mds3.4[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedoids_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds3.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  kmedoids_medium[i] <- ARI(v1, v2)
}

# large datasets
mds3.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- fastkmed(dist(large_ds[[i]][, -(n[i]:n[i])]), as.numeric(k[i]))
  mds3.5[[i]] <- clusters[[1]]
}

v1 <- vector()
v2 <- vector()
kmedoids_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds3.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  kmedoids_large[i] <- ARI(v1, v2)
}

# 4) SINGLE LINKAGE ----------------------------------------------------------
# gaussian datasets
mds4.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.1[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
single_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds4.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
 single_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds4.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.2[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
single_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds4.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  single_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds4.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.3[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
single_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds4.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  single_small[i] <- ARI(v1, v2)
}

# medium datasets
mds4.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.4[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
single_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds4.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  single_medium[i] <- ARI(v1, v2)
}

# large datasets
mds4.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.5[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
single_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds4.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  single_large[i] <- ARI(v1, v2)
}


# 5) COMPLETE LINKAGE --------------------------------------------------------
# gaussian datasets
mds5.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.1[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
complete_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds5.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  complete_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds5.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.2[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
complete_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds5.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  complete_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds5.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.3[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
complete_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds5.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  complete_small[i] <- ARI(v1, v2)
}

# medium datasets
mds5.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.4[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
complete_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds5.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  complete_medium[i] <- ARI(v1, v2)
}

# large datasets
mds5.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.5[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
complete_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds5.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  complete_large[i] <- ARI(v1, v2)
}


# 6) AVERAGE LINKAGE ---------------------------------------------------------
# gaussian datasets
mds6.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.1[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
average_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds6.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  average_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds6.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.2[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
average_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds6.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  average_ellipsoidal[i] <- ARI(v1, v2)
}


# small datasets
mds6.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.3[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
average_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds6.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  average_small[i] <- ARI(v1, v2)
}

# medium datasets
mds6.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.4[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
average_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds6.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  average_medium[i] <- ARI(v1, v2)
}

# large datasets
mds6.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.5[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
average_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds6.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  average_large[i] <- ARI(v1, v2)
}

# 7) WARD'S METHOD -----------------------------------------------------------
# gaussian datasets
mds7.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.1[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
wards_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds7.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  wards_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds7.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.2[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
wards_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds7.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  wards_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds7.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.3[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
wards_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds7.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  wards_small[i] <- ARI(v1, v2)
}

# medium datasets
mds7.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.4[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
wards_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds7.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  wards_medium[i] <- ARI(v1, v2)
}

# large datasets
mds7.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.5[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
wards_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds7.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  wards_large[i] <- ARI(v1, v2)
}

# 8) CLOSEST CENTROID --------------------------------------------------------
# gaussian datasets
mds8.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.1[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
closest_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds8.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  closest_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds8.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.2[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
closest_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds8.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  closest_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds8.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.3[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
closest_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds8.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  closest_small[i] <- ARI(v1, v2)
}

# medium datasets
mds8.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.4[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
closest_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds8.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  closest_medium[i] <- ARI(v1, v2)
}

# large datasets
mds8.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.5[[i]] <- as.matrix(cutree(clusters, as.numeric(k[i])))
}

v1 <- vector()
v2 <- vector()
closest_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds8.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  closest_large[i] <- ARI(v1, v2)
}

# 9) EM ALGORITHM ---------------------------------------------------------
# gaussian datasets
mds9.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  base <- gauss_ds[[i]][, -(n[i]:n[i])]
  clusters1 <- simple.init(base, nclass = k[i]) # random initialization
  clusters2 <- emcluster(base, clusters1, assign.class = TRUE)
  mds9.1[[i]] <- clusters2[[11]]
}

v1 <- vector()
v2 <- vector()
em_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds9.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  em_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds9.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  base <- ellips_ds[[i]][, -(n[i]:n[i])]
  clusters1 <- simple.init(base, nclass = k[i]) # random initialization
  clusters2 <- emcluster(base, clusters1, assign.class = TRUE)
  mds9.2[[i]] <- clusters2[[11]]
}

v1 <- vector()
v2 <- vector()
em_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds9.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  em_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds9.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  base <- small_ds[[i]][, -(n[i]:n[i])]
  clusters1 <- simple.init(base, nclass = k[i]) # random initialization
  clusters2 <- emcluster(base, clusters1, assign.class = TRUE)
  mds9.3[[i]] <- clusters2[[11]]
}

v1 <- vector()
v2 <- vector()
em_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds9.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  em_small[i] <- ARI(v1, v2)
}

# medium datasets
mds9.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  base <- medium_ds[[i]][, -(n[i]:n[i])]
  clusters1 <- simple.init(base, nclass = k[i]) # random initialization
  clusters2 <- emcluster(base, clusters1, assign.class = TRUE)
  mds9.4[[i]] <- clusters2[[11]]
}

v1 <- vector()
v2 <- vector()
em_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds9.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  em_medium[i] <- ARI(v1, v2)
}

# large datasets
mds9.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  base <- large_ds[[i]][, -(n[i]:n[i])]
  clusters1 <- simple.init(base, nclass = k[i]) # random initialization
  clusters2 <- emcluster(base, clusters1, assign.class = TRUE)
  mds9.5[[i]] <- clusters2[[11]]
}

v1 <- vector()
v2 <- vector()
em_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds9.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  em_large[i] <- ARI(v1, v2)
}

# 10) MINI-BATCH K-MEANS --------------------------------------------------
# gaussian datasets
mds10.1 <- list()
n <- vector()
k <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  k[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
  clusters <- MiniBatchKmeans(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters = k[i])
  mds10.1[[i]] <- predict_MBatchKMeans(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
}

v1 <- vector()
v2 <- vector()
mbkmeans_gaussian <- vector()
for(i in 1:length(gauss_ds)){
  n <- dim(gauss_ds[[i]])[2]
  v1 <- as.vector(mds10.1[[i]])
  v2 <- as.vector(t(gauss_ds[[i]][, n:n]))
  mbkmeans_gaussian[i] <- ARI(v1, v2)
}

# ellipsoidal datasets
mds10.2 <- list()
n <- vector()
k <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  k[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
  clusters <- MiniBatchKmeans(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters = k[i])
  mds10.2[[i]] <- predict_MBatchKMeans(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
}

v1 <- vector()
v2 <- vector()
mbkmeans_ellipsoidal <- vector()
for(i in 1:length(ellips_ds)){
  n <- dim(ellips_ds[[i]])[2]
  v1 <- as.vector(mds10.2[[i]])
  v2 <- as.vector(t(ellips_ds[[i]][, n:n]))
  mbkmeans_ellipsoidal[i] <- ARI(v1, v2)
}

# small datasets
mds10.3 <- list()
n <- vector()
k <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  k[i] <- max(small_ds[[i]][, n[i]:n[i]])
  clusters <- MiniBatchKmeans(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters = k[i])
  mds10.3[[i]] <- predict_MBatchKMeans(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
}

v1 <- vector()
v2 <- vector()
mbkmeans_small <- vector()
for(i in 1:length(small_ds)){
  n <- dim(small_ds[[i]])[2]
  v1 <- as.vector(mds10.3[[i]])
  v2 <- as.vector(t(small_ds[[i]][, n:n]))
  mbkmeans_small[i] <- ARI(v1, v2)
}

# medium datasets
mds10.4 <- list()
n <- vector()
k <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  k[i] <- max(medium_ds[[i]][, n[i]:n[i]])
  clusters <- MiniBatchKmeans(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters = k[i])
  mds10.4[[i]] <- predict_MBatchKMeans(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
}

v1 <- vector()
v2 <- vector()
mbkmeans_medium <- vector()
for(i in 1:length(medium_ds)){
  n <- dim(medium_ds[[i]])[2]
  v1 <- as.vector(mds10.4[[i]])
  v2 <- as.vector(t(medium_ds[[i]][, n:n]))
  mbkmeans_medium[i] <- ARI(v1, v2)
}

# large datasets
mds10.5 <- list()
n <- vector()
k <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  k[i] <- max(large_ds[[i]][, n[i]:n[i]])
  clusters <- MiniBatchKmeans(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters = k[i])
  mds10.5[[i]] <- predict_MBatchKMeans(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
}

v1 <- vector()
v2 <- vector()
mbkmeans_large <- vector()
for(i in 1:length(large_ds)){
  n <- dim(large_ds[[i]])[2]
  v1 <- as.vector(mds10.5[[i]])
  v2 <- as.vector(t(large_ds[[i]][, n:n]))
  mbkmeans_large[i] <- ARI(v1, v2)
}


# Generating Metadata -----------------------------------------------------

kmeans_ari <- rbind(kmeans_gaussian, kmeans_ellipsoidal, kmeans_small, kmeans_medium, kmeans_large)
kmedians_ari <- rbind(kmedians_gaussian, kmedians_ellipsoidal, kmedians_small, kmedians_medium, kmedians_large)
kmedoids_ari <- rbind(kmedoids_gaussian, kmedoids_ellipsoidal, kmedoids_small, kmedoids_medium, kmedoids_large)
single_ari <- rbind(single_gaussian, single_ellipsoidal, single_small, single_medium, single_large)
complete_ari <- rbind(complete_gaussian, complete_ellipsoidal, complete_small, complete_medium, complete_large)
average_ari <- rbind(average_gaussian, average_ellipsoidal, average_small, average_medium, average_large)
wards_ari <- rbind(wards_gaussian, wards_ellipsoidal, wards_small, wards_medium, wards_large)
closest_ari <- rbind(closest_gaussian, closest_ellipsoidal, closest_small, closest_medium, closest_large)
em_ari <- rbind(em_gaussian, em_ellipsoidal, em_small, em_medium, em_large)
mbkmeans_ari <- rbind(mbkmeans_gaussian, mbkmeans_ellipsoidal, mbkmeans_small, mbkmeans_medium, mbkmeans_large)

ari <- cbind(kmeans_ari, kmedians_ari, kmedoids_ari, single_ari, complete_ari, average_ari, wards_ari, closest_ari, em_ari, mbkmeans_ari)
write.csv(ari, "ari.csv")





