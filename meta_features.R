### META-FEATURES FUNCTIONS FOR CLUSTERING PROBLEMS ###

#require(MVN)
#require(varhandle) # factor to numeric
#require(cluster)
#require(igraph)

data("iris")
ds <- iris[, 1:4]


## functions ##
#1 log10 number of examples
log_number_ex <- function(ds){
  return(round(log(dim(ds)[1], 10), 4))
}

#2 log10 number of features
log_number_ftr <- function(ds){
  return(round(log(dim(ds)[2], 10), 4))
}

#3 ratio of the number of examples to features
ratio_ex_ftr <- function(ds){
  return(round(dim(ds)[1]/dim(ds)[2], 4))
}

#4 multivariate normality skewness (MVN package)
mvn_skewness <- function(ds){
  result <- MVN::mvn(data = ds, mvnTest = "mardia")
  return(round(varhandle::unfactor(result[[1]][1, 2]), 4))
} 

#5 multivariate normality kurtosis (MVN package)
mvn_kurtosis <- function(ds){
  result <- MVN::mvn(data = ds, mvnTest = "mardia")
  return(round(varhandle::unfactor(result[[1]][2, 2]), 4))
} 

#6 multivariate normality (MVN package)
multi_norm <- function(ds){
  result <- MVN::mvn(data = ds, mvnTest = "hz", multivariatePlot = "none") # Henze-Zirkler's MVN test 
  return(round(result[[1]][1, 2], 4))
} 

#7 percentage of outliers (MVN package), Mahalanobis distance method
perc_out <- function(ds){
  outliers <- MVN::mvn(data = ds, mvnTest = "hz", multivariatePlot = 'none', multivariateOutlierMethod = "adj", showOutliers = TRUE)
  return(round(dim(outliers[[4]])[1]/dim(ds)[1], 4))
} 

#8 average number of points per PCA dimension
avg_pca <- function(ds){
  ds.pca <- summary(prcomp(ds, center = TRUE, scale. = TRUE))
  results <- ds.pca[[6]]
  for (i in 1:dim(results)[2]){
    if (results[3, i] >= 0.95){
      break
    }
  }
  return(round(dim(ds)[1]/i, 4))
}  

#9 ratio of the PCA to the original dimension
ratio_pca <- function(ds){
  ds.pca <- summary(prcomp(ds, center = TRUE, scale. = TRUE))
  results <- ds.pca[[6]]
  for (i in 1:dim(results)[2]){
    if (results[3, i] >= 0.95){
      break
    }
  }
  return(round(i/dim(ds)[2], 4))
} 

#10 average absolute correlation
avg_abs_cor <- function(ds){
  return(round(mean(abs(cor(ds))), 4))
}

#11 percentage of points of low distance  
per_low_dist <- function(ds){
  dist <- as.vector(dist(ds))
  low <- dist[dist < (mean(dist) - sd(dist))]
  return(round(length(low)/length(dist), 4))  
}

#12 percentage of points of high distance  
per_high_dist <- function(ds){
  dist <- as.vector(dist(ds))
  high <- dist[dist > (mean(dist) + sd(dist))]
  return(round(length(high)/length(dist), 4))  
}

#13 eigenvalue centrality of minimum spanning tree 
eigen_cent_mst <- function(ds){
  graph <- igraph::as.undirected(igraph::graph.adjacency(as.matrix(dist(ds)), weighted=TRUE))
  mst <- igraph::as.undirected(igraph::mst(graph))
  ec <- igraph::eigen_centrality(mst)
  return(round(ec$value, 4))
}

# epsilon-NN function
enn <- function(ds, e) {
  dst <- as.matrix(dist(ds))
  for(i in 1:nrow(ds)) {
    a <- names(sort(dst[i,])[1:(e+1)])
    b <- rownames(ds)
    dst[i, setdiff(rownames(ds), intersect(a, b))] <- 0
  }
  return(dst)
}

#14 network density 
net_dens <- function(ds) {
  dst <- enn(ds, 0.15*nrow(ds))
  graph <- igraph::graph.adjacency(dst, mode="undirected", weighted=TRUE)
  density <- igraph::graph.density(graph)
  return(round(density, 4))
}

#15 clustering coefficient 
clust_coef <- function(ds) {
  dst <- enn(ds, 0.15*nrow(ds))
  graph <- igraph::graph.adjacency(dst, mode="undirected", weighted=TRUE)
  clust_coef <- igraph::transitivity(graph, type="global", isolates="zero")
  return(round(clust_coef, 4))
}


## testing functions... ##
log_number_ex(ds)
log_number_ftr(ds)
ratio_ex_ftr(ds)
mvn_skewness(ds)
mvn_kurtosis(ds)
multi_norm(ds)
perc_out(ds)
avg_pca(ds)
ratio_pca(ds)
avg_abs_cor(ds)
per_low_dist(ds)
per_high_dist(ds)
eigen_cent_mst(ds)
net_dens(ds)
clust_coef(ds)

