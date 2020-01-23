### META-FEATURES FUNCTIONS FOR CLUSTERING PROBLEMS ###

#require(MVN)
#require(varhandle) # factor to numeric
#require(cluster)
#require(igraph)

#data("iris")
#ds <- iris[, 1:4]


## functions ##
#1 log10 number of examples
log_number_ex <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  return(round(log(dim(ds1)[1], 10), 4))
}

#2 log10 number of features
log_number_ftr <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  return(round(log(dim(ds1)[2], 10), 4))
}

#3 ratio of the number of examples to features
ratio_ex_ftr <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  return(round(dim(ds1)[1]/dim(ds1)[2], 4))
}

#4 multivariate normality skewness (MVN package)
mvn_skewness <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  result <- MVN::mvn(data = ds1, mvnTest = "mardia")
  return(round(varhandle::unfactor(result[[1]][1, 2]), 4))
} 

#5 multivariate normality kurtosis (MVN package)
mvn_kurtosis <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  result <- MVN::mvn(data = ds1, mvnTest = "mardia")
  return(round(varhandle::unfactor(result[[1]][2, 2]), 4))
} 

#6 multivariate normality (MVN package)
multi_norm <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  result <- MVN::mvn(data = ds1, mvnTest = "hz", multivariatePlot = "none") 
  return(round(result[[1]][1, 2], 4))
} 

#7 percentage of outliers (distance based) 
perc_out <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dist <- as.vector(dist(ds1))
  results <- summary(dist)
  q1 <- as.numeric(results[2])
  q3 <- as.numeric(results[5])
  d <- q3 - q1
  outliers <- dist[(dist < (q1 - 1.5*d))|(dist > (q3 + 1.5*d))]
  return(round(length(outliers)/length(dist), 4))
} 

#8 average number of points per PCA dimension
avg_pca <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  ds.pca <- summary(prcomp(ds1, center = TRUE, scale. = TRUE))
  results <- ds.pca[[6]]
  for (i in 1:dim(results)[2]){
    if (results[3, i] >= 0.95){
      break
    }
  }
  return(round(dim(ds1)[1]/i, 4))
}  

#9 ratio of the PCA to the original dimension
ratio_pca <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  ds.pca <- summary(prcomp(ds1, center = TRUE, scale. = TRUE))
  results <- ds.pca[[6]]
  for (i in 1:dim(results)[2]){
    if (results[3, i] >= 0.95){
      break
    }
  }
  return(round(i/dim(ds1)[2], 4))
} 

#10 average absolute correlation
avg_abs_cor <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  return(round(mean(abs(cor(ds1))), 4))
}

#11 percentage of points of low distance  
per_low_dist <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dist <- as.vector(dist(ds1))
  low <- dist[dist < (mean(dist) - sd(dist))]
  return(round(length(low)/length(dist), 4))  
}

#12 percentage of points of high distance  
per_high_dist <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dist <- as.vector(dist(ds1))
  high <- dist[dist > (mean(dist) + sd(dist))]
  return(round(length(high)/length(dist), 4))  
}

#13 eigenvalue centrality of minimum spanning tree 
eigen_cent_mst <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  graph <- igraph::as.undirected(igraph::graph.adjacency(as.matrix(dist(ds1)), weighted=TRUE))
  mst <- igraph::as.undirected(igraph::mst(graph))
  ec <- igraph::eigen_centrality(mst)
  return(round(ec$value, 4))
}

# epsilon-NN function
enn <- function(ds, e) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- as.matrix(dist(ds1))
  for(i in 1:nrow(ds1)) {
    a <- names(sort(dst[i,])[1:(e+1)])
    b <- rownames(ds1)
    dst[i, setdiff(rownames(ds1), intersect(a, b))] <- 0
  }
  return(dst)
}

#14 network density 
net_dens <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- enn(ds1, 0.15*nrow(ds1))
  graph <- igraph::graph.adjacency(dst, mode="undirected", weighted=TRUE)
  density <- igraph::graph.density(graph)
  return(round(density, 4))
}

#15 clustering coefficient 
clust_coef <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- enn(ds1, 0.15*nrow(ds1))
  graph <- igraph::graph.adjacency(dst, mode="undirected", weighted=TRUE)
  clust_coef <- igraph::transitivity(graph, type="global", isolates="zero")
  return(round(clust_coef, 4))
}


## testing functions... ##
#log_number_ex(ds)
#log_number_ftr(ds)
#ratio_ex_ftr(ds)
#mvn_skewness(ds)
#mvn_kurtosis(ds)
#multi_norm(ds)
#perc_out(ds)
#avg_pca(ds)
#ratio_pca(ds)
#avg_abs_cor(ds)
#per_low_dist(ds)
#per_high_dist(ds)
#eigen_cent_mst(ds)
#net_dens(ds)
#clust_coef(ds)


