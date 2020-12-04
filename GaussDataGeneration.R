
GaussDataGeneration <- function(K, n, dimension, mix_num = 10, cmean = 0.5, cvar = 1, V = 5){
  #K: nums of categories
  #n: vector, sample size of each class
  #dimension: nums of variables
  #mix_num: nums of Gaussian Clusters, must be greater than 1.
  #cmean: parameter controling the distance between classes, the bigger the cmean, the larger the distance.
  #cvar: parameter controling intra classes distance
  #V: parameter controling the relative size of variance of features
  sd0  <- list()
  sigma0 <- list()
  for (i in 1:mix_num) {
    sd0[[i]] = matrix(rnorm(dimension*dimension,cmean,V), ncol = dimension, nrow = dimension)
    sigma0[[i]] = svd(sd0[[i]])$v%*%diag(svd(sd0[[i]])$d)%*%t(svd(sd0[[i]])$v)
  }
  centroids <- list()
  x <- list()
  y <- list()
  for (k in 1:K){
    mu0 = numeric(dimension)
    if(k <= dimension){
      mu0[k] = cmean
    }
    else{
      mu0[1] = k*cmean
    }
    centroids[[k]] = mvrnorm(mix_num, mu = mu0, Sigma = cvar*diag(dimension))
    x[[k]] = t(sapply(1:n[k], function(i){mn = sample(1:mix_num, 1);return(mvrnorm(1, mu=c(centroids[[k]][mn, ]),Sigma = sigma0[[mn]]))}))
    y[[k]] = rep(k, n[k])
  }
  Y <- as.factor(unlist(y))
  X <- Reduce(rbind,x)
  X <- scale(X)
  colnames(X) <- NULL
  Data <- as.data.frame(X)
  Data[, dimension+1] <- Y
  names(Data)[dimension+1] <- "y_p"
  return(Data)
}