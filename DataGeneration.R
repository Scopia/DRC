
DataGeneration <- function(K, n, dimension, dist, paras, RandMat = list(dist = rnorm, mean = 0, sd = 30)){
  #K: nums of categories
  #n: vector, sample size of each class
  #dimension: nums of variables
  #RandMat: Specifies the distribution that generates the random covariance matrix.
  #dist: vector of functions for specifing distribution of each feature, length of K.
  #paras: A list contains necessary parameters corresponding to each element of dist. 
  
  sigma0<-list()
  for (i in 1:K) {
    sigma0[[i]] = matrix(RandMat$dist(dimension*dimension,unlist(RandMat[-1])),ncol = dimension,nrow = dimension)
  }
  x <- list()
  y <- list()
  for (k in 1:K){
    xx = list()
    for (d in 1:dimension) {
      xx[[d]] = dist[[d]](n[k], paras[[d]])
    }
    x[[k]] = sqrt(n[k]-1)*qr.Q(qr(Reduce(cbind,xx)))%*%sqrt(diag(svd(sigma0[[k]])$d))%*%t(svd(sigma0[[k]])$v)
    y[[k]] = rep(k, n[k])
  }
  
  X <- Reduce(rbind,x)
  X <- scale(X)
  colnames(X) <- NULL
  Y <- as.factor(unlist(y))
  Data <- as.data.frame(X)
  Data[, dimension+1] <- Y
  names(Data)[dimension+1] <- "y_p"
  return(Data)
}
