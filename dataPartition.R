
dataPartition <- function(X, Y, n_f = 5){
  #X: original features of datasets
  #Y: original label of datasets
  #n_f: nums of fold. By default, the dataset is divided into 5 parts
  n_total = c(table(Y))
  CVgroup <- function(k,datasize){
    cvlist <- list()
    n <- rep(1:k,ceiling(datasize/k))[1:datasize]    #将数据分成K份
    temp <- sample(n,datasize)   #把n打乱
    x <- 1:k
    dataseq <- 1:datasize
    cvlist <- lapply(x,function(x) dataseq[temp==x])
    return(cvlist)
  }
  cvlist <- CVgroup(k = n_f,datasize = sum(n_total))
  return(cvlist)
}
