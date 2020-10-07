
kernel_func<-function(xx, x, sigpar){
  #xx:new data
  #x:training set
  #sigpar: kernel parameter
  n_sam<-dim(xx)[1]
  xx = as.matrix(xx)
  x = as.matrix(x)
  temp<-matrix(0,nrow=nrow(x),ncol = n_sam)
  for (i in 1:nrow(x)) {
    temp[i,]<-exp(-sigpar*sqrt(diag((xx-matrix(rep(x[i,],n_sam),n_sam,byrow = T))%*%t(xx-matrix(rep(x[i,],n_sam),n_sam,byrow = T)))))
  }
  return(temp)
}