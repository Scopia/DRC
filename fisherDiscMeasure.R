fisherDiscMeasure <- function(data, K, p){
  #data: all data
  #K: number of classes
  #p: dimension of features
  #This is a function used for calculating degree of overlapping between classes.
  x = list()
  for (k in 1:K) {
    x[[k]] = data[data$y_p==k,-1]
  }
  fdm = numeric(K^2)
  i = 1
  for (k in 1:K) {
    for (m in (k+1):K) {
      fdm[i] = max(sapply(1:p,function(i){(mean(x[[m]][,i])-mean(x[[k]][,i]))^2/(var(x[[m]][,i])+var(x[[k]][,i]))}))
      i = i+1
    }
  }
  return(min(fdm[fdm!=0]))
}
