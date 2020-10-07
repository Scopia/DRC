
predictDRC<-function(model, x_t, y_t, threshold = 0){
  #model: model object
  #x_t: features of test data
  #y_t: label of test data, lenth same as x_t
  #threshold: confidence level for classify
  result = list()
  K = model$K
  alpha1 = model$alpha
  x = as.matrix(model$x)
  x_t = as.matrix(x_t)
  y = factor(model$y, levels = c(1, 2))
  y_t = factor(y_t, levels = c(1, 2))
  sigpar = model$sigpar
  K = model$K
  Gram = kernel_func(x_t, x, sigpar)
  q = nrow(Gram)
  discriminantFunc = matrix(0, nrow = nrow(x_t) , ncol = K-1)
  for (i in 1:(K-1)){
    discriminantFunc[,i] = c(alpha1[((i-1)*q+1):(i*q)]%*%Gram)
  }
  discriminantFunc<-as.data.frame(discriminantFunc)
  
  rho<-c(table(y_t)/sum(table(y_t)))
  prob<-sapply(1:(K-1), function(k){rho[k]*exp(discriminantFunc[,k])/(rowSums(sapply(1:(K-1),function(j){rho[j]*exp(discriminantFunc[,j])}))+rep(rho[K],dim(discriminantFunc)[1]))})
  prob<-as.data.frame(prob)
  prob[,K]<-1/(rowSums(sapply(1:(K-1),function(j){rho[j]/rho[K]*exp(discriminantFunc[,j])}))+rep(1,dim(discriminantFunc)[1]))
  colnames(prob)<-levels(y_t)
  
  categories<-numeric(dim(discriminantFunc)[1])
  categories[rowSums(discriminantFunc < threshold) == K-1 ]<- K
  categories[categories==0]<-(apply(discriminantFunc,1,which.max))[categories==0]
  categories <- factor(categories,levels=c(1:K))
  
  result$score = discriminantFunc
  result$probability = prob
  result$ytrue = y_t
  result$ypred = categories
  result$K = K
  return(result)
}