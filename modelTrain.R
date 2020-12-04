
modelTrain<-function(x_p, y_p, K, sigpar, lambda, iterations){
  #x_p: feature set for training
  #y_p: label,length same as x_p
  #K: nums of categories
  #sigpar: kernel parameter
  #lambda: parameter of penalized term
  #iterations:  max steps of iteration
  
  x<-x_p
  y<-as.factor(y_p)
  rho<-c(table(y)/sum(table(y)))
  Gram<-kernel_func(x, x, sigpar)+diag(rep(1e-6,dim(x)[1]))
  q<-dim(Gram)[1]
  y_matrix<-sapply(1:(K-1), function(j){sapply(1:length(y),function(i){(y[i]==j)})})
  K_0 <- as.matrix(bdiag(lapply(1:(K-1), function(i){Gram})))
  alpha0<-rep(0,(K-1)*q)
  alpha1<-rep(0.01,(K-1)*q)
  iter_num<-0
  B_0<-1/2*kronecker(diag(K-1)-rep(1,K-1)%*%t(rep(1,K-1))/K,Reduce('+',lapply(1:length(y), function(i){Gram[,i]%*%t(Gram[,i])})))
  while (c(sqrt((alpha1-alpha0)%*%(alpha1-alpha0)))/max(sqrt((alpha1)%*%(alpha1)),1)>1e-4 & iter_num <= iterations) {
    alpha0<-alpha1
    pr<-t(sapply(1:length(y),function(i){sapply(1:(K-1),function(j){rho[j]*exp(alpha0[((j-1)*q+1):(j*q)]%*%Gram[,i])/(rho[K]+sum(sapply(1:(K-1),function(l){rho[l]*exp(alpha0[((l-1)*q+1):(l*q)]%*%Gram[,i])})))})}))
    G<-K_0%*%(-c(y_matrix)+c(pr)+1/lambda*alpha0)
    alpha1<-c(alpha0-solve(B_0+1/lambda*K_0)%*%G)
    iter_num=iter_num+1
    cat("iteration:",iter_num,'\n')
  }
  
  discriminantFunc = matrix(0, nrow = nrow(x_p) , ncol = K-1)
  for (i in 1:(K-1)){
    discriminantFunc[,i] = c(alpha1[((i-1)*q+1):(i*q)]%*%Gram)
    
  }
  
  p_i = 1/sum(n)/(rho[K]+ discriminantFunc%*%rho[1:(K-1)])
  threshold = numeric(K-1)
  for (i in 1:(K-1)) {
    com_chr12 = discriminantFunc[,i]
    threshold[i] = com_chr12[order(com_chr12)[which.max(cumsum(p_i*(1-exp(com_chr12))[order(com_chr12)]))]]
  }
  
  model = list()
  model$alpha = alpha1
  model$sigpar = sigpar
  model$lambda = lambda
  model$x = x_p
  model$y = y_p
  model$K = K
  model$iteration = iter_num
  model$p_i = p_i
  model$discriminantFunc = discriminantFunc
  model$threshold = threshold
  return(model)
}