
measureCal<-function(result){
  #result: object of predictDRC
  
  measures = list()
  
  score = result$score
  ytrue = result$ytrue
  ypred = result$ypred
  prob = result$probability
  if (K>2){auc_p <- multiclass.roc(ytrue, prob)$auc}
  else{auc_p <- auc(ytrue, score$V1)}

  confusion <- confusionMatrix(ytrue, ypred)
  
  measures$confusion = confusion
  measures$auc = auc_p
  return(measures)
  
}