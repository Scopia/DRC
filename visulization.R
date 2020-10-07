visulization <- function(data_p){
  d = Rtsne(data_p)
  dd = data.frame(d$Y)
  label = as.factor(data_p$y_p)
  p.scatter <- ggplot(dd) + geom_point(aes(x=X1, y=X2, color=label, shape = label))
  p.scatter
}
