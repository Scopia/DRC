library(caret)
library(MASS)
library(Matrix)
library(pROC)
library(Rtsne)
library(ggplot2)

setwd("~/Downloads/paper/DRCCode")
source('GaussDataGeneration.R')
source('dataPartition.R')
source('kernel_func.R')
source('measureCal.R')
source('modelTrain.R')
source('predictDRC.R')
source('visulization.R')

K = 2
n = c(50, 150)
dimension = 3
n_f = 10
sigpar = 0.21
lambda = 4.01

data = GaussDataGeneration(K, n, dimension = dimension, mix_num = 2)
#data = DataGeneration(K, n, dimension, c(rt,rt,rt), list(rt = 2, rt1 = 3 , rt2 = 4))
X = data[,1:dimension]
Y = data[,dimension+1]
visulization(data)

cvlist <- dataPartition(X, Y, n_f = n_f)
x_p = X[-cvlist[[n_f]],]
y_p = as.factor(Y[-cvlist[[n_f]]])
x_t = X[cvlist[[n_f]],]
y_t = as.factor(Y[cvlist[[n_f]]])

model <- modelTrain(x_p, y_p, K, sigpar, lambda, 1000)
pred <- predictDRC(model, x_t, y_t)
result <- measureCal(pred)

AucRoc <- result$auc
measurebyClass <- result$confusion$byClass
measureOverall <- result$confusion$overall
