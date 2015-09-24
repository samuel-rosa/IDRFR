# Source: Xu, R. Improvements to random forest methodology. Ames, Iowa: Iowa 
# State University, p. 87, 2013.
# 
### Here we tried FOUR kinds of BC methods compared to No-BC to real datasets.

rm(list = ls())
library(randomForest)
source("code/R/@iterative-RF-Fast.R")

dat <- read.csv("data/servo.data", header = F )

nsample <- nrow(dat)
X <- NULL
for (i in 1:(ncol(dat) - 1)) X <- cbind(X, factor(dat[, i]))
Y <- dat[, ncol(dat)]

nround <- 1000 # Number of partitioning data to training and test
numtree <- 500
ntrain <- round(2 * nsample / 3)
ntest <- nsample - ntrain
nz <- 5
niter <- 10 # Number of iterations

mse <- NULL

k <- 0
repeat {
  k <- k + 1
  id.train <- sample(1:nsample, ntrain, replace = F)
  id.test <- c(1:nsample)[-id.train]
  
  res <- iter.RF(X[id.train, ], Y[id.train], X[id.test,], numtree, nsize = nz, niter)
  res <- res[, -c(1:ntrain)] # prediction for test cases of this holding-out
  
  mse <- rbind(mse, apply(((t(res) - Y[id.test])^2), 2, mean))
  
  if (k %% 100 == 0) print(k)
  if (k == nround) break
  
} # repeat ends

colnames(mse) <- paste("iter-", 1:niter, sep = "")
mse <- apply(mse, 2, mean)
plot(1:niter, mse)
