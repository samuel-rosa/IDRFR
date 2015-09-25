# Source: Xu, R. Improvements to random forest methodology. Ames, Iowa: Iowa 
# State University, p. 87, 2013.
# Dan (dnett@iastate.edu) and Dan (dnordman@iastate.edu)
# 
### bt resamples are drawn separately for every iteration.

### This function grow RFs iteratively.
### It output a ntest*niter matrix, with each column saving the prediction for 
### the test case response (or -bias) for an iteration. 

iter.RF <- 
  function (Xtrain, Ytrain, Xtest, numtree, nsize, niter) {
    # Xtrain is the design matrix for training cases, n*p; 
    # Ytrain is the response for training cases, a vector of length n;
    # Xtest is the design matrix for test cases, m*p;
    # ntree is the number of trees per forest;
    # nsize is the maximal node size per tree;
    # niter is the number of iterations for the bias-correction RFs.
    
    ni <- 0
    ntrain <- nrow(Xtrain)
    ntest <- nrow(Xtest)
    pred.iter <- NULL # This records the predicted value for all X's
    b <- Ytrain # Reponse of training data of each iteration
    
    repeat {
      ni <- ni + 1
      RF.temp <- randomForest(Xtrain, b, ntree = numtree, nodesize = nsize)
      bpred.oob <- RF.temp[[3]]
      bpred.test <- predict(RF.temp, Xtest) # Predict for test cases
      pred.iter <- rbind(pred.iter, c(bpred.oob, bpred.test))
      b <- b - bpred.oob
      if (ni >= niter) break
    } # repeat ends
    
    rownames(pred.iter) <- paste("iter-", 1:niter, sep = "")
    colnames(pred.iter) <- c(paste("Tr-", 1:ntrain, sep = ""), paste("Test-", 1:ntest, sep = ""))
    
    pred <- pred.iter[1,]		# This saves the final prediction results of different iterations of BC
    for (k in 2:niter){
      pred = rbind(pred, apply(pred.iter[1:k,], 2, sum))
    } # for k ends
    rownames(pred) <- paste("iter-", 1:niter, sep = "")
    colnames(pred) <- c(paste("Tr-", 1:ntrain, sep = ""), paste("Test-", 1:ntest, sep = ""))
    
    return(pred)
    
  } # function ends
