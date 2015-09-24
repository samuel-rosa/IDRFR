rm(list = ls())
# Optimum number of iterations for debiasing a random forest regression ########
optimRandomForest <-
  function (x, y, niter = 10, nruns = 100, ntree = 500, ntrain = 2/3, 
            nodesize = 5, mtry = max(floor(ncol(x) / 3), 1), profile = TRUE,
            progress = TRUE) {
    
    # Check if suggested packages are installed
    pkg <- c("randomForest", "utils")
    id <- !sapply(pkg, requireNamespace, quietly = TRUE)
    if (any(id)) {
      pkg <- paste(pkg[which(id)], collapse = " ")
      stop(paste("Package(s) needed for this function to work but not",
                 "installed: ", pkg, sep = ""), call. = FALSE)
    }
    
    # Settings
    nsample <- length(y)
    if (ntrain < 1) ntrain <- round(nsample * ntrain)
    ntest <- nsample - ntrain
    
    # Start simulation
    if (progress) {
      pb <- utils::txtProgressBar(min = 1, max = niter, style = 3) 
    }
    mse <- NULL
    # k <- 0
    for (k in 1:nruns) {
    # repeat {
      # k <- k + 1
      id.train <- sample(1:nsample, ntrain, replace = FALSE)
      id.test <- c(1:nsample)[-id.train]
      
      res <- .iterRandomForest(xtrain = x[id.train, ], ytrain = y[id.train],
                               xtest = x[id.test, ], ntree = ntree, mtry = mtry,
                               nodesize = nodesize, niter = niter)
      
      # prediction for test cases of this holding-out
      res <- res[, -c(1:ntrain)]
      
      mse <- rbind(mse, apply(((t(res) - y[id.test]) ^ 2), 2, mean))
      
      # if (k == nruns) break
      if (progress) utils::setTxtProgressBar(pb, k)
    }
    if (progress) close(pb)
    
    # Prepare output
    colnames(mse) <- paste("iter-", 1:niter, sep = "")
    res <- list(mse = data.frame(mean = apply(mse, 2, mean), 
                                 sd = apply(mse, 2, sd)),
                call = data.frame(nruns = nruns, ntree = ntree, 
                                  ntrain = ntrain, ntest = ntest, 
                                  nodesize = nodesize, mtry = mtry))
    
    # Plot mse profile
    if (profile) {
      .profRandomForest(mse = mse, nruns = nruns, niter = niter)
    }
    
    # Output
    return (res)
  }

# Plot MSE profile #############################################################
.profRandomForest <-
  function (mse, nruns, niter) {
    
    # Prepare data for plotting
    eb <- apply(mse, 2, sd) / sqrt(nruns)
    mse <- apply(mse, 2, mean)
    upper <- (mse + eb) / mse[1]
    lower <- (mse - eb) / mse[1]
    mse <- mse / mse[1]
    
    # Plotting
    plot(1:niter, mse, type = "b", ylim = c(min(lower), max(upper)),
         ylab = "Standardized mean squared error", xlab = "Iteration",
         main = "Mean squared error profile")
    abline(h = mse[1], col = "red")
    arrows(1:niter, lower, 1:niter, upper, length = 0.05, angle = 90, code = 3)
    points(x = which.min(mse), y = min(mse), col = "blue", lwd = 2)
  }
# Iterative calibration of random forest regressions ###########################
.iterRandomForest <- 
  function (xtrain, ytrain, xtest, ntree, nodesize, mtry, niter) {
    # xtrain is the design matrix for training cases, n*p; 
    # ytrain is the response for training cases, a vector of length n;
    # xtest is the design matrix for test cases, m*p;
    # ntree is the number of trees per forest;
    # nodesize is the maximal node size per tree;
    # niter is the number of iterations for the bias-correction RFs.
    
    # Initial settings
    ni <- 0
    ntrain <- nrow(xtrain)
    ntest <- nrow(xtest)
    pred.iter <- NULL # This records the predicted value for all X's
    b <- ytrain # Reponse of training data of each iteration
    
    repeat {
      ni <- ni + 1
      RF.temp <- randomForest(x = xtrain, y = b, ntree = ntree, mtry = mtry,
                              nodesize = nodesize)
      bpred.oob <- RF.temp$predicted
      
      # Predict for test cases
      bpred.test <- as.numeric(predict(object = RF.temp, newdata = xtest))
      pred.iter <- rbind(pred.iter, c(bpred.oob, bpred.test))
      b <- b - bpred.oob
      if (ni >= niter) break
    } # repeat ends
    
    rownames(pred.iter) <- paste("iter-", 1:niter, sep = "")
    colnames(pred.iter) <- c(paste("Tr-", 1:ntrain, sep = ""), 
                             paste("Test-", 1:ntest, sep = ""))
    
    # This saves the final prediction results of different iterations of BC
    pred <- pred.iter[1,]
    for (k in 2:niter) {
      pred <- rbind(pred, apply(pred.iter[1:k,], 2, sum))
    } # for k ends
    
    rownames(pred) <- paste("iter-", 1:niter, sep = "")
    colnames(pred) <- c(paste("Tr-", 1:ntrain, sep = ""), 
                        paste("Test-", 1:ntest, sep = ""))
    
    return(pred)
    
  } # function ends

# Fit a debiased random forest regression model ################################
fitRandomForest <- 
  function (x, y, ntree = 500, mtry = max(floor(ncol(x) / 3), 1), nodesize = 5,
            niter = 2, slr = FALSE) {
    
    # Check if suggested packages are installed
    pkg <- c("randomForest")
    id <- !sapply(pkg, requireNamespace, quietly = TRUE)
    if (any(id)) {
      pkg <- paste(pkg[which(id)], collapse = " ")
      stop(paste("Package(s) needed for this function to work but not",
                 "installed: ", pkg, sep = ""), call. = FALSE)
    }
    
    # Fit random forest models
    rf <- list()
    for (i in 1:niter) {
      rf[[i]] <- randomForest(y = y, x = x, ntree = ntree, mtry = mtry, 
                            nodesize = nodesize)
      y <- y - rf[[i]]$predicted
    }
#     rf_p <- randomForest(y = y, x = x, ntree = ntree, mtry = mtry, 
#                          nodesize = nodesize)
#     rf_r <- rf_p$predicted - y
#     rf_r <- randomForest(y = rf_r, x = x, ntree = ntree, mtry = mtry, 
#                          nodesize = nodesize)
    
    # Fit a simple linear regression
    if (slr) {
      x <- sapply(1:length(rf), function (i) rf[[i]]$predicted)
      x <- apply(x, 1, sum)
      # x <- rf_p$predicted - rf_r$predicted
      rf[[niter + 1]] <- stats::lm(y ~ x)
    }
    
    # Output
    # A list with the random forest regression (and simple linear regression) 
    # models in the order in which they have been fitted.
    return (rf)
  }
#' Predict using an iteratively debiased random forest regression ##############
#' 
#' Prediction using iteratively debiased random forest regression (IDRFR).
#' 
#' @param object An object of class \code{list}, as created by the function 
#'        \code{fitRandomForest}.
#' @param newdata A data frame or matrix containing the covariates with which 
#'        to predict.
#' 
#' @details 
#' 
#' 
#' The final prediction is defined as the sum of the individual predictions made
#' by each of the individual random forest regression (and simple linear 
#' regression) models.
#' 
#' @return \code{predRandomForest} produces a vector of predictions.
#' 
#' @note Additional arguments supported by \code{predict.randomForest} cannot
#' be passed to \code{predRandomForest}.
#' 
predRandomForest <-
  function (object, newdata) {
    
    # Check if suggested packages are installed
    pkg <- c("randomForest")
    id <- !sapply(pkg, requireNamespace, quietly = TRUE)
    if (any(id)) {
      pkg <- paste(pkg[which(id)], collapse = " ")
      stop(paste("Package(s) needed for this function to work but not",
                 "installed: ", pkg, sep = ""), call. = FALSE)
    }
    
    pred <- list()
    slr <- sapply(object, class)
    if (slr[length(slr)] == "lm") {
      
      # Predict with random forest followed by simple linear regression
      for (i in 1:c(length(slr) - 1)) {
        pred[[i]] <- predict(object = object[[i]], 
                             newdata = data.frame(newdata))
      }
      x <- apply(data.frame(pred), 1, sum)
      pred[[length(slr)]] <- predict(object = object[[length(slr)]], 
                                     newdata = data.frame(x))
    } else {
      
      # Predict with random forest only
      for (i in 1:length(object)) {
        pred[[i]] <- predict(object = object[[i]], newdata = newdata)
      }
    }
    
    # The final prediction is defined as the sum of the individual predictions
    res <- as.numeric(apply(data.frame(pred), 1, sum))
    
    
    # Predict with random forest models
#     pred1 <- predict(object = object[[1]], newdata = newdata)
#     pred2 <- predict(object = object[[2]], newdata = newdata)
#     res <- pred1 - pred2
    
    # Predict with simple linear regression model
#     if (attr(object, "slr")) {
#       x <- data.frame(x = res)
#       res <- predict(object = object[[3]], newdata = x)
#     }
    
    # Output
    return (res)
      
  }
