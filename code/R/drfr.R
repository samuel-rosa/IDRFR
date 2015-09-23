# Fit a debiased random forest regression model ################################
fitDRFR <- 
  function (y, x, ntree = 500, mtry = max(floor(ncol(x) / 3), 1), nodesize = 5,
            slr = FALSE) {
    
    # Fit random forest models
    rf_p <- randomForest(y = y, x = x, ntree = ntree, mtry = mtry, 
                         nodesize = nodesize)
    rf_r <- rf_p$predicted - y
    rf_r <- randomForest(y = rf_r, x = x, ntree = ntree, mtry = mtry, 
                         nodesize = nodesize)
    
    # Fit simple linear regression
    if (slr) {
      x <- rf_p$predicted - rf_r$predicted
      lr <- lm(y ~ x)
      drfr <- list(rf_p, rf_r, lr)
    } else {
      drfr <- list(rf_p, rf_r)
    }
    
    # Prepare output: debiased random forest regression
    a <- attributes(drfr)
    a$slr <- slr
    attributes(drfr) <- a
    return (drfr)
  }

# Predict using a debiased random forest regression model ######################
predictDRFR <-
  function (object, newdata) {
    
    # Predict with random forest models
    pred1 <- predict(object = object[[1]], newdata = newdata)
    pred2 <- predict(object = object[[2]], newdata = newdata)
    res <- pred1 - pred2
    
    # Predict with simple linear regression model
    if (attr(object, "slr")) {
      x <- data.frame(x = res)
      res <- predict(object = object[[3]], newdata = x)
    }
    
    # Output
    return (res)
      
  }

require(randomForest)

# Synthetic dataset (based on Xu (2013)) #######################################
# The raw random forest prediction is biased because it is fundamentally a 
# nearest neighbour method. The naive bias correction consists in fitting a 
# simple linear regression model with the observed values as response variable 
# (Y) and the predicted values as the explanatory variable (X). This method does
# not remove the bias because it ignores that the bias varies non-linearly 
# accross the regression space. In some circumstances, the naive bias correction
# can worsen the prediction accuracy. A robust alternative is to fit a second 
# random forest model to the residuals of the raw random forest predictions.
# The bias-corrected random forest prediction is then defined as the sum of the
# predictions of both random forest models. After this step, it is generally 
# reasonable to assume a linear relationship between observed and predicted 
# values. Fitting a simple linear regression model between observed and 
# predicted values does not seem to affect the predictions.
# 

# Create synthetic data
n <- 50
x <- runif(n = n)
e <- rnorm(n = n, mean = 0, sd = 0.001)
y <- vector()
for (i in 1:n) {
  if (x[i] <= 0.5) {
    y[i] <- x[i] + 0.5# + e[i]
  }
  if (x[i] > 0.5) {
    y[i] <- x[i] - 0.5# + e[i]
  }
}

# Fit random forest models
fit1 <- randomForest(y = y, x = data.frame(x))
pred1 <- predict(fit1, data.frame(x))
fit2 <- randomForest(y = y, x = data.frame(x), corr.bias = TRUE)
pred2 <- predict(fit2, data.frame(x))
fit3 <- fitDRFR(y = y, x = data.frame(x))
pred3 <- predictDRFR(fit3, data.frame(x))
fit4 <- fitDRFR(y = y, x = data.frame(x), slr = TRUE)
pred4 <- predictDRFR(fit4, data.frame(x))

# Plot predictions
plot(y ~ x, pch = 20, type = "n")
abline(a = 0.5, b = 1, lwd = 2)
abline(a = -0.5, b = 1, lwd = 2)
points(pred1 ~ x, pch = 1, cex = 0.5, lwd = 2)
points(pred2 ~ x, pch = 2, cex = 0.5, lwd = 2)
points(pred3 ~ x, pch = 3, cex = 0.5, lwd = 2, col = "red")
points(pred4 ~ x, pch = 4, cex = 0.5, lwd = 2, col = "blue")

# Real data example (based on Xu (2013)) #######################################
# The raw random forest predictions have a significant bias, as seen when the
# average of the mean squared error over 100 simulations is computed. Using the
# naive bias correction implemented in the randomForest package does not remove
# the bias, and can result in poorer results (larger mean squared error). 
# Fitting a second random forest model to the residuals of the first random 
# forest model as a means of computing the bias is the most efficient method.
# The mean squared error is reduced considerably when using the robust bias
# correction method. Finnally, fitting a single linear regression method with 
# the observed values and bias corrected values does not have a considerable
# effect in the prediction accuracy. In fact, it appears to have a slight 
# negative effect.
#
require(MASS)
data("Boston")
p <- 0.7
n <- nrow(Boston)
n_sim <- 100
res_boston <- matrix(0, nrow = n_sim, ncol = 4)

for (j in 1:n_sim) {
  
  # Select calibration and prediction datasets
  i <- sample(n)
  i <- i[1:floor(n * p)]
  
  # Fit random forest models
  fit1 <- randomForest(y = Boston$medv[i], x = Boston[i, -ncol(Boston)])
  fit2 <- randomForest(y = Boston$medv[i], x = Boston[i, -ncol(Boston)],
                       corr.bias = TRUE)
  fit3 <- fitDRFR(y = Boston$medv[i], x = Boston[i, -ncol(Boston)])
  fit4 <- fitDRFR(y = Boston$medv[i], x = Boston[i, -ncol(Boston)], slr = TRUE)
  
  # Predict at new observations
  pred1 <- predict(object = fit1, newdata = Boston[-i, -ncol(Boston)])
  pred2 <- predict(object = fit2, newdata = Boston[-i, -ncol(Boston)])
  pred3 <- predictDRFR(object = fit3, newdata = Boston[-i, -ncol(Boston)])
  pred4 <- predictDRFR(object = fit4, newdata = Boston[-i, -ncol(Boston)])
  
  # Compute perfomance statistics
  res_boston[j, 1] <- mean((pred1 - Boston$medv[-i]) ^ 2)
  res_boston[j, 2] <- mean((pred2 - Boston$medv[-i])^2)
  res_boston[j, 3] <- mean((pred3 - Boston$medv[-i])^2)
  res_boston[j, 4] <- mean((pred4 - Boston$medv[-i])^2)
}

# Compute averaged performance statistics
colMeans(res_boston)
apply(res_boston, 2, sd)

# Real data example (based on Samuel-Rosa et al. (2015)) #######################
# Random forest is essentially a nearest neighbour method. As such, it performs
# best in the regions of the feature space that are more densily sampled. For a
# right skewed variable, more accurate prediction will be made in the left side
# of the histogram, where we find the highest frequency of values. High values
# will be strongly underestimated. The bias correction will work in favor of
# having better estimates of the high values, reducing considerably the mean 
# prediction error.
# For a normally distributed variable, low values are overpredicted, and high 
# values are underpredicted, while the centre of the distribution will be
# predicted accuratelly. The bias correction will improve the predictions
# in the edges of the distribution. However, it is important to note that the
# change in the mean prediction error due to the bias correction is negligible 
# compared to the raw random forest predictions. Because the bias in the raw 
# random forest predictions in the edges of the distribution cancel each other,
# resulting in an optimitic (low) mean prediction error.

# Prepare data
address <- paste("https://raw.githubusercontent.com/samuel-rosa/dnos-sm-rs/",
                 "master/data/point/labData.csv", sep = "")
dnos <- read.table(address, header = TRUE, sep = ";", quote = "")
dnos <- dnos[, c(3:4, 7, 10, 13, 16, 19, 22, 28, 31)]
dnos$logECEC <- log(dnos$ECEC)

# Set parameters
p <- 0.5
n <- nrow(dnos)
n_sim <- 1
res_dnos <- matrix(0, nrow = n_sim, ncol = 4)
y <- "ECEC"
y <- "logECEC"
y <- which(colnames(dnos) == y)

for (j in 1:n_sim) {
  
  # Select calibration and prediction datasets
  i <- sample(n)
  i <- i[1:floor(n * p)]
  
  # Fit random forest models
  fit1 <- randomForest(y = dnos[i, y], x = dnos[i, -y])
  fit2 <- randomForest(y = dnos[i, y], x = dnos[i, -y], corr.bias = TRUE)
  fit3 <- fitDRFR(y = dnos[i, y], x = dnos[i, -y])
  fit4 <- fitDRFR(y = dnos[i, y], x = dnos[i, -y], slr = TRUE)
  
  # Predict at new observations
  pred1 <- predict(object = fit1, newdata = dnos[-i, -y])
  pred2 <- predict(object = fit2, newdata = dnos[-i, -y])
  pred3 <- predictDRFR(object = fit3, newdata = dnos[-i, -y])
  pred4 <- predictDRFR(object = fit4, newdata = dnos[-i, -y])
  
  # Compute perfomance statistics
  res_dnos[j, 1] <- mean((pred1 - dnos[-i, y]) ^ 2)
  res_dnos[j, 2] <- mean((pred2 - dnos[-i, y])^2)
  res_dnos[j, 3] <- mean((pred3 - dnos[-i, y])^2)
  res_dnos[j, 4] <- mean((pred4 - dnos[-i, y])^2)
}

# Compute averaged performance statistics
colMeans(res_dnos)
apply(res_dnos, 2, sd)

# Plot results
lim <- range(c(dnos[-i, y], pred1, pred2, pred3, pred4))
par(mfrow = c(2, 2))
plot(pred1 ~ dnos[-i, y], main = "RF",  xlab = "Observed", ylab = "Predicted", 
     ylim = lim, xlim = lim, 
     sub = paste("ME = ", round(mean(pred1 - dnos[-i, y]), 2), sep = ""))
abline(0, 1, col = 2)
plot(pred2 ~ dnos[-i, y], main = "SLR", xlab = "Observed", ylab = "Predicted", 
     ylim = lim, xlim = lim, 
     sub = paste("ME = ", round(mean(pred2 - dnos[-i, y]), 2), sep = ""))
abline(0, 1, col = 2)
plot(pred3 ~ dnos[-i, y], main = "BC", xlab = "Observed", ylab = "Predicted", 
     ylim = lim, xlim = lim, 
     sub = paste("ME = ", round(mean(pred3 - dnos[-i, y]), 2), sep = ""))
abline(0, 1, col = 2)
plot(pred4 ~ dnos[-i, y], main = "BC + SLR", xlab = "Observed", 
     ylab = "Predicted", ylim = lim, xlim = lim, 
     sub = paste("ME = ", round(mean(pred4 - dnos[-i, y]), 2), sep = ""))
abline(0, 1, col = 2)
