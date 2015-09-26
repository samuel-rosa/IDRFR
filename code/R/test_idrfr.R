# Iteratively debiased random forest regression
rm(list = ls())
require(randomForest)
source("code/R/idrfr.R")

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

# Create synthetic nonlinear data
n <- 50
set.seed(1984)
x <- runif(n = n)
set.seed(1984)
e <- rnorm(n = n, mean = 0, sd = 0.001)
y <- vector()
for (i in 1:n) {
  if (x[i] <= 0.5) {
    y[i] <- x[i] + 0.5 + e[i]
  }
  if (x[i] > 0.5) {
    y[i] <- x[i] - 0.5 + e[i]
  }
}

# Fit random forest regression
fit1 <- randomForest(y = y, x = data.frame(x), nodesize = 1)
fit2 <- fitRandomForest(y = y, x = data.frame(x), nodesize = 1)

# Compute mean residuals
data.frame(rf1 = mean(fit1$predicted - y), rf2 = mean(fit2$fitted.values - y))
           
# Make predictions
pred1 <- predict(fit1, data.frame(x))
pred2 <- predRandomForest(object = fit2, newdata = data.frame(x))

# Compute error statistics
data.frame(me = c(rf1 = mean(pred1 - y), rf2 = mean(pred2 - y)), 
           mse = c(rf1 = mean((pred1 - y) ^ 2), rf2 = mean((pred2 - y) ^ 2)), 
           mae = c(rf1 = mean(abs(pred1 - y)), rf2 = mean(abs(pred2 - y))))

# Plot predictions against covariate
par(mfrow = c(1, 2))
xy_lim <- range(c(pred1, pred2, y, x))
plot(y ~ x, pch = 20, type = "n", xlim = xy_lim, ylim = xy_lim,
     ylab = "Predicted", xlab = "Covariate")
abline(a = 0.5, b = 1, lwd = 2)
abline(a = -0.5, b = 1, lwd = 2)
points(pred1 ~ x, pch = 1, col = "red")
points(pred2 ~ x, pch = 3, col = "blue")
legend(0.7, 1, legend = c("RF", "IDRF"), col = c("red", "blue"), pch = c(1, 3))

# Plot predictions against observed values
xy_lim <- range(c(pred1, pred2, y))
plot(y ~ y, pch = 20, type = "n", xlim = xy_lim, ylim = xy_lim,
     ylab = "Predicted", xlab = "Observed")
abline(a = 0, b = 1)
points(pred1 ~ y, pch = 1, col = "red")
points(pred2 ~ y, pch = 3, col = "blue")
legend(0, 1, legend = c("RF", "IDRF"), col = c("red", "blue"), pch = c(1, 3))

# Create synthetic linear data
n <- 50
set.seed(2001)
x <- rnorm(n = n)
set.seed(2001)
e <- rnorm(n = n, mean = 0, sd = 0.001)
y <- x + e

# Fit random forest regression and linear regression
fit1 <- randomForest(y = y, x = data.frame(x), nodesize = 1)
fit2 <- lm(y ~ x)

# Make predictions
pred1 <- predict(fit1, data.frame(x))
pred2 <- predict(fit2, data.frame(x))

# Plot predictions against observed values
dev.off()
xy_lim <- range(c(pred1, pred2, y, x))
plot(y ~ x, pch = 20, type = "n", xlim = xy_lim, ylim = xy_lim, 
     ylab = "Predicted", xlab = "Observed")
abline(a = 0, b = 1)
points(pred1 ~ y, pch = 1, col = "red")
points(pred2 ~ y, pch = 3, col = "blue")
legend(-3, 2.5, legend = c("RF", "LM"), col = c("red", "blue"), pch = c(1, 3))

# Compute error statistics
data.frame(me = c(rf1 = mean(pred1 - y), rf2 = mean(pred2 - y)),
           mse = c(rf1 = mean((pred1 - y) ^ 2), rf2 = mean((pred2 - y) ^ 2)),
           mae = c(rf1 = mean(abs(pred1 - y)), rf2 = mean(abs(pred2 - y))))


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
n_sim <- 10
res_boston <- matrix(0, nrow = n_sim, ncol = 4)

for (j in 1:n_sim) {
  
  # Select calibration and prediction datasets
  i <- sample(n)
  i <- i[1:floor(n * p)]
  
  # Fit random forest models
  fit1 <- randomForest(y = Boston$medv[i], x = Boston[i, -ncol(Boston)])
  fit2 <- randomForest(y = Boston$medv[i], x = Boston[i, -ncol(Boston)],
                       corr.bias = TRUE)
  fit3 <- fitRandomForest(y = Boston$medv[i], x = Boston[i, -ncol(Boston)])
  fit4 <- fitRandomForest(y = Boston$medv[i], x = Boston[i, -ncol(Boston)], 
                          slr = TRUE)
  
  # Predict at new observations
  pred1 <- predict(object = fit1, newdata = Boston[-i, -ncol(Boston)])
  pred2 <- predict(object = fit2, newdata = Boston[-i, -ncol(Boston)])
  pred3 <- predRandomForest(object = fit3, newdata = Boston[-i, -ncol(Boston)])
  pred4 <- predRandomForest(object = fit4, newdata = Boston[-i, -ncol(Boston)])
  
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
# Using a simple linear regression after the iterative debias degrades the 
# prediction accuracy to a larger extent when the response variable is skewed.
# If the response variable is Gaussian distributed, the effect of the simple
# linear regression is negligible.

require(sp)

# Prepare data
address <- paste("https://raw.githubusercontent.com/samuel-rosa/dnos-sm-rs/",
                 "master/data/point/labData.csv", sep = "")
dnos <- read.table(address, header = TRUE, sep = ";", quote = "")
dnos <- dnos[, c(3:4, 7, 10, 13, 16, 19, 22, 28, 31)]
dnos$logECEC <- log(dnos$ECEC)
coordinates(dnos) <- ~ longitude + latitude
proj4string(dnos) <- CRS("+init=epsg:4674")
dnos <- spTransform(dnos, CRS("+init=epsg:32722"))
dnos <- data.frame(dnos@coords, dnos@data)

# Set parameters
p <- 0.5
n <- nrow(dnos)
n_sim <- 10
res_dnos <- matrix(0, nrow = n_sim, ncol = 4)
y <- "ECEC"
# y <- "logECEC"
y <- which(colnames(dnos) == y)
x <- 1:9

# Optimize debiasing
tmp <- optimRandomForest(x = dnos[, x], y = dnos[, y], niter = 10, nruns = 2)

for (j in 1:n_sim) {

  # Select calibration and prediction datasets
  i <- sample(n)
  i <- i[1:floor(n * p)]
  
  # Fit random forest models
  fit1 <- randomForest(y = dnos[i, y], x = dnos[i, x])
  fit2 <- randomForest(y = dnos[i, y], x = dnos[i, x], corr.bias = TRUE)
  fit3 <- fitRandomForest(y = dnos[i, y], x = dnos[i, x], niter = 4)
  fit4 <- fitRandomForest(y = dnos[i, y], x = dnos[i, x], niter = 4, slr = TRUE)
  
  # Predict at new observations
  pred1 <- predict(object = fit1, newdata = dnos[-i, x])
  pred2 <- predict(object = fit2, newdata = dnos[-i, x])
  pred3 <- predRandomForest(object = fit3, newdata = dnos[-i, x])
  pred4 <- predRandomForest(object = fit4, newdata = dnos[-i, x])
  
  # Compute perfomance statistics
  res_dnos[j, 1] <- mean((pred1 - dnos[-i, y])^2)
  res_dnos[j, 2] <- mean((pred2 - dnos[-i, y])^2)
  res_dnos[j, 3] <- mean((pred3 - dnos[-i, y])^2)
  res_dnos[j, 4] <- mean((pred4 - dnos[-i, y])^2)
}

# Compute averaged performance statistics
colMeans(res_dnos)
apply(res_dnos, 2, sd)

# Plot results
lim <- range(c(dnos[-i, y], pred1, pred2, pred3, pred4))
par(mfrow = c(2, 2), cex = 0.7)
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
