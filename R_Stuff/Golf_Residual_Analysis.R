## Evaluating Model Assumptions with Residuals -- PGA Data

## Change working directory
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")

## Load the PGA data
golf <- read.csv("pga_data.csv", header = T)

## Create a linear regression with driving accuracy as the response and 
## driving distance as the explanatory variable
y <- golf$Drv.Acc
x <- golf$Avg.Drive
model <- lm(y ~ x, data = golf)
summ <- summary(model)
summ 

## Plot the data and the fitted model
plot(x = x, y = y, col = "black", pch = 16, xlab = "Driving Distance (yards)", ylab = "Driving Accuracy (%)", main = "Driving Accuracy vs. Distance")
abline(model, col = "red", lwd = 2)

## Create Residual Diagnostic Plots
## Plot of residuals vs. index
n <- length(model$residuals)
index <- 1:n
plot(x = index, y = model$residuals, col = "purple", pch = 16, xlab = "Index", ylab = "Model Residuals", main = "Residuals vs. Index")
abline(h = 0, col = "red", lwd = 2)

## Plot of Residuals vs. Fitted Values
plot(x = model$fitted.values, y = model$residuals, col = "darkblue", pch = 16, xlab = "Fitted Values", ylab = "Model Residuals", main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red", lwd = 2)

## Plot of Residuals vs. x
plot(x = x, y = model$residuals, col = "darkgreen", pch = 16, xlab = "Driving Distance", ylab = "Model Residuals", main = "Residuals vs. Driving Distance")
abline(h = 0, col = "red", lwd = 2)

## Histogram of residuals
hist(model$residuals, xlab = "Model Residuals", main = "Histogram of Residuals")

## QQ-plot of residuals
qqnorm(model$residuals, pch = 16, main = "QQ-plot of Residuals")
qqline(model$residuals, col = "red", lwd = 2)

## Redo all of this with 'Studentized' Residuals ##############################
sigma_hat <- summ$sigma # sigma-hat
X <- cbind(rep(1, 196), x) # X matrix
H <- X %*% solve(t(X) %*% X) %*% t(X) # Hat matrix
h <- diag(H) # diagonal elements of H
# OR
h <- hatvalues(model)
st_resid <- model$residuals/(sigma_hat*sqrt(1-h)) # 'Studentized' Residuals

## Plot of St. Residuals vs. Index
plot(x = index, y = st_resid, col = "purple", pch = 16, xlab = "Index", ylab = "Studentized Residuals", main = "Studentized Residuals vs. Index")
abline(h = 0, col = "red", lwd = 2)

## Plot of St. Residuals vs. Fitted Values
plot(x = model$fitted.values, y = st_resid, col = "darkblue", pch = 16, xlab = "Fitted Values", ylab = "Studentized Residuals", main = "Studentized Residuals vs. Fitted Values")
abline(h = 0, col = "red", lwd = 2)

## Histogram of St. Residuals
hist(st_resid, xlab = "Studentized Residuals", main = "Histogram of Studentized Residuals")

## QQ-plot of St. Residuals
qqnorm(st_resid, pch = 16, main = "QQ-plot of Studentized Residuals")
qqline(st_resid, col = "red", lwd = 2)
