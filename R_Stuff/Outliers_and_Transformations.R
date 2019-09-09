## Detecting Outliers and Transformations

## Load the necessary packages
library(MASS)

## Get the data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")
air <- read.csv("airquality.csv", header = T) 

## Look at the data
pairs(air)

## Fit a model with all explanatory variables
m <- lm(Ozone ~ Solar.R + Wind + Temp, data = air)
summary(m)

## Look at plots of the studentized residuals
par(mfrow = c(2,2))
plot(studres(m), ylim = c(min(-3, min(studres(m))),max(3, max(studres(m)))), main = "Stud. Residuals vs. Index", ylab = "Stud. Residuals")
abline(h=c(0,3,-3), lty = c(1,2,2), col = "red")
plot(m$fitted.values, studres(m), ylim = c(min(-3, min(studres(m))),max(3, max(studres(m)))), main = "Stud. Residuals vs. Fitted Values", ylab = "Stud. Residuals", xlab = "Fitted Values")
abline(h=c(0,3,-3), lty = c(1,2,2), col = "red")
hist(studres(m), main = "Histogram of Stud. Residuals", xlab = "Stud. Residuals")
qqnorm(studres(m), main = "QQ-Plot of Stud. Residuals", ylab = "Residual Quantiles")
qqline(studres(m), col = "red")

## There appears to be one outlier. The code below indicates that it is observation 77.
which(studres(m) > 4)

## Let's check the leverage and influence of this point. In fact, let's do it for all points.
hatvalues(m) # These are the leverages (i.e., diagonal values of the hat matrix)
cooks.distance(m) # These are the influence measures (Cook's D-Statistic)

## Both of these are better visualized as plots
par(mfrow = c(1,2))
plot(hatvalues(m), main = "Leverage", ylab = "Hat Values", ylim = c(0,1))
abline(h = 2*mean(hatvalues(m)), col = "red", lty = 2)
plot(cooks.distance(m), main = "Influence", ylab = "Cook's D", ylim = c(0,1))
abline(h = 0.5, col = "red", lty = 2)

## We see that some points have a relatively high leverage, but not substantially large.
## The point with the largest leverage is the 30th observation.
which(hatvalues(m) == max(hatvalues(m)))

## We can see that one point in particular has a much larger influence than any of the others.
## The point with the largest leverage is the 77th observation.
which(cooks.distance(m) == max(cooks.distance((m))))

## Suppose that the 77th observation is recorded incorrectly and we choose to do the rest of the 
## analysis without it. The following is the "air" data frame with the 77th datapoint removed:
air_new <- air[-77,]

## Fit the full model using this new dataset (the one with the 77th observation deleted)
m_new <- lm(Ozone ~ Solar.R + Wind + Temp, data = air_new)
summary(m_new)

## Look at plots of the studentized residuals
par(mfrow = c(2,2))
plot(studres(m_new), ylim = c(min(-3, min(studres(m_new))),max(3, max(studres(m_new)))), main = "Stud. Residuals vs. Index", ylab = "Stud. Residuals")
abline(h=c(0,3,-3), lty = c(1,2,2), col = "red")
plot(m_new$fitted.values, studres(m_new), ylim = c(min(-3, min(studres(m_new))),max(3, max(studres(m_new)))), main = "Stud. Residuals vs. Fitted Values", ylab = "Stud. Residuals", xlab = "Fitted Values")
abline(h=c(0,3,-3), lty = c(1,2,2), col = "red")
hist(studres(m_new), main = "Histogram of Stud. Residuals", xlab = "Stud. Residuals")
qqnorm(studres(m_new), main = "QQ-Plot of Stud. Residuals", ylab = "Residual Quantiles")
qqline(studres(m_new), col = "red")

## We see again two potential outliers, but this time is seems more like perhaps they arose because
## our distributional assumption is incorrect, or because the variance is not constant. Let us try three
## variance stabilizing transformations:

## Natural Log
m_l <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = air_new)
summary(m_l)

## Square-root
m_sq <- lm(sqrt(Ozone) ~ Solar.R + Wind + Temp, data = air_new)
summary(m_sq)

## Box-Cox
par(mfrow = c(1,1))
bc <- boxcox(m_new, plotit = TRUE)
lambda <- bc$x[which(bc$y == max(bc$y))]
lambda

m_bc <- lm(((Ozone^lambda)-1)/lambda ~ Solar.R + Wind + Temp, data = air_new)
summary(m_bc)

## Look at Studentized Residuals vs. Fitted Values plots for each of these
par(mfrow = c(2,2))
plot(m_new$fitted.values, studres(m_new), ylim = c(min(-3, min(studres(m_new))),max(3, max(studres(m_new)))), main = "No Transformation", ylab = "Stud. Residuals", xlab = "Fitted Values")
abline(h=c(0,3,-3), lty = c(1,2,2), col = "red")
plot(m_l$fitted.values, studres(m_l), ylim = c(min(-3, min(studres(m_l))),max(3, max(studres(m_l)))), main = "Log-Transformation", ylab = "Stud. Residuals", xlab = "Fitted Values")
abline(h=c(0,3,-3), lty = c(1,2,2), col = "red")
plot(m_sq$fitted.values, studres(m_sq), ylim = c(min(-3, min(studres(m_sq))),max(3, max(studres(m_sq)))), main = "Square-Root Transformation", ylab = "Stud. Residuals", xlab = "Fitted Values")
abline(h=c(0,3,-3), lty = c(1,2,2), col = "red")
plot(m_bc$fitted.values, studres(m_bc), ylim = c(min(-3, min(studres(m_bc))),max(3, max(studres(m_bc)))), main = "Box-Cox Transformation", ylab = "Stud. Residuals", xlab = "Fitted Values")
abline(h=c(0,3,-3), lty = c(1,2,2), col = "red")

## We first notice that what appeared to be outliers are not anymore. Furthermore, each of the transformations has 
## eliminated the non-constant variance. However, in the log-transformed model  there is a single observation that 
## appears to be an outlier. If we were to use this model, we may want to do another round of outlier detection/removal.

## Look at QQ-plots of the Studentized Residuals for each transformation
par(mfrow = c(2,2))
qqnorm(studres(m_new), main = "No Transformation", ylab = "Residual Quantiles")
qqline(studres(m_new), col = "red")
qqnorm(studres(m_l), main = "Log-Transformation", ylab = "Residual Quantiles")
qqline(studres(m_l), col = "red")
qqnorm(studres(m_sq), main = "Square-Root Transformation", ylab = "Residual Quantiles")
qqline(studres(m_sq), col = "red")
qqnorm(studres(m_bc), main = "Box-Cox Transformation", ylab = "Residual Quantiles")
qqline(studres(m_bc), col = "red")

## The residuals in the transformed regressions appear to be more normally distributed (aside from one outlier in the
## log-transformed model). In fact the Box-Cox transformed appears to best satisfy the OLS assumptions, however this model
## is not very interpretable. If interpretation is important, the log-transformed model should be used. If prediction is
## important then the Box-Cox model should be used.
