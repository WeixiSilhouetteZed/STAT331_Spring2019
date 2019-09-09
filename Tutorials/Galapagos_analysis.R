## Tutorial -- Monday July 29

## Let's investigate some data concerning the diversity of plant species in the 
## Galapagos islands

## Let's import and learn about the data:
library(faraway)
? gala

## We will treat "Species" as the response variable and the 5 geographic variables as
## explanatory variables. For this analysis we will ignore the "Endemics" variable.

## Let's visualize the data:
pairs(gala[,-2])

## Let's fit a model:
m <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(m)

## It seems like not all of the explanatory variables are important, but let's
## check to see whether we're being fooled by multicollinearity.
library(car)
vif(m)

## Multicollinearity doesn't seem to be an issue. Let's check the residuals to make sure 
## our error assumptions are being satisfied
library(MASS)
par(mfrow = c(2,2))
plot(x = 1:dim(gala)[1], y = studres(m), xlab = "Index", ylab = "St. Residuals", main = "St. Residuals vs. Index")
abline(h = c(-3,3), col = "red", lty = 3)
plot(x = m$fitted.values, y = studres(m), xlab = "Fitted Values", ylab = "St. Residuals", main = "St. Residuals vs. Fitted Values")
abline(h = c(-3,3), col = "red", lty = 3)
hist(studres(m), xlab = "St. Residuals", main = "Histogram of St. Residuals")
qqnorm(studres(m), ylab = "Residual Quantiles", main = "QQ-plot of St. Residuals")
qqline(studres(m), col = "red")

## The constant variance assumption seems to be questionable. Also, there appear to be two
## observations that are very different from the others.

## Let's figure which observations these are and also check the leverages and influence values
which(studres(m) >3 | studres(m) < -3)

par(mfrow = c(1,1))
plot(hatvalues(m))
abline(h = 2*mean(hatvalues(m)), col = "red", lty = 3)
which(hatvalues(m) > 2*mean(hatvalues(m)))

par(mfrow = c(1,1))
plot(cooks.distance(m))
which.max(cooks.distance(m))

## Island 16 seems problematic for our model, but maybe a transformation can account for it.
## Afterall, there was non-constant variance we needed to account for.

## Try Box-Cox transformation
bc <- boxcox(m) 
lambda <- bc$x[which.max(bc$y)]
lambda

## Either the cube-root or even square-root look like promising transformations, but let's
## fit a model using the optimal value of lmabda
m_bc <- lm((Species^lambda - 1)/lambda ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(m_bc)

## Check to see whether the constant variance and outlier problems have been fixed:
par(mfrow = c(2,2))
plot(x = 1:dim(gala)[1], y = studres(m_bc), xlab = "Index", ylab = "St. Residuals", main = "St. Residuals vs. Index")
abline(h = c(-3,3), col = "red", lty = 3)
plot(x = m_bc$fitted.values, y = studres(m_bc), xlab = "Fitted Values", ylab = "St. Residuals", main = "St. Residuals vs. Fitted Values")
abline(h = c(-3,3), col = "red", lty = 3)
hist(studres(m_bc), xlab = "St. Residuals", main = "Histogram of St. Residuals")
qqnorm(studres(m_bc), ylab = "Residual Quantiles", main = "QQ-plot of St. Residuals")
qqline(studres(m_bc), col = "red")

## These look good -- the Box-Cox transformation helped everything. But the betas are not
## interpretable now. Let's see what the log-transformation would have done. It would be nice
## to use that transformation since the model is interpretable
m_l <- lm(log(Species) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(m_l)

## Did this transformation fix the outlier and non-constant variance problem?
par(mfrow = c(2,2))
plot(x = 1:dim(gala)[1], y = studres(m_l), xlab = "Index", ylab = "St. Residuals", main = "St. Residuals vs. Index")
abline(h = c(-3,3), col = "red", lty = 3)
plot(x = m_l$fitted.values, y = studres(m_l), xlab = "Fitted Values", ylab = "St. Residuals", main = "St. Residuals vs. Fitted Values")
abline(h = c(-3,3), col = "red", lty = 3)
hist(studres(m_l), xlab = "St. Residuals", main = "Histogram of St. Residuals")
qqnorm(studres(m_l), ylab = "Residual Quantiles", main = "QQ-plot of St. Residuals")
qqline(studres(m_l), col = "red")

## These look good. Let's use this model. Now let's interpret the betas:
coef(m_l)
exp(coef(m_l))

## For a unit-increase in Area (all else equal) we expect the number of plant species to change 
## by a factor of 0.9992650. In other words, we expect the number of plant species to decrease
## by (1-0.9992650)x100% = 0.0735%

## For a unit-increase in Elevation (all else equal) we expect the number of plant species to change 
## by a factor of 1.0045122. In other words, we expect the number of plant species to increase
## by (1.0045122-1)x100% = 0.45122%

## For a unit-increase in Nearest (all else equal) we expect the number of plant species to change 
## by a factor of 1.0167046. In other words, we expect the number of plant species to increase
## by (1.0167046-1)x100% = 1.67046%

## For a unit-increase in Scruz (all else equal) we expect the number of plant species to change 
## by a factor of 0.9961107. In other words, we expect the number of plant species to decrease
## by (1-0.9961107)x100% = 0.38893%

## For a unit-increase in Adjacent (all else equal) we expect the number of plant species to change 
## by a factor of 0.9992002. In other words, we expect the number of plant species to decrease
## by (1-0.9992002)x100% = 0.07998%

## The changes in response corresponding to changes in "Nearest" and "Scruz" are not statistically 
## significant. Let's fit a reduced model without them and see if fits the data significantly worse
m_l_r <- lm(log(Species) ~ Area + Elevation + Adjacent, data = gala)
summary(m_l_r)
anova(m_l_r, m_l)

## So the reduced model doesn't fit significantly worse and the adjusted R^2 has actually increased,
## and all of the remaining terms are significant. This seems like a good model.

## Out of curiosity, which model would the different selection techniques have chosen as being optimal?
## Let's use BIC as the decision metric.

## All possible regressions
library(leaps)
all_poss <- regsubsets(log(Species) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala, nvmax = 5, nbest = 2^5)
all_poss_summ <- summary(all_poss)
min.indx <- which.min(all_poss_summ$bic)
all_poss_summ$which[min.indx,]

## Forward Selection
sml <- lm(log(Species) ~ 1, data = gala)
lrg <- lm(log(Species) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
m_fs <- stepAIC(object = sml, scope = list(upper = lrg, lower = sml), direction = "forward", trace = 1, k = log(dim(gala)[1]))
summary(m_fs)

## Backward Selection
m_bs <- stepAIC(object = lrg, scope = list(upper = lrg, lower = sml), direction = "backward", trace = 1, k = log(dim(gala)[1]))
summary(m_bs)

## Hybrid selection
m_hs <- stepAIC(object = sml, scope = list(upper = lrg, lower = sml), direction = "both", trace = 1, k = log(dim(gala)[1]))
summary(m_hs)

## So, same same. We did good.