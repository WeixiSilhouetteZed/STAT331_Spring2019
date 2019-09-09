#### Analysis of Sales Data (see problem 4.13 in textbook) ####

## Check where R thinks the working directory is
getwd()

## Set the working directory to where the relevant file is stored
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")

## Read in the relevant file
sales <- read.csv(file = "sales.csv", header = T)

## Look at the data
sales

## Examine individual scatter plots between y and each explanatory variable
par(mfrow = c(2,2))
plot(x = sales$X1, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Promotional Expenditures (in $1000s)", pch = 16)
plot(x = sales$X2, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Number of Active Accounts", pch = 16)
plot(x = sales$X3, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Number of Competing Brands", pch = 16)
plot(x = sales$X4, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "District Potential", pch = 16)

## Construct a full multiple linear regression model with all four explanatory variables
model <- lm(Y ~ X1 + X2 + X3 + X4, data = sales)
summary(model)

## Obtain beta estimates manually
X <- cbind(rep(1, 15), sales$X1, sales$X2, sales$X3, sales$X4) # X matrix
y <- matrix(sales$Y, ncol = 1) # response vector
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat

## Obtain sigma estimate manually
mu_hat <- X %*% beta_hat # fitted values
e <- y - mu_hat # residuals
sigma_hat <- sqrt((t(e) %*% e) / (10)) #n-p-1 = 15-4-1 = 10
sigma_hat

## Compute standard errors of the betas
# Automatic
sqrt(diag(vcov(model)))

# Manual
st.error <- sigma_hat * sqrt(diag(solve(t(X) %*% X)))
st.error

## Estimate the mean response when x1 = 3, x2 = 45, x3 = 10 and x4 = 10 and provide a 95% CI for this estimate
predict(object = model, newdata = data.frame(X1 = 3, X2 = 45, X3 = 10, X4 = 10), interval = "confidence", level = 0.95)

## Do this manually:
x0 <- matrix(c(1, 3, 45, 10, 10), nrow = 1)
mu0_hat <- x0 %*% beta_hat
se_mu0 <- sigma_hat * sqrt(x0 %*% solve(t(X) %*% X) %*% t(x0))
crit_val <- qt(p = 0.975, df = model$df.residual)
ci_lo <- mu0_hat - crit_val*se_mu0
ci_hi <- mu0_hat + crit_val*se_mu0
print(paste(mu0_hat, ci_lo, ci_hi))

## Predict the value of the response when x1 = 3, x2 = 45, x3 = 10 and x4 = 10 and provide a 95% PI for this prediction
predict(object = model, newdata = data.frame(X1 = 3, X2 = 45, X3 = 10, X4 = 10), interval = "prediction", level = 0.95)

## Do this manually:
x0 <- matrix(c(1, 3, 45, 10, 10), nrow = 1)
mu0_hat <- x0 %*% beta_hat
se_y0 <- sigma_hat * sqrt(1+ x0 %*% solve(t(X) %*% X) %*% t(x0))
crit_val <- qt(p = 0.975, df = model$df.residual)
pi_lo <- mu0_hat - crit_val*se_y0
pi_hi <- mu0_hat + crit_val*se_y0
print(paste(mu0_hat, pi_lo, pi_hi))
