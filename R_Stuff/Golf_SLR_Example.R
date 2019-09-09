#### Analysis of PGA Data ####

## Check where R thinks the working directory is
getwd()

## Set the working directory to where the relevant file is stored
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")

## Read in the relevant file
golf <- read.csv(file = "pga_data.csv", header = TRUE)

## Look at the first 10 rows
golf[1:10, ]

## View all of the data
View(golf)

## Let y = the driving accuracy and x = the driving distance
y <- golf$Drv.Acc # percent of fairways hit
x <- golf$Avg.Drive # distance (in yards) of drive

## Construct a scatterplot of these data
plot(x, y, ylab = "Accuracy of Drive (% fairways hit)", xlab = "Distance of Drive (yards)", main = "Driving Accuracy vs. Driving Distance", pch = 16)

## Calculate the correlation coefficent
cor(x, y)

## Now let y = the driving accuracy and x = average number of putts
y <- golf$Drv.Acc # percent of fairways hit
x <- golf$Avg.No.Putts # average number putts once on green

## Construct a scatterplot of these data
plot(x, y, ylab = "Accuracy of Drive (% fairways hit)", xlab = "Average Number of Putts", main = "Driving Accuracy vs. Putting Accuracy", pch = 16)

## Calculated the correlation coefficent
cor(x, y)

## Fit a simple linear regression between driving accuracy and driving distance
y <- golf$Drv.Acc 
x <- golf$Avg.Drive 
model <- lm(y ~ x) # this is the model
s <- summary(model) # look at a summary of the model
s

## Add the fitted line to the scatterplot (remake plot first, and then add line)
plot(x, y, ylab = "Accuracy of Drive (% fairways hit)", xlab = "Distance of Drive (yards)", main = "Driving Accuracy vs. Driving Distance", pch = 16)
abline(model, col = "red", lwd = 2)

## Calculate the parameter estimates manually
beta1_hat <- cor(x,y) * sd(y) / sd(x)
beta1_hat
beta0_hat <- mean(y) - beta1_hat * mean(x)
beta0_hat
mu_hat <- beta0_hat + beta1_hat * x
sigma_hat_mle <- sqrt(mean((y - mu_hat)^2))
sigma_hat_mle
sigma_hat_lse <- sqrt(sum((y - mu_hat)^2)/model$df.residual)
sigma_hat_lse

## Ho: beta0 = 0 vs. Ha: beta0 != 0
se_beta0 <- s$coefficients[1,2]
t <- beta0_hat / se_beta0
p_val <- 2*pt(q = abs(t), df = 194, lower.tail = FALSE)
print(paste("The p-value associated with Ho: beta0 = 0 is ", p_val, sep = ""))

## 95% CI for beta0
crit_val <- qt(p = 0.975, df = 194, lower.tail = TRUE)
low_CL <- beta0_hat - crit_val * se_beta0
upp_CL <- beta0_hat + crit_val * se_beta0 
print(paste("The 95% confidence interval for beta0 is (", round(low_CL, 3), ", ", round(upp_CL, 3), ").", sep = ""))

## Ho: beta1 = 0 vs. Ha: beta1 != 0
se_beta1 <- s$coefficients[2,2]
t <- beta1_hat / se_beta1
p_val <- 2*pt(q = abs(t), df = 194, lower.tail = FALSE)
print(paste("The p-value associated with Ho: beta1 = 0 is ", p_val, sep = ""))

## 95% CI for beta1
low_CL <- beta1_hat - crit_val * se_beta1
upp_CL <- beta1_hat + crit_val * se_beta1 
print(paste("The 95% confidence interval for beta0 is (", round(low_CL, 3), ", ", round(upp_CL, 3), ").", sep = ""))

## 95% CI for mu0 (fitted values)
x0 <- seq(from = min(x), to = max(x), length.out = 100)
mu0_hat <- beta0_hat + beta1_hat * x0
n <- dim(golf)[1]
sxx <- (n-1) * var(x)
se_mu0 <- sigma_hat_lse * sqrt((1/n) + ((x0-mean(x))^2/sxx))
low_CL <- mu0_hat - crit_val * se_mu0
upp_CL <- mu0_hat + crit_val * se_mu0
lines(x0, low_CL, col = "blue", lwd = 2, lty = 2)
lines(x0, upp_CL, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Fitted Values", "95% CI"), lwd = c(2,2), lty = c(1,2), col = c("red", "blue"))

## 95% PI for future values of the response (note that this requires recreating the plot)
x0 <- seq(from = min(x), to = max(x), length.out = 100)
y0_hat <- beta0_hat + beta1_hat * x0 # this is the same as mu0_hat above
se_y0 <- sigma_hat_lse * sqrt(1 + (1/n) + ((x0-mean(x))^2/sxx))
low_PL <- y0_hat - crit_val * se_y0
upp_PL <- y0_hat + crit_val * se_y0
plot(x, y, ylab = "Accuracy of Drive (% fairways hit)", xlab = "Distance of Drive (yards)", main = "Driving Accuracy vs. Driving Distance", pch = 16)
abline(model, col = "red", lwd = 2) #this is the fitted line
lines(x0, low_CL, col = "blue", lwd = 2, lty = 2) #this is the lower confidence limit
lines(x0, low_PL, col = "purple", lwd = 2, lty = 2) #this is the lower prediction limit
lines(x0, upp_CL, col = "blue", lwd = 2, lty = 2) #this is the upper confidence limit
lines(x0, upp_PL, col = "purple", lwd = 2, lty = 2) #this is the upper prediction limit
legend("topright", legend = c("Fitted Values", "95% CI", "95% PI"), lwd = c(2,2,2), lty = c(1,2,2), col = c("red", "blue", "purple"))

## 95% CIs for mu_0 can be obtained automatically using the following line of code:
predict(object = model, newdata = data.frame(x), interval = "confidence", level = 0.95)

## 95% PIs for y_0 can be obtained automatically using the following line of code:
predict(object = model, newdata = data.frame(x), interval = "prediction", level = 0.95)
