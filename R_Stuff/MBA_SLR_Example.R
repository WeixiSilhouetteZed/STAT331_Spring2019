#### Analysis of MBA Data ####

## Check where R thinks the working directory is
getwd()

## Set the working directory to where the relevant file is stored
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")

## Read in the relevant file
mba <- read.csv("mba_data.csv", header = T)

## Look at the data
mba

## Let y = the GPA in the MBA program and x = the GMAT score prior to MBA program
y <- mba$GPA 
x <- mba$GMAT

## Construct a scatterplot of these data
plot(x, y, ylab = "GPA", xlab = "GMAT Score", main = "MBA GPA vs. GMAT Score", pch = 16)

## Calculate the correlation coefficent
cor(x, y)

## Fit a simple linear regression between MBA GPA and GMAT Score
model <- lm(y~x)
summary(model)

## Add the fitted line to the scatterplot
abline(model, col = "red", lwd = 2)

## Add 95% CIs and PIs to the plot
xp <- seq(from = min(x), to = max(x), length.out = 100)
CI <- predict(model, newdata = data.frame(x = xp), interval = "confidence", level = 0.95)
PI <- predict(model, newdata = data.frame(x = xp), interval = "prediction", level = 0.95)
ci_low <- CI[,2]
ci_hi <- CI[,3]
pi_low <- PI[,2]
pi_hi <- PI[,3]
plot(x, y, ylab = "GPA", xlab = "GMAT Score", main = "MBA GPA vs. GMAT Score", pch = 16, ylim = c(min(pi_low), max(pi_hi))) #Note we have to remake the plot because the vertical axis was not wide enough
abline(model, col = "red", lwd = 2)
lines(x = xp, y = ci_low, col = "blue", lty = 2, lwd = 2)
lines(x = xp, y = ci_hi, col = "blue", lty = 2, lwd = 2)
lines(x = xp, y = pi_low, col = "purple", lty = 2, lwd = 2)
lines(x = xp, y = pi_hi, col = "purple", lty = 2, lwd = 2)
legend("bottomright", legend = c("Fitted Values", "95% CI", "95% PI"), lwd = c(2,2,2), lty = c(1,2,2), col = c("red", "blue", "purple"))

