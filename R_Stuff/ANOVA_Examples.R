## Analysis of Variance (ANOVA)
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")

## PGA Example
golf <- read.csv(file = "pga_data.csv", header = T)
plot(x = golf$Avg.Drive, y = golf$Drv.Acc, ylab = "Accuracy of Drive (% fairways hit)", xlab = "Distance of Drive (yards)", main = "Driving Accuracy vs. Driving Distance", pch = 16)
m_golf <- lm(Drv.Acc ~ Avg.Drive, data = golf)
anova(m_golf)
summary(m_golf)

## MBA Example
mba <- read.csv("mba_data.csv", header = T)
plot(x = mba$GMAT, y = mba$GPA, ylab = "GPA", xlab = "GMAT Score", main = "MBA GPA vs. GMAT Score", pch = 16)
m_mba <- lm(GPA ~ GMAT, data = mba)
anova(m_mba)
summary(m_mba)

## Sales Example
sales <- read.csv(file = "sales.csv", header = T)
par(mfrow = c(2,2))
plot(x = sales$X1, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Promotional Expenditures (in $1000s)", pch = 16)
plot(x = sales$X2, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Number of Active Accounts", pch = 16)
plot(x = sales$X3, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Number of Competing Brands", pch = 16)
plot(x = sales$X4, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "District Potential", pch = 16)
m_sales <- lm(Y ~ X1 + X2 + X3 + X4, data = sales)
anova(m_sales)
summary(m_sales)

