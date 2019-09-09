## Additional Sum of Squares Principle

## Consider the "sales" data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")
sales <- read.csv(file = "sales.csv", header = T)
par(mfrow = c(2,2))
plot(x = sales$X1, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Promotional Expenditures (in $1000s)", pch = 16)
plot(x = sales$X2, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Number of Active Accounts", pch = 16)
plot(x = sales$X3, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "Number of Competing Brands", pch = 16)
plot(x = sales$X4, y = sales$Y, ylab = "Sales (in $100,000s)", xlab = "District Potential", pch = 16)

## Full model:
m_full <- lm(Y ~ X1 + X2 + X3 + X4, data = sales)
summary(m_full)
anova(m_full)
SSE_full <- anova(m_full)$`Sum Sq`[5]

## Reduced model (beta1=beta4=0):
m_red <- lm(Y ~ X2 + X3, data = sales)
summary(m_red)
anova(m_red)
SSE_red <- anova(m_red)$`Sum Sq`[3]

## F-statistic
l <- 2
n <- dim(sales)[1]
p <- 4
t <- ((SSE_red - SSE_full)/l) / (SSE_full / (n-p-1))
pval <- pf(q = t, df1 = l, df2 = n-p-1, lower.tail = F)

## Test of overall significance
m_red2 <- lm(Y ~ 1, data = sales)
summary(m_red2)
anova(m_red2)
SSE_red2 <- anova(m_red2)$`Sum Sq`[1]

## F-statistic
l <- 4
n <- dim(sales)[1]
p <- 4
t <- ((SSE_red2 - SSE_full)/l) / (SSE_full / (n-p-1))
pval <- pf(q = t, df1 = l, df2 = n-p-1, lower.tail = F)




## Consider the bike rentals data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Assignments/Assignment 1/")
bike <- read.csv(file = "bike_share.csv", header = T)
par(mfrow = c(1,1))
boxplot(bike$count ~ bike$season, xaxt = "n", ylab = "Number of Bike Rentals", xlab = "Season")
axis(side = 1, at = 1:4, labels = c("Spring", "Summer", "Fall", "Winter"))

## Full model:
m_full <- lm(count ~ factor(season), data = bike)
summary(m_full)
anova(m_full)
SSE_full <- anova(m_full)$`Sum Sq`[2]

## Reduced model (beta1=beta2=beta3=0):
m_red <- lm(count ~ 1, data = bike)
summary(m_red)
anova(m_red)
SSE_red <- anova(m_red)$`Sum Sq`[1]

## F-statistic
l <- 3
n <- dim(bike)[1]
p <- 3
t <- ((SSE_red - SSE_full)/l) / (SSE_full / (n-p-1))
pval <- pf(q = t, df1 = l, df2 = n-p-1, lower.tail = F)

