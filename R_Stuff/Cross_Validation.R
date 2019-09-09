## Cross Validation Examples

setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")
credit <- read.csv(file = "credit.csv", header = T)

## Here we consider a fuller version of the credit card data
library(ISLR)
? Credit

## Ordinary Cross-Validation #####################################
n <- dim(credit)[1]
trn <- sample(x = c(rep(TRUE, round(0.8*n)), rep(FALSE, n-round(0.8*n))), size = n, replace = FALSE)
train <- credit[trn,]
tst <- !trn 
test <- credit[tst,]

m1 <- lm(Balance ~ ., data = train)
pred1 <- predict(object = m1, newdata = test)
RMSE1 <- sqrt(mean((test$Balance - pred1)^2))
RMSE1

m2 <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Student, data = train)
pred2 <- predict(object = m2, newdata = test)
RMSE2 <- sqrt(mean((test$Balance - pred2)^2))
RMSE2

m3 <- lm(Balance ~ Rating, data = train)
pred3 <- predict(object = m3, newdata = test)
RMSE3 <- sqrt(mean((test$Balance - pred3)^2))
RMSE3

## K-fold Cross-Validation #####################################
library(boot)
m1 <- glm(Balance ~ ., data = credit)
RMSE1 <- sqrt((cv.glm(credit, m1, K = 10)$delta)[1])
RMSE1

m2 <- glm(Balance ~ Income + Limit + Rating + Cards + Age + Student, data = credit)
RMSE2 <- sqrt((cv.glm(credit, m2, K = 10)$delta)[1])
RMSE2

m3 <- glm(Balance ~ Rating, data = credit)
RMSE3 <- sqrt((cv.glm(credit, m3, K = 10)$delta)[1])
RMSE3



