## Multicollinearity Examples
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")
library(car)

## The Pizza Example
pizza <- read.csv(file = "pizza.csv", header = T)
pairs(pizza)

m1 <- lm(Sales ~ Num_Ads, data = pizza)
summary(m1)

m2 <- lm(Sales ~ Cost_Ads, data = pizza)
summary(m2)

m3 <- lm(Sales ~ Num_Ads + Cost_Ads, data = pizza)
summary(m3)

m_num <- lm(Num_Ads ~ Cost_Ads, data = pizza)
s_num <- summary(m_num)
VIF_num <- 1 / (1 - s_num$r.squared)
VIF_num

m_cost <- lm(Cost_Ads ~ Num_Ads, data = pizza)
s_cost <- summary(m_cost)
VIF_cost <- 1 / (1 - s_cost$r.squared)
VIF_cost

vif(m3)


## The Credit Example
credit <- read.csv(file = "credit.csv", header = T)
pairs(data.frame(credit$Balance, credit$Age, credit$Limit, credit$Rating))

m1 <- lm(Balance ~ Age + Limit, data = credit)
summary(m1)

m2 <- lm(Balance ~ Age + Rating, data = credit)
summary(m2)

m3 <- lm(Balance ~ Age + Limit + Rating, data = credit)
summary(m3)

mAge <- lm(Age ~ Limit + Rating, data = credit)
sAge <- summary(mAge)
VIFage <- 1/(1-sAge$r.squared)
VIFage

mLimit <- lm(Limit ~ Age + Rating, data = credit)
sLimit <- summary(mLimit)
VIFlimit <- 1/(1-sLimit$r.squared)
VIFlimit

mRating <- lm(Rating ~ Age + Limit, data = credit)
sRating <- summary(mRating)
VIFrating <- 1/(1-sRating$r.squared)
VIFrating

vif(m3)

