## Model Selection

setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")
credit <- read.csv(file = "credit.csv", header = T)
  
## Here we consider a fuller version of the credit card data
library(ISLR)
? Credit

## All possible regressions #####################################

# Fit all possible models and for a given number of explanatory variables, find
# the best model (in terms of adjusted R^2)
library(leaps)
all_poss <- regsubsets(Balance ~ ., data = credit, nvmax = 11, nbest = 2^11, really.big = TRUE)
all_poss_summ <- summary(all_poss)
all_poss_summ$which
all_poss_summ$adjr2
# Visualize the results with a plot (of all possible adjusted-R^2 values):
q <- 11
order <- c(rep(1, choose(q,1)),
           rep(2, choose(q,2)),
           rep(3, choose(q,3)),
           rep(4, choose(q,4)),
           rep(5, choose(q,5)),
           rep(6, choose(q,6)),
           rep(7, choose(q,7)),
           rep(8, choose(q,8)),
           rep(9, choose(q,9)),
           rep(10, choose(q,10)),
           rep(11, choose(q,11)))
boxplot(all_poss_summ$adjr2 ~ order, xlab = "Model Order", ylab = expression(R[adj]^2), ylim = c(0,1))
abline(h = c(0,1), lty = 2, col = "red")

# Find the optimal model (in terms of adjusted-R^2)
max_idx <- which.max(all_poss_summ$adjr2)
max_idx

# So which model is this?
all_poss_summ$which[max_idx,]
m_ap <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Gender + Student, data = credit)
summary(m_ap)

## Stepwise Selection #####################################
library(MASS)

## Now let's perform Stepwise model selection and use AIC is a selection criteria.

# Define the scope of the search
sml <- lm(Balance ~ 1, data = credit)
lrg <- lm(Balance ~ ., data = credit)

# Forward
stepAIC(object = sml, scope = list(upper = lrg, lower = sml), direction = "forward")
m_f <- stepAIC(object = sml, scope = list(upper = lrg, lower = sml), direction = "forward", trace = 0)
summary(m_f)

# Backward
stepAIC(object = lrg, scope = list(upper = lrg, lower = sml), direction = "backward")
m_b <- stepAIC(object = lrg, scope = list(upper = lrg, lower = sml), direction = "backward", trace = 0)
summary(m_b)

# Hybrid
stepAIC(object = sml, scope = list(upper = lrg, lower = sml), direction = "both")
m_h <- stepAIC(object = sml, scope = list(upper = lrg, lower = sml), direction = "both", trace = 0)
summary(m_h)

## All three stepwise selection techniques pick the same model (this will not necessarily be the case).
## The optimal model by this process has all of the explanatory variables the optimal "all possible regressions"
## one does, except Gender.
