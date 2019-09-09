
#########################
### R as a Calculator ###
#########################

# Addition
2+8

# Subtraction
2-8

# Multiplication
2*8

# Division
2/8

# Remainder / Modulus
8 %% 3

# Exponents
2^8

# Square Root
sqrt(2)

# Absolute Value
abs(-3)

# Exponential Function
exp(2)

# Natural Logarithm
log(8)

# Logarithm with any base
log(8, base = 2)

# Trigonometric functions
sin(pi/2)
cos(pi)
tan(0)

# BEDMAS
exp((-(2-5)^2)/(2*3)) / sqrt(2*pi*3)

###########################
### Variable Assignment ###
###########################
a <- 2
b <- 8
a+b

a = 2
b = 8
a+b

###############
### Vectors ###
###############

# Create a vector of numbers
v1 <- c(1, 4, 5, 3, 9, 8)

# Create a vector of strings
v2 <- c("STAT331", "linear", "regression")

# Find the length of a vector
length(v1)
length(v2)

# Create a vector that repeats the same element n times
rep(x = 3, times = 7)
rep(x = "cat", times = 4)

# Create a vector that is an integer sequence
1:10
22:27

# Create a vector that is an arbitrary sequence
seq(from = 0, to = 1, by = 0.1)
seq(from = 0, to = 1, length.out = 15)

# Indexing
v1[1]
v1[2]
v1[length(v1)]

# Adding an element to a vector
c(v1, 2)

# Removing an element
v1[-3]

# Sort a vector
sort(v1)

# Scalar arithmetic with a vector
v1 + 3
v1 - 3
v1 * 3
v1 / 3

# Elementwise vector arithmetic
v3 <- 1:6
v1 + v3
v1 - v3
v1 * v3
v1 / v3

####################################
### Numeric summaries of vectors ###
####################################
x <- rnorm(n = 100, mean = 0, sd = 1)

# Mean
mean(x)

# Variance
var(x)

# Standard Deviation
sd(x)

# Minimum
min(x)

# Maximum
max(x)

# Range
range(x)

# Median
median(x)

# Arbitrary Quantiles
quantile(x, probs = 0.5)
quantile(x, probs = c(0.05, 0.25, 0.75, 0.95))

# Rounding
round(x, digits = 4)
round(x, digits = 2)
round(x, digits = 0)

# General Summary
summary(x)

################
### Matrices ###
################

# Create a matrix
M1 <- matrix(data = v1, nrow = 2, ncol = 3, byrow = TRUE)
M2 <- matrix(data = v1, nrow = 2, ncol = 3, byrow = FALSE)

# Transpose a matrix
t(M1)

# Diagonal of a matrix
A <- matrix(data = rpois(n = 9, lambda = 10), nrow = 3, ncol = 3)
A
diag(A)

# Determinant of a matrix
det(A)

# Inverse of a matrix
solve(A)

# Matrix multiplication
A %*% solve(A)

# Create an identity  matrix
diag(5)

# Scalar arithmetic with a matrix
A+2
A-2
A*2
A/2

# Elementwise Matrix Arithmetic
B <- matrix(data = 1:9, nrow = 3, ncol = 3)
A+B
A-B
A*B
A/B

# Indexing
A[3,2]
A[c(1,3), c(1,3)]
A[1,]
A[,2]


#######################
### Simple Logicals ###
#######################
2 > 3
2 >= 3
2 < 3
2 <= 3
2 == 3
2 != 3

###########################
### Logicals and Vectors###
###########################
v1 > 3
v1 == 5
v1 > 3 | v1 <= 1
v1 > 3 & v1 <= 1

########################
### Logical Indexing ###
########################

which(v1 == 5)
which(v1 == min(v1))
which(v1 == max(v1))
v1[which(v1 %% 2 == 0)]
v1[which(v1 %% 2 != 0)]

#####################
### IF Statements ###
#####################

a <- 6
if(a == 5){
  print("a is equal to 5")
}else{
  print("a does not equal 5")
}

b <- 3
if(b == 5){
  print("b is equal to 5")
}else if(b > 5){
  print("b is greater than 5")
}else if(b < 5){
  print("b is less than 5")
}

#################
### FOR Loops ###
#################

for(i in 1:10){
  print(i)
}

for(i in v1){
  print(i)
}

for(i in v2){
  print(i)
}

for(i in 1:3){
  for(j in 1:5){
    print(paste("(", i, ",", j, ")", sep = ""))
  }
}

#########################
### Working with Data ###
#########################

# R contains many datasets already that you can work with simply by calling their names
# To see a list of all such datasets use the following command:
data()

# We're going to look at the "ChickWeight" dataset
? ChickWeight

# But most of the time we need load data into R from a file
getwd()
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Tutorials/")

# Read in a .csv file
df <- read.csv(file = "ChickWeight.csv", header = TRUE)

# Read in a .txt file
df <- read.table(file = "ChickWeight.txt", header = TRUE)

# View the dataset in a spreadsheet window
View(df)

# View the dataset in the console
df

# Dimension of the data frame (rows/columns)
dim(df)

# View the top n rows of the dataframe
head(df, n = 10)

# View the bottom n rows of the dataframe
tail(df, n = 10)

# Summary of the dataframe
summary(df)

# "Chick" and "Diet" should be treated as categorical (not numeric) variables. In R
# a categorocal variable is called a "factor" variable. We should change this. But first
# let's figure out how to access specific columns of a data frame.

# Extracting columns that are named
df$Chick
df$Diet

# Extracting columns (in general) -- behaves like a matrix
df[,3]
df[,4]

# Confirm that the "Chick" and "Diet" variables are not treated as factors
is.factor(df$Chick)
is.factor(df$Diet)

# Coerce them to become factors
df$Chick <- as.factor(df$Chick)
is.factor(df$Chick)

df$Diet <- factor(df$Diet, levels = 1:4, labels = c("Diet 1", "Diet 2", "Diet 3", "Diet 4"))
is.factor(df$Diet)

# Sanity Check
summary(df)

################
### Plotting ###
################

# Univariate plots (for quantitative variables)
hist(x = df$weight, xlab = "Weight (grams)", main = "Histogram of Weight")
boxplot(x = df$weight, xlab = "Weight (grams)", main = "Boxplot of Weight")

hist(x = df$Time, xlab = "Time (days)", main = "Histogram of Time")
boxplot(x = df$Time, xlab = "Time (days)", main = "Boxplot of Time")

# Univariate plots (for categorical variables)
# First the table() function:
table(df$Diet)
barplot(x = table(df$Diet), main = "Frequency Barplot of Diet")
barplot(x = table(df$Diet)/sum(table(df$Diet)), main = "Relative Frequency Barplot of Diet")

pie(x = table(df$Diet), main = "Pie Chart of Diet")

# Bivariate plots (for quantitative variables)
plot(x = df$Time, y = df$weight, xlab = "Time (days)", ylab = "Weight (grams)", main = "Scatter Plot of Weight vs. Time")

# Bivariate plots (for quantitative and categorical variables)
boxplot(df$weight ~ df$Diet, ylab = "Weight (grams)", main = "Weight by Diet Type")

# Multiple plots in one window
par(mfrow = c(1,3))
hist(x = df$weight, xlab = "Weight (grams)", main = "Histogram of Weight")
plot(x = df$Time, y = df$weight, xlab = "Time (days)", ylab = "Weight (grams)", main = "Scatter Plot of Weight vs. Time")
boxplot(df$weight ~ df$Diet, ylab = "Weight (grams)", main = "Weight by Diet Type")

# Bivariate plots (for categorical variables)
# First the table() function:
table(df$Diet, df$Chick)
mosaicplot(x = table(df$Diet, df$Chick), xlab = "Diet", ylab = "Chick ID", main = "Chick ID by Diet")

# Look at another dataset which is a better illustration of this function
? HairEyeColor
HairEyeColor
mosaicplot(x = HairEyeColor[,,1], main = "Male Hair and Eye Colour")
mosaicplot(x = HairEyeColor[,,2], main = "Female Hair and Eye Colour")

# Adding lines to plots
plot(x = rnorm(n = 1000, mean = 0, sd = 1), y = rnorm(n = 1000, mean = 0, sd = 1), xlab = "", ylab = "", main = "", pch = 16, xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5))

# Diagonal Lines
abline(a = 0, b = 1, col = "red")

# Lines with any slope/intercept
abline(a = -1, b = 0.5, col = "green", lwd = 2)

# Horizontal and verticle lines
abline(h = 0, col = "blue", lwd = 2, lty = 2)
abline(v = -2, col = "purple", lwd = 2, lty = 3)

# Line Segments
segments(x0 = 1, y0 = -2, x1 = 3, y1 = -2, col = "magenta", lwd = 3, lty = 1)

# Adding a legend
legend("bottomright", legend = c("Diagonal", "General Line", "Horizontal", "Vertical", "Segment"), 
                      col = c("red", "green", "blue", "purple", "magenta"),
                      lwd = c(1, 2, 2, 2, 3),
                      lty = c(1, 1, 2, 3, 1))

# Add more points, but with a different colour and shape
points(x = rnorm(n = 100, mean = -3, sd = 0.2), y = rnorm(n = 100, mean = 3, sd = 0.2), col = "cyan", pch = 17)

#################################
### Probability Distributions ###
#################################

# The binomial distribution
dbinom(x = 5, size = 20, prob = 0.3)
pbinom(q = 5, size = 20, prob = 0.3)
qbinom(p = 0.5, size = 20, prob = 0.3)
rbinom(n = 10, size = 20, prob = 0.3)

# The Poisson distribution
dpois(x = 4, lambda = 10)
ppois(q = 4, lambda = 10)
qpois(p = 0.5, lambda = 10)
rpois(n = 10, lambda = 10)

# The normal distribution
plot(x = seq(-3, 3, 0.01), y = dnorm(x = seq(-3, 3, 0.01), mean = 0, sd = 1), type = "l", xlab = "", ylab = "", main = "Standard Normal Distribution", col = "darkred", lwd = 2)
pnorm(q = 1.96, mean = 0, sd = 1)
qnorm(p = 0.975, mean = 0, sd = 1)
rnorm(n = 10, mean = 0, sd = 1)

# Student's t-distribution
plot(x = seq(-3, 3, 0.01), y = dt(x = seq(-3, 3, 0.01), df = 25), type = "l", xlab = "", ylab = "", main = "t-Distribution with 25 degrees of freedom", col = "navyblue", lwd = 2)
pt(q = 0, df = 25)
qt(p = 0.90, df = 25)
rt(n = 10, df = 25)

# The F-distribution
plot(x = seq(0, 10, 0.01), y = df(x = seq(0, 10, 0.01), df1 = 25, df2 = 10), type = "l", xlab = "", ylab = "", main = "F-Distribution with 25 and 10 degrees of freedom", col = "darkgreen", lwd = 2)
pf(q = 2, df1 = 25, df2 = 10)
qf(p = 0.90, df1 = 25, df2 = 10)
rf(n = 10, df1 = 25, df2 = 10)

# The chi-squared distribution
plot(x = seq(0, 40, 0.1), y = dchisq(x = seq(0, 40, 0.1), df = 10), type = "l", xlab = "", ylab = "", main = "Chi-Squared Distribution with 10 degrees of freedom", col = "orchid4", lwd = 2)
pchisq(q = 10, df = 10)
qchisq(p = 0.90, df = 10)
rchisq(n = 10, df = 10)
