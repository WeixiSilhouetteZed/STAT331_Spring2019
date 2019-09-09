## Bike share example -- one categorical explanatory variable

## Read-in the data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_331/Lecture Material/")
bike <- read.csv("bike_share.csv", header = TRUE)

## Preview the data
head(bike)
summary(bike)

## Extract response and "season" as the explanatory variable of interest
y <- bike$count
x <- bike$season

## Examine their relationship
plot(x, y, xlab = "Season", ylab = "Number of Bike Rentals", main = "Hourly Bike Rentals by Season")
boxplot(y~x, xlab = "Season", ylab = "Number of Bike Rentals", main = "Hourly Bike Rentals by Season")

## Naive linear regression
m1 <- lm(y ~ x)
summary(m1)
abline(m1, col = "red", lwd = "2")

## Correct linear regression
m2 <- lm(y ~ factor(x))
summary(m2)

## Or:
x <- factor(x, levels = 1:4, labels = c("Spring", "Summer", "Fall", "Winter"))
m3 <- lm(y ~ x)
summary(m3)


## Extract response and "weather" as the explanatory variable of interest
y <- bike$count
x <- bike$weather

## Examine their relationship
plot(x, y, xlab = "Weather", ylab = "Number of Bike Rentals", main = "Hourly Bike Rentals by Weather type")
boxplot(y~x, xlab = "Season", ylab = "Number of Bike Rentals", main = "Hourly Bike Rentals by Weather type")

## Naive linear regression
m4 <- lm(y ~ x)
summary(m4)
abline(m4, col = "red", lwd = "2")

## Correct linear regression
m5 <- lm(y ~ factor(x))
summary(m5)

## Or:
x <- factor(x, levels = 1:4, labels = c("Nice", "Cloudy", "Rainy", "Stormy"))
m6 <- lm(y ~ x)
summary(m6)
