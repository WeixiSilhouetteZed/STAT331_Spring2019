## The Bivariate Normal Distribution
library(plot3D)
library(lattice)
library(mvtnorm)

# Relevant parameters
mu1 <- 10
mu2 <- 15
sigma1 <- 2
sigma2 <- 3
rho <- 1
sigma12 <- rho*sigma1*sigma2

# Mean vector and variance-covariance matrix
mu <- matrix(data = c(mu1,mu2), nrow = 2, ncol = 1)
Sigma <- matrix(data = c(sigma1^2, sigma12, sigma12, sigma2^2), nrow = 2, ncol = 2)

# Visualize this BVN distribution: 3D Surface plot
m <- mesh(seq(mu[1]-3*sqrt(Sigma[1,1]), mu[1]+3*sqrt(Sigma[1,1]), length.out = 75), seq(mu[2]-3*sqrt(Sigma[2,2]), mu[2]+3*sqrt(Sigma[2,2]), length.out = 75))
y1 <- m$x
y1 <- matrix(y1, ncol = 1)
y2 <- m$y
y2 <- matrix(y2, ncol = 1)
f <- dmvnorm(x = cbind(y1, y2), mean = mu, sigma = Sigma)
SurfaceData <- data.frame(y1, y2, f)
wireframe(f ~ y1*y2, data = SurfaceData, xlab = bquote(y[1]), ylab = bquote(y[2]), zlab = "", main = "Bivariate Normal Distribution")

# Visualize this BVN distribution: 2D Contour plot
m <- mesh(seq(mu[1]-3*sqrt(Sigma[1,1]), mu[1]+3*sqrt(Sigma[1,1]), length.out = 300), seq(mu[2]-3*sqrt(Sigma[2,2]), mu[2]+3*sqrt(Sigma[2,2]), length.out = 300))
y1 <- m$x
y1 <- matrix(y1, ncol = 1)
y2 <- m$y
y2 <- matrix(y2, ncol = 1)
f <- dmvnorm(x = cbind(y1, y2), mean = mu, sigma = Sigma)
SurfaceData <- data.frame(y1, y2, f)
levelplot(f ~ y1*y2, at = seq(0, max(f), length.out = 10), contour = TRUE, colorkey = TRUE, xlim = c(mu[1]-3*sqrt(Sigma[1,1]), mu[1]+3*sqrt(Sigma[1,1])), ylim = c(mu[2]-3*sqrt(Sigma[2,2]), mu[2]+3*sqrt(Sigma[2,2])), xlab = bquote(y[1]), ylab = bquote(y[2]), main = "Bivariate Normal Distribution")

# Visualize this BVN distribution: Scatterplot of randomly generated data
res <- rmvnorm(n = 10000, mean = mu, sigma = Sigma)
plot(res, xlim = c(mu[1]-3*sqrt(Sigma[1,1]), mu[1]+3*sqrt(Sigma[1,1])), ylim = c(mu[2]-3.5*sqrt(Sigma[2,2]), mu[2]+3.5*sqrt(Sigma[2,2])), xlab = bquote(y[1]), ylab = bquote(y[2]), main = "Bivariate Normal Distribution")
abline(lm(res[,2]~res[,1]), col = "red", lwd = 2)
abline(v = mean(res[,1]), h = mean(res[,2]), col = "red", lwd = 2)
legend("topleft", legend = c(as.expression(bquote(bar(x)[1] == .(round(mean(res[,1]), 4)))), 
                             as.expression(bquote(bar(x)[2] == .(round(mean(res[,2]), 4)))),
                             as.expression(bquote(s[1] == .(round(sd(res[,1]), 4)))), 
                             as.expression(bquote(s[2] == .(round(sd(res[,2]), 4)))),
                             as.expression(bquote(r == .(round(cor(res)[1,2], 4))))))



