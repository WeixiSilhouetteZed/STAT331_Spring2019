par(mfrow = c(1,1))
plot(x = seq(0,5,0.01), y = df(x = seq(0,5,0.01), df1 = 1, df2 = 1), type = "l", col = "yellow", xlab = "x", ylab = "f(x)", lwd = 2, ylim = c(0,2.5))
lines(x = seq(0,5,0.01), y = df(x = seq(0,5,0.01), df1 = 2, df2 = 1), type = "l", col = "green", lwd = 2)
lines(x = seq(0,5,0.01), y = df(x = seq(0,5,0.01), df1 = 5, df2 = 2), type = "l", col = "cyan", lwd = 2)
lines(x = seq(0,5,0.01), y = df(x = seq(0,5,0.01), df1 = 10, df2 = 1), type = "l", col = "blue", lwd = 2)
lines(x = seq(0,5,0.01), y = df(x = seq(0,5,0.01), df1 = 20, df2 = 5), type = "l", col = "purple", lwd = 2)
lines(x = seq(0,5,0.01), y = df(x = seq(0,5,0.01), df1 = 100, df2 = 100), type = "l", col = "red", lwd = 2)
legend("topright", legend = c("a=1, b=1","a=2, b=1","a=5, b=2","a=10, b=1","a=20, b=5","a=100, b=100"),
       lty = 1, lwd = 2, col = c("yellow", "green", "cyan", "blue", "purple", "red"), cex = 1)

