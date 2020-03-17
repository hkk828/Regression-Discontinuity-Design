# Ludwig and Miller (2005) - Cross Validation Figures
# Left Side
library(plotrix)
set.seed(15)
plot(1, type='n', xlab="", ylab="", xlim=c(0, 5), ylim=c(-1, 3))
X = runif(10, min=-1, max=5)
Y = 0.3 * X + rnorm(10, mean=0, sd=0.3)
points(X, Y, pch=22)
abline(v=X[10], lty='dotted')
abline(v=X[10]-2.5, lty='dotted')
abline(v=5, col='blue', lwd=1.5)

left.X = X[(X < X[10]) & (X >= (X[10]-2.5))]
left.Y = Y[(X < X[10]) & (X >= X[10]-2.5)]
data = data.frame(left.X, left.Y)
left.reg = lm(left.Y ~ left.X, data=data)
ablineclip(left.reg, x1=X[10]-2.5, x2=X[10], lwd=1.5)
points(X[10], predict(left.reg, data.frame(left.X = X[10])), pch=8)

# Right Side
set.seed(25)
plot(1, type='n', xlab="", ylab="", xlim=c(5, 10), ylim=c(0, 3))
X = runif(10, min=5, max=10)
Y = 0.2 * X + rnorm(10, mean=0, sd=0.3)
points(X, Y, pch=22)
abline(v=X[10], lty='dotted')
abline(v=X[10]+2.5, lty='dotted')
abline(v=5, col='blue', lwd=1.5)

right.X = X[(X > X[10]) & (X <= X[10]+2.5)]
right.Y = Y[(X > X[10]) & (X <= X[10]+2.5)]
data = data.frame(left.X, left.Y)
right.reg = lm(right.Y ~ right.X, data=data)
ablineclip(right.reg, x1=X[10], x2=X[10]+2.5, lwd=1.5)
points(X[10], predict(right.reg, data.frame(right.X = X[10])), pch=8)

