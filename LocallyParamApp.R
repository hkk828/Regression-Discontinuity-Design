# Chapter 4 
# Virtual Implementation of "locally" parametric approach

library(plotrix)
set.seed(535)
cutoff = 5
x_min = 0
x_max = 10

n_sample = 500
window = 2

X = rnorm(n_sample, mean=5, sd=2)
X = ifelse(X < 0, 0, X)
X = ifelse(X > 10, 10, X)
summary(X)

Y = ifelse(X < cutoff, 0.3*X+1, 0.4*X+2.5) + rnorm(n_sample, mean=0, sd=1)

plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(-1.5, 8.5), xaxt='n', yaxt='n')
points(X[X < cutoff], Y[X < cutoff], pch=4, col='black')
points(X[X >= cutoff], Y[X >= cutoff], pch=1, col='black')

abline(v=cutoff, lty='dotted')
abline(v=cutoff-window, lty='dotted')
abline(v=cutoff+window, lty='dotted')

X_0 = X[(X < cutoff) & (X >= cutoff - window)]
Y_0 = Y[(X < cutoff) & (X >= cutoff - window)]
X_1 = X[(X >= cutoff) & (X <= cutoff + window)]
Y_1 = Y[(X >= cutoff) & (X <= cutoff + window)]

left.data = data.frame(X_0, Y_0)
left.fit = lm(Y_0 ~ X_0, data=left.data)
ablineclip(left.fit, x1=cutoff-window, x2=cutoff, lwd=1.5, col='red')
right.data = data.frame(X_1, Y_1)
right.fit = lm(Y_1 ~ X_1, data=right.data)
ablineclip(right.fit, x1=cutoff, x2=cutoff+window, lwd=1.5, col='red')

mu_zero.c = predict(left.fit, data.frame(X_0 = cutoff))
mu_one.c = predict(right.fit, data.frame(X_1 = cutoff))
ablineclip(v=cutoff, y1=mu_zero.c, y2=mu_one.c, col='blue', lwd=2.5)
points(cutoff, mu_zero.c, pch=16 , col='red')
points(cutoff, mu_one.c, pch=16 , col='red')

