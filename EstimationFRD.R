# Exemplary Implementation of FRD
library(plotrix)
set.seed(535)
cutoff = 5
x_min = 0
x_max = 10

# Sample size
n_sample = 500
# Given bandwidth
window = 2

# Sample generation
X = sort(rnorm(n_sample, mean=5, sd=2))
X = ifelse(X < 0, 0, X)
X = ifelse(X > 10, 10, X)

probs = ifelse(X < 5, 0.3/5 * X, 0.3/5 * X + 0.4) # (Control) y=0.3/5 x / (Treatment) y=0.3/5 x + 0.4
treatments = rbinom(n_sample, 1, probs) # multi bernoulli

XT.pair = cbind(X, treatments)

# (Control) y = 0.3/5 x + 0.2 / (Treatment) y = 0.3/5 x + 0.7
Y = ifelse(XT.pair[,2] == 0, 0.3/5 * XT.pair[,1] + 0.2, 0.3/5 * XT.pair[,1] + 0.7) + rnorm(n_sample, mean=0, sd=0.15)

plot(1, type='n', xlab='', ylab='', xlim=c(0, 10), ylim=c(-0.2, 1.7), xaxt='n', yaxt='n')
# Control Group
points(X[treatments==0], Y[treatments==0], pch=4)
# Treatment Group
points(X[treatments==1], Y[treatments==1], pch=1)
abline(v=cutoff, lty='dotted')
abline(v=cutoff-window, lty='dotted')
abline(v=cutoff+window, lty='dotted')

# data on the left neighborhood
X_0 = X[(X < cutoff) & (X >= cutoff - window)]
Y_0 = Y[(X < cutoff) & (X >= cutoff - window)]
# data on the right neighborhood
X_1 = X[(X >= cutoff) & (X <= cutoff + window)]
Y_1 = Y[(X >= cutoff) & (X <= cutoff + window)]

# left regression line
left.data = data.frame(X_0, Y_0)
left.fit = lm(Y_0 ~ X_0, data=left.data)
ablineclip(left.fit, x1=cutoff-window, x2=cutoff, lwd=1.5, col='red')

# right regression line
right.data = data.frame(X_1, Y_1)
right.fit = lm(Y_1 ~ X_1, data=right.data)
ablineclip(right.fit, x1=cutoff, x2=cutoff+window, lwd=1.5, col='red')

# mu_0 (c) and mu_1 (c)
mu_zero.c = predict(left.fit, data.frame(X_0 = cutoff))
mu_one.c = predict(right.fit, data.frame(X_1 = cutoff))
ablineclip(v=cutoff, y1=mu_zero.c, y2=mu_one.c, col='blue', lwd=2.5)
points(cutoff, mu_zero.c, pch=16 , col='red')
points(cutoff, mu_one.c, pch=16 , col='red')

# Intent-To-Treat Effect
ITT = predict(right.fit, data.frame(X_1 = cutoff)) - predict(left.fit, data.frame(X_0 = cutoff))

# Discontinuity of P(T=1|X=x) at x=c
plot(1, type='n', xlab='', ylab='', xlim=c(0, 10), ylim=c(0, 1), xaxt='n', yaxt='n')
# Control group
points(X[treatments==0], treatments[treatments==0], pch=4)
# Treatment group
points(X[treatments==1], treatments[treatments==1], pch=1)
abline(v=cutoff, lty='dotted')
abline(v=cutoff-window, lty='dotted')
abline(v=cutoff+window, lty='dotted')

# data on the left neighborhood
X_0 = X[(X < cutoff) & (X >= cutoff - window)]
T_0 = treatments[(X < cutoff) & (X >= cutoff - window)]
# data on the right neighborhood
X_1 = X[(X >= cutoff) & (X <= cutoff + window)]
T_1 = treatments[(X >= cutoff) & (X <= cutoff + window)]

# left regression line
left.data = data.frame(X_0, T_0)
left.fit = lm(T_0 ~ X_0, data=left.data)
ablineclip(left.fit, x1=cutoff-window, x2=cutoff, lwd=1.5, col='red')
# right regression line
right.data = data.frame(X_1, T_1)
right.fit = lm(T_1 ~ X_1, data=right.data)
ablineclip(right.fit, x1=cutoff, x2=cutoff+window, lwd=1.5, col='red')

# Pr[T=1 | X=c+] and Pr[T=1 | X=c-]
T_zero.c = predict(left.fit, data.frame(X_0 = cutoff))
T_one.c = predict(right.fit, data.frame(X_1 = cutoff))
ablineclip(v=cutoff, y1=T_zero.c, y2=T_one.c, col='blue', lwd=2.5)
points(cutoff, T_zero.c, pch=16 , col='red')
points(cutoff, T_one.c, pch=16 , col='red')

# delta_FRD
delta =  predict(right.fit, data.frame(X_1 = cutoff)) - predict(left.fit, data.frame(X_0 = cutoff))

# tau_FRD
tau = ITT / delta
