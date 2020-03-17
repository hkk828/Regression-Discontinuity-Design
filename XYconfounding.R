# auxiliary function to find the most common item (returns the smallest if many)
# Credit: https://community.rstudio.com/t/replace-values-of-a-variable-by-the-most-frequent-value/11140/7
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
## Need to run "mywindow.R" first !!!


# 1. H confounds (X) and Y (XY-Confounding Model)
set.seed(5)

cutoff = 5
min_x = 0
max_x = 10
Y_error = 0.5
h_0 = 1
h_1 = 3
H_error = 1
true.effect = 1
confounding.effect = 2

# Sample size
n_sample = 200

# gamma pool
gammas = seq(-1, 1, 0.1)
n_gammas = length(gammas)
# bias^2 is stored
bias.squared = rep(NA, n_gammas)


# For each gamma, average value of 100 trials are recorded
for (g in c(1:n_gammas)){
  gamma = gammas[g]
# Bandwidth selection with another 100 data sets
  window.XY = rep(NA, 100)
  for (j in c(1:100)){
  assign_var = sort(rnorm(n_sample, mean=5, sd=1.5))
  assign_var = ifelse(assign_var<0, 0,
                      ifelse(assign_var > 100, 100, assign_var))
  H = ifelse(assign_var < cutoff, h_0, h_1)
  H = H + rnorm(n_sample, mean=0, sd=H_error)
  
  D = (assign_var >= cutoff)  # D: Treatment indicator
  W = D                       # W: Threshold indicator
  
  observed_outcome = ifelse(assign_var < cutoff, 0.4*assign_var+1, 0.2*assign_var+3)
  observed_outcome = observed_outcome + gamma*H + rnorm(n_sample, mean=0, sd=Y_error)
  
  # Data Formatting
  data = data.frame(assign_var, D, W, observed_outcome)
  centered_data = data
  centered_data["assign_var"] = centered_data["assign_var"] - cutoff
  
  # Select a bandwidth with my.window() for each data set
  w_pool = seq(from=0.2, to=1, by=0.05)
  n_pool = length(w_pool)

  my_loss = rep(NA, n_pool)
  for (i in c(1:n_pool)){
    w = w_pool[i]
    index = which(((assign_var >= (cutoff-w)) * (assign_var <= (cutoff+w))) == TRUE)
    window_data = centered_data[index,]
    
    my_loss[i] = my.window(window_data, w, min_x, max_x, lambda=5.5)[1]
  }
# Handling NaN
  na.my = sum(is.na(my_loss))
  m.mywindow = which(my_loss[!is.na(my_loss)] == min(my_loss[!is.na(my_loss)]))
  window.XY[j] = w_pool[m.mywindow+na.my]
}
# window selected for gamma = g  
w.gamma = Mode(window.XY)

# calculating bias^2 for gamma = g
bias.sq = rep(NA, 100)
for (k in c(1:100)){
  assign_var = sort(rnorm(n_sample, mean=5, sd=1.5))
  assign_var = ifelse(assign_var<0, 0,
                      ifelse(assign_var > 100, 100, assign_var))
  H = ifelse(assign_var < cutoff, h_0, h_1)
  H = H + rnorm(n_sample, mean=0, sd=H_error)
  
  D = (assign_var >= cutoff)
  W = D
  
  observed_outcome = ifelse(assign_var < cutoff, 0.4*assign_var+1, 0.2*assign_var+3)
  observed_outcome = observed_outcome + gamma*H + rnorm(n_sample, mean=0, sd=Y_error)
  
  # Data Formatting (For now, W = D)
  data = data.frame(assign_var, D, assign_var*D, observed_outcome)
  centered_data = data
  centered_data["assign_var"] = centered_data["assign_var"] - cutoff  # Centering around the threshold
  centered_data["assign_var...D"] = centered_data["assign_var"]*centered_data["D"]

  index = which(((assign_var >= (cutoff-w.gamma)) * (assign_var <= (cutoff+w.gamma))) == TRUE)
  window_data = centered_data[index,]
  
  estimated.effect = lm(observed_outcome ~ assign_var * D, data=window_data)$coef[3]
  bias.sq[k] = (true.effect - estimated.effect)^2
}
# average of 100 trials are stored
bias.squared[g] = mean(bias.sq)
}
library(latex2exp)
plot(gammas, bias.squared, pch=19, ylim=c(0, 4), xlab=TeX("$\\gamma$"), ylab=TeX("bias^2"), xlab.cex=1.5)
g.sq = gammas^2   # gamma^2
a.hat = lm(bias.squared ~ g.sq + 0)$coef  # True Bias^2 = a*(gamma^2)
g = seq(-1, 1, 0.01)
lines(g, a.hat*g^2, col='red', lwd=1.5)   # Fitted regression curve




