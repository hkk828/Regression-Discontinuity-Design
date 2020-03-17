# auxiliary function to find the most common item (returns the smallest if many)
# Credit: https://community.rstudio.com/t/replace-values-of-a-variable-by-the-most-frequent-value/11140/7
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
## Need to run "mywindow.R" first !!!


# 2. H confounds (X) and T (XT-Confounding Model)
set.seed(7)

cutoff = 5
min_x = 0
max_x = 10
Y_error = 0.5
h_0 = 1
h_1 = 3
H_error = 0   # One can set the error for H to be positive, but then the estimation process becomes little unstable.

# Sample size
n_sample = 200

# Beta pool
betas = seq(0, 1, 0.05)
n_betas = length(betas)
# bias^2 is stored
bias.squared = rep(NA, n_betas)


# For each beta, we use 100 trials to choose a bandwidth
# and then use another 100 trials to calculate the average result
for (b in c(1:n_betas)){
  beta = betas[b]
  adj.cutoff = 5 - (beta*h_0)
  # Bandwidth selection for ITT / delta.FRD
  window.XT = rep(NA, 100)
  window.delta = rep(NA, 100)
  for (j in c(1:100)){
    
    # Data Generation
    assign_var = sort(rnorm(n_sample, mean=5, sd=1.5))
    assign_var = ifelse(assign_var<0, 0,
                        ifelse(assign_var > 100, 100, assign_var))
    H = ifelse(assign_var < cutoff, h_0, h_1)
    H = H + rnorm(n_sample, mean=0, sd=H_error)
    
    D = (assign_var + beta*H >= cutoff) # D: Treatment indicator
    W = (assign_var >= adj.cutoff)      # W: (adjusted) Threshold indicator
    
    observed_outcome = ifelse(assign_var + beta*H < cutoff, 0.4*assign_var+1, 0.2*assign_var+3)
    observed_outcome = observed_outcome + rnorm(n_sample, mean=0, sd=Y_error)
    
    # Data Formatting
    data = data.frame(assign_var, D, W, observed_outcome)
    centered_data = data
    centered_data["assign_var"] = centered_data["assign_var"] - adj.cutoff
    
    # select a proper window : my.window()
    w_pool = seq(from=0.2, to=1, by=0.05)
    n_pool = length(w_pool)
    
    # Bandwidth selected from IIT
    my_loss = rep(NA, n_pool)
    for (i in c(1:n_pool)){
      w = w_pool[i]
      index = which(((assign_var >= (adj.cutoff-w)) * (assign_var <= (adj.cutoff+w))) == TRUE)
      window_data = centered_data[index,]
      my_loss[i] = my.window(window_data, w, min_x, max_x, lambda=3)[1]
    }
    # Handling NaN
    na.my = sum(is.na(my_loss))
    m.mywindow = which(my_loss[!is.na(my_loss)] == min(my_loss[!is.na(my_loss)]))
    window.XT[j] = w_pool[m.mywindow+na.my]
    
    # Bandwidth selected from delta.FRD
    delta_loss = rep(NA, n_pool)
    for (i in c(1:n_pool)){
      w = w_pool[i]
      index = which(((assign_var >= (adj.cutoff-w)) * (assign_var <= (adj.cutoff+w))) == TRUE)
      window_data = centered_data[index,]
      window_data["observed_outcome"] = window_data["D"]
      delta_loss[i] = my.window(window_data, w, min_x, max_x, lambda=3)[1]
    }
    # Handling NaN
    na.delta = sum(is.na(delta_loss))
    m.delta = which(my_loss[!is.na(my_loss)] == min(my_loss[!is.na(my_loss)]))
    window.delta[j] = w_pool[m.delta+na.delta]
  }
  # bandwidth selected for gamma = g  
  w.delta = Mode(window.delta)
  w.tau = Mode(window.XT)
  w.beta = min(w.delta, w.tau)
  
  # calculating bias^2 for gamma = g
  bias.sq = rep(NA, 100)
  for (k in c(1:100)){
    assign_var = sort(rnorm(n_sample, mean=5, sd=1.5))
    assign_var = ifelse(assign_var<0, 0,
                        ifelse(assign_var > 100, 100, assign_var))
    H = ifelse(assign_var < cutoff, h_0, h_1)
    H = H + rnorm(n_sample, mean=0, sd=H_error)
    
    D = (assign_var + beta*H >= cutoff)
    W = (assign_var >= adj.cutoff)
    
    observed_outcome = ifelse(assign_var + beta*H < cutoff, 0.4*assign_var+1, 0.2*assign_var+3)
    observed_outcome = observed_outcome + rnorm(n_sample, mean=0, sd=Y_error)
    
    # Data Formatting
    data = data.frame(assign_var, D, assign_var*D, W, assign_var*W, observed_outcome)
    centered_data = data
    centered_data["assign_var"] = centered_data["assign_var"] - adj.cutoff  # Centering around the threshold
    centered_data["assign_var...D"] = centered_data["assign_var"]*centered_data["D"]
    centered_data["assign_var...W"] = centered_data["assign_var"]*centered_data["W"]
    
    index = which(((assign_var >= (adj.cutoff-w.beta)) * (assign_var <= (adj.cutoff+w.beta))) == TRUE)
    window_data = centered_data[index,]
    
    estimated.ITT = lm(observed_outcome ~ assign_var * W, data=window_data)$coef[3]
    estimated.delta = lm(D ~ assign_var * W, data=window_data)$coef[3]
    estimated.effect = estimated.ITT / estimated.delta
    
    # tau(beta) and bias squared
    true.effect = 2 - 0.2*adj.cutoff
    bias.sq[k] = (true.effect - estimated.effect)^2
  }
  # average of 100 trials are stored
  bias.squared[b] = mean(bias.sq)
}
library(latex2exp)
plot(betas, bias.squared, ylim=c(0, 1), pch=19, xlab=TeX("$\\beta$"))
abline(h=mean(bias.squared), lty='solid', col='red', lwd=1.5)
print(mean(bias.squared))
