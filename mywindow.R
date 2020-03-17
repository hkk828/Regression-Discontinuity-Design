# My Window Selection (Alternative Bandwidth Selection Method)
# Loss =  (lambda)*(proportion of the window) + (volatility of the estimator at the cutoff)

# X : "assign_var"
# T : "D" (Treatment Assignment)
# W : (Threshold indicator)
# Y : "observed_outcome"

# Input
# data : data.frame [X, D, Y] with (X: "assign_var", D: "D", Y: "observed_outcome")
#                                   X contains sorted, centered, and the assignment variable in [c-w, c+w]
# window : w (numeric)
# min : X_min (numeric) (The smallest Value of the assignment variable)
# max : X_max (numeric) (The largest value of the assignment variable)
# lambda : lambda (numeric) (Tuning parater in the Loss function)

# Output
# loss : loss (numeric) (Loss for given inputs)
# bias : bias (numeric) (window / (max-min))
# sigma : sigma (numeric) (volatility at the cutoff)
# estimate : estimate (numeric) (Estimator of the discontinuity : either on outcomes, or on treatment assignment probability)

my.window = function(data, window, min, max, lambda){
  left.data = data[(data["W"]==FALSE),]
  right.data = data[(data["W"]==TRUE),]
  left.size = dim(left.data)[1]
  right.size = dim(right.data)[1]
  
  left.reg = lm(observed_outcome ~ assign_var, data=left.data)
  left.sigma = summary(left.reg)$coef[1,2]^2
  
  right.reg = lm(observed_outcome ~ assign_var, data=right.data)
  right.sigma = summary(right.reg)$coef[1,2]^2
  
  estimate = summary(right.reg)$coef[1,1] - summary(left.reg)$coef[1,1]
  sigma = left.sigma + right.sigma
  bias = (window / (max-min))
  loss = lambda*(window / (max-min)) + (left.sigma + right.sigma)
  return(c(loss, bias, sigma, estimate))
}
