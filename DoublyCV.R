# Doubly k-fold Cross Validation
# Input: data, k, cutoff, (degree)
# Output: Cross-Validation loss, (From here fitted with full data) variance loss, R squared, Residual error, estimates

# data format: data-frame (x, D, y), x: centered
Doubly.CV = function(data, k=2, degree=1, window){
  left.data = data[(data["D"]==FALSE),]
  right.data = data[(data["D"]==TRUE),]
  
  left.size = dim(left.data)[1]
  right.size = dim(right.data)[1]
  
  left.index = sample(c(1:left.size), left.size, replace=FALSE)
  left.folds = cut(c(1:left.size), breaks=k, labels=FALSE)
  right.index = sample(c(1:right.size), right.size, replace=FALSE)
  right.folds = cut(c(1:right.size), breaks=k, labels=FALSE)
  
  cv.loss.left = rep(NA, k)
  cv.loss.right = rep(NA, k)
  weighted.cv.loss.left = rep(NA, k)
  weighted.cv.loss.right = rep(NA, k)
  constraint.cv.loss = rep(NA, k)
  
  for(i in c(1:k)){
    # Validation data and training data for left/right of the cutoff
    left.valid = left.data[left.index[left.folds==i],]
    left.fit = left.data[-left.index[left.folds==i],]
    right.valid = right.data[right.index[right.folds==i],]
    right.fit = right.data[-right.index[right.folds==i],]
    
    # fit the data with training data only
    left.model = lm(observed_outcome~poly(assign_var, degree, raw=TRUE), data=left.fit)
    right.model = lm(observed_outcome~poly(assign_var, degree, raw=TRUE), data=right.fit)
    
    # Weighted Regression
    weighted.left.model = lm(observed_outcome~poly(assign_var, degree, raw=TRUE), weights=-abs(assign_var)/(window^2) + 1/window, data=left.fit)
    weighted.right.model = lm(observed_outcome~poly(assign_var, degree, raw=TRUE), weights=-abs(assign_var)/(window^2) + 1/window, data=right.fit)
    
    # Constraint at the cutoff
    both.fit = rbind(left.fit, right.fit)
    A = matrix(NA, nrow=(dim(both.fit)[1]), ncol=2*degree+2)
    A[,1] = 1
    for(j in c(1:(2*degree+1))){
      if(j<=degree){
        A[,j+1] = (both.fit["assign_var"][,])^j
      }
      else{
        A[,j+1] = (both.fit["assign_var"][,])^(j-degree-1) * (both.fit["D"][,])
      }
    }
    a = matrix(0, ncol=1, nrow=2*degree+2)
    a[degree+3,1] = 1
    deriv.diff = 0 # The same derivative at the cutoff
    
    print(A)
      
    constraint.model = solve.QP(t(A)%*%A, t(A)%*%both.fit["observed_outcome"][,], a, deriv.diff, meq=1)
    
    # MSE with Validation data
    left.MSE = mean((predict(left.model, data.frame(assign_var = left.valid["assign_var"][,1])) - left.valid["observed_outcome"][,1])^2)
    right.MSE = mean((predict(right.model, data.frame(assign_var = right.valid["assign_var"][,1])) - right.valid["observed_outcome"][,1])^2)
    
    cv.loss.left[i] = left.MSE
    cv.loss.right[i] = right.MSE
    
    weighted.left.MSE = mean((predict(weighted.left.model, data.frame(assign_var = left.valid["assign_var"][,1])) - left.valid["observed_outcome"][,1])^2)
    weighted.right.MSE = mean((predict(weighted.right.model, data.frame(assign_var = right.valid["assign_var"][,1])) - right.valid["observed_outcome"][,1])^2)
    
    weighted.cv.loss.left[i] = weighted.left.MSE
    weighted.cv.loss.right[i] = weighted.right.MSE
    
    both.valid = rbind(left.valid, right.valid)
    valid.mat = matrix(NA, nrow=(dim(both.valid)[1]), ncol=2*degree+2)
    valid.mat[,1] = 1
    for(j in c(1:(2*degree+1))){
      if(j<=degree){
        valid.mat[,j+1] = (both.valid["assign_var"][,])^j
      }
      else{
        valid.mat[,j+1] = (both.valid["assign_var"][,])^(j-degree-1) * (both.valid["D"][,])
      }
    }
    constraint.cv.loss[i] = mean((valid.mat%*%(constraint.model$solution) - both.valid["observed_outcome"][,])^2)
  }
  
  # End of Cross Validation, Now fit the whole data
  reg.left = lm(observed_outcome~poly(assign_var, degree, raw=TRUE), data=left.data)
  reg.right = lm(observed_outcome~poly(assign_var, degree, raw=TRUE), data=right.data)
  
  var.loss = summary(reg.left)$coef[1,2]^2 + summary(reg.right)$coef[1,2]^2
  R.squared = summary(reg.left)$r.squared + summary(reg.right)$r.squared
  Res.error = summary(reg.left)$sigma^2 + summary(reg.right)$sigma^2
  estimate = reg.right$coef[1] - reg.left$coef[1]
  
  # For weighted regression
  weighted.reg.left = lm(observed_outcome~poly(assign_var, degree, raw=TRUE), weights=-abs(assign_var)/(window^2) + 1/window, data=left.data)
  weighted.reg.right = lm(observed_outcome~poly(assign_var, degree, raw=TRUE), weights=-abs(assign_var)/(window^2) + 1/window, data=right.data)
  weighted.estimate = weighted.reg.right$coef[1] - weighted.reg.left$coef[1]
  
  # For constraint at the cutoff
  A = matrix(NA, nrow=(dim(data)[1]), ncol=2*degree+2)
  A[,1] = 1
  for(j in c(1:(2*degree+1))){
    if(j<=degree){
      A[,j+1] = (data["assign_var"][,])^j
    }
    else{
      A[,j+1] = (data["assign_var"][,])^(j-degree-1) * (data["D"][,])
    }
  }
  a = matrix(0, ncol=1, nrow=2*degree+2)
  a[degree+3,1] = 1
  deriv.diff = 0 # The same derivative at the cutoff
  constraint.reg.model = solve.QP(t(A)%*%A, t(A)%*%data["observed_outcome"][,], a, deriv.diff, meq=1)
  constraint.estimate = constraint.reg.model$solution[degree+2]
  
  cv.loss = (sum(cv.loss.left) + sum(cv.loss.right)) / k
  weighted.cv.loss = (sum(weighted.cv.loss.left) + sum(weighted.cv.loss.right)) / k
  constraint.cv.loss = mean(constraint.cv.loss)
  
  return(c(cv.loss, var.loss, R.squared, Res.error, estimate, weighted.cv.loss, weighted.estimate, constraint.cv.loss, constraint.estimate))
}
