# Cross Validation Method by Ludwig and Miller (2005), and Imbens and Lemieux (2008)

# Input
# data : data.frame [X, D, Y] with (X: "assign_var", D: "D", Y: "observed_outcome")
#                                   X contains sorted, centered, and all the assignment variable (quantile data if one wants to apply quantile cross validation method)
# window : w (numeric)

# Output
# cv.loss : loss (numeric) (Cross Validation loss for a given window and data)
Original.cv = function(data, window){ 
  left.data = data[(data["D"]==FALSE),]
  right.data = data[(data["D"]==TRUE),]
  left.size = dim(left.data)[1]
  right.size = dim(right.data)[1]
  
  # Corner Case Handling
  if (left.size<3){
    return("Too few data on the left side!")} 
  if (right.size<3){
    return("Too few data on the right side!")}
  
  # Columns to store the deviations
  left.dev = rep(NA, left.size-2)
  right.dev = rep(NA, right.size-2)
  
  # Calculating left deviation (left.dev)
  for (i in c(3:left.size)){
    x_i = left.data["assign_var"][i,]
    local.index = (left.data["assign_var"][c(1:(i-1)),] >= (x_i - window))
    if (sum(local.index) < 2){
      left.dev[i-2] = 0} 
    else {
      local.index = which(local.index == TRUE)
      local.data = left.data[local.index,]
      local.model = lm(observed_outcome~assign_var, data=local.data)
      left.dev[i-2] = left.data["observed_outcome"][i,] - predict(local.model, data.frame(assign_var = x_i))}
  }
  
  # Calculating right deviation (right.dev)
  for (i in c(1:(right.size-2))){
    x_i = right.data["assign_var"][i,]
    local.index = (right.data["assign_var"][c((i+1):right.size),] <= (x_i + window))
    if (sum(local.index) < 2){
      right.dev[i] = 0}
    else {
      local.index = which(local.index == TRUE) + i
      local.data = right.data[local.index,]
      local.model = lm(observed_outcome~assign_var, data=local.data)
      right.dev[i] = right.data["observed_outcome"][i,] - predict(local.model, data.frame(assign_var = x_i))}
  }
  
  # Cross Validation Loss
  cv.loss = (sum(left.dev^2) + sum(right.dev^2)) / (left.size+right.size-4)
  return(cv.loss)
}
