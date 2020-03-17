# Compare results between Original CV / Quantile CV / My Window Selection
# SRD is assumed

## Need to run "mywindow.R" and "EconCV.R" beforehand !!!

set.seed(55555)

# Total number of data sets to be tested
n_rep = 5000  
# Selected bandwidths will be stored here
cv.windows = rep(NA, n_rep)
quantile.windows = rep(NA, n_rep)
my.windows = rep(NA, n_rep)

for (i in c(1:n_rep)){
n_sample = 200
min_x = 0
max_x = 10
cutoff = 5
window = 0.5  # True window
true_effect = 1

assign_var = sort(rnorm(n_sample, mean=5, sd=1.5))
assign_var = ifelse(assign_var<0, 0,
                      ifelse(assign_var > 100, 100, assign_var))
H = ifelse(assign_var < (cutoff-window), 4/4.5 * assign_var,
              ifelse(assign_var > (cutoff+window), 4/4.5 * (assign_var-(cutoff+window)) + 4, 4))
H = H + rnorm(n_sample, mean=0, sd=0.25)

observed_outcome = ifelse(assign_var < cutoff, 0.4*assign_var+1, 0.2*assign_var+3)
observed_outcome = observed_outcome + H + rnorm(n_sample, mean=0, sd=1)

D = (assign_var >= cutoff)  # D: Treatment indicator
W = D                       # W: Threshold indicator

data = data.frame(assign_var, D, W, observed_outcome)
centered_data = data
centered_data["assign_var"] = centered_data["assign_var"] - cutoff    # Centering around a threshold

control.data = centered_data[(data["W"]==FALSE),]
control.size = dim(control.data)[1]
control.quantile = control.data[c((control.size/2) : control.size),]  # 50%-quantile

treat.data = centered_data[(data["W"]==TRUE),]
treat.size = dim(treat.data)[1]
treat.quantile = treat.data[c((treat.size/2) : treat.size),]  # 50%-quantile

quantile_data = rbind(control.quantile, treat.quantile)


w_pool = seq(from=0.2, to=1, by=0.05) # Bandwidth pool
n_pool = length(w_pool)

my_loss = rep(NA, n_pool)
my_bias = rep(NA, n_pool)
my_sigma = rep(NA, n_pool)
my_estimate = rep(NA, n_pool)

quantile.cv_loss = rep(NA, n_pool)
cv_loss = rep(NA, n_pool)

for (iter in c(1:n_pool)){
  w = w_pool[iter]

  index = which(((assign_var >= (cutoff-w)) * (assign_var <= (cutoff+w))) == TRUE)
  double.window = length(index)
  
  window_data = centered_data[index,]
  
  # Quantile Cross Validation Approach
  quantile.cv.res = Original.cv(quantile_data, w)
  quantile.cv_loss[iter] = quantile.cv.res

  # Original Cross Validation Approach
  cv.res = Original.cv(centered_data, w)
  cv_loss[iter] = cv.res
  
  # Alternative Window Selection
  my.res = my.window(window_data, w, min_x, max_x, lambda=5.5)
  my_loss[iter] = my.res[1]
  my_bias[iter] = my.res[2]
  my_sigma[iter] = my.res[3]
  my_estimate[iter] = my.res[4]
  
}
# Handling NaN exceptions
na.cv = sum(is.na(cv_loss))
na.quantile = sum(is.na(quantile.cv_loss))
na.my = sum(is.na(my_loss))

m.quantile = which(quantile.cv_loss[!is.na(quantile.cv_loss)] == min(quantile.cv_loss[!is.na(quantile.cv_loss)]))
m.cv = which(cv_loss[!is.na(cv_loss)] == min(cv_loss[!is.na(cv_loss)]))
m.mywindow = which(my_loss[!is.na(my_loss)] == min(my_loss[!is.na(my_loss)]))

cv.windows[i] = w_pool[m.cv+na.cv]
quantile.windows[i] = w_pool[m.quantile+na.quantile]
my.windows[i] = w_pool[m.mywindow+na.my]

}
library(wesanderson)
colors = wes_palette(name="GrandBudapest2", n=4)[c(1,3,4)]

plot(ux, w.ux.cv, type="l", col=colors[2], lwd=2, xaxt='n', xlab="bandwidth", ylab="frequency")
axis(side=1, at=seq(0.2, 1, 0.1))
lines(ux, w.ux, col=colors[1], lwd=2)
lines(ux, w.ux.quantile, col=colors[3], lwd=2)
points(ux, w.ux, pch=19, cex=0.7, col=colors[1])
points(ux, w.ux.cv, pch=17, cex=0.7, col=colors[2])
points(ux, w.ux.quantile, pch=15, cex=0.7, col=colors[3])

legend(0.2, 2000, legend=c("Alternative Approach", "Cross Validation", "Quantile Cross Validation"), col=colors, cex=0.7, pch=c(19, 17, 15), lwd=2, text.font=2, box.lty=0)


