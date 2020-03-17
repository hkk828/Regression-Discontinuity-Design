# An example to show that the randomized experiment can solve the confounding problem

# Example Setting
# 50 units each from Group A and Group B
# Group A: Normally distributed with mean 5, variance 1
# Group B: Normally distributed with mean 10, variance 1
# Treatment effect = Normally distributed with mean 5, variance (0.5)^2

set.seed(5) # For the sake of the reproducibility
group_A = rnorm(50, mean=5, sd=1)
group_B = rnorm(50, mean=10, sd=1)
# treatment effect is pre-determined for each unit with some error
effect = rnorm(100, mean=5, sd=0.5) # 1~50 for A, 51~100 for B
# control effect as well
control_effect = rnorm(100, mean=0, sd=0.5) # 1~50 for A, 51~100 for B

### 1st Experiment with Confounding
# All units of group A receives the treatment
treat_conf = group_A + effect[1:50]
# All units of group B receives the control treatment
control_conf = group_B + control_effect[51:100]

# Mean of the treatment group and the control group in the 1st experiment
mu_treat_conf = mean(treat_conf)
mu_control_conf = mean(control_conf)
# Observed average causal effect of 1st experiment
observed_effect_conf = mu_treat_conf - mu_control_conf
print(observed_effect_conf)

### 2nd Experiment with Randomization
# Randomly select 50 units
treat_ind = sort(sample(c(1:100), 50))
# Separating treatment group into the group A and B
treat_ind_A = treat_ind[treat_ind <= 50]
treat_ind_B = treat_ind[treat_ind > 50] - 50

# Apply treatment/control treatment
treat_rand = c(group_A[treat_ind_A], group_B[treat_ind_B]) + c(effect[treat_ind_A], effect[treat_ind_B + 50])
control_rand = c(group_A[-treat_ind_A], group_B[-treat_ind_B]) + c(control_effect[1:50][-treat_ind_A], control_effect[51:100][-treat_ind_B])

# Mean of the treatment group and the control group in the 2nd experiment
mu_treat_rand = mean(treat_rand)
mu_control_rand = mean(control_rand)
# Observed average causal effect of the 2nd experiment
observed_effect_rand = mu_treat_rand - mu_control_rand
print(observed_effect_rand)


### The results in box-plots
library(latex2exp)
# 1st Experiment with Confounding
df_conf = data.frame(cbind(treat_conf, control_conf))
boxplot(df_conf, medlty=0, names=c("Treatment", "Control"), xlab=TeX(sprintf("$\\hat{\\tau}_{Exp}=%.3f$", observed_effect_conf)), ylab="Confounded Experiment")
points(1:2, c(mu_treat_conf, mu_control_conf), col='red', lwd=2)
text(1:2, c(mu_treat_conf, mu_control_conf)+0.3, c(round(mu_treat_conf,3), round(mu_control_conf,3)))

# 2nd Experiment with Randomization
df_rand = data.frame(cbind(treat_rand, control_rand))
boxplot(df_rand, medlty=0, names=c("Treatment", "Control"), xlab=TeX(sprintf("$\\hat{\\tau}_{RCE}=%.3f$", observed_effect_rand)), ylab="Randomized Experiment")
points(1:2, c(mu_treat_rand, mu_control_rand), col='red', lwd=2)
text(1:2, c(mu_treat_rand, mu_control_rand)+0.8, c(format(round(mu_treat_rand,3), nsmall=3), round(mu_control_rand,3)))

### Observational Study (Mathcing on groups)
# Calculate average causal effects separately for each group
# For the matching example, the same data from the 2nd experiment will be used
# group_A = treat_obs_A + control_obs_A
treat_obs_A = group_A[treat_ind_A] + effect[treat_ind_A]
control_obs_A = group_A[-treat_ind_A] + control_effect[1:50][-treat_ind_A]

# Mean of the treatment group and the control group (Only in group A)
mu_treat_obs_A = mean(treat_obs_A)
mu_control_obs_A = mean(control_obs_A)

# Observed average causal effect of group A (using matching method)
observed_effect_obs_A = mu_treat_obs_A - mu_control_obs_A
print(observed_effect_obs_A)

# group_B = treat_obs_B + control_obs_B
treat_obs_B = group_B[treat_ind_B] + effect[treat_ind_B+50]
control_obs_B = group_B[-treat_ind_B] + control_effect[51:100][-treat_ind_B]

# Mean of the treatment group and the control group (Only in group B)
mu_treat_obs_B = mean(treat_obs_B)
mu_control_obs_B = mean(control_obs_B)

# Observed average causal effect of group B (using mathcing method)
observed_effect_obs_B = mu_treat_obs_B - mu_control_obs_B
print(observed_effect_obs_B)

# Box-Plots for the Observational Study (Matching)
# Group A Box-plot
df_obs_A = data.frame(cbind(treat_obs_A, control_obs_A))
df_obs_A[24:27,"control_obs_A"] = NA # Fill missing rows with NA
boxplot(df_obs_A, medlty=0, names=c("Treatment", "Control"), xlab=TeX(sprintf("$\\hat{\\tau}_{Obs, A}=%.3f$", observed_effect_obs_A)), ylab="Group A")
points(1:2, c(mu_treat_obs_A, mu_control_obs_A), col='red', lwd=2)
text(1:2, c(mu_treat_obs_A, mu_control_obs_A)+0.4, c(round(mu_treat_obs_A,3), round(mu_control_obs_A,3)))

# Group B Box-plot
df_obs_B = data.frame(cbind(treat_obs_B, control_obs_B))
df_obs_B[24:27, "treat_obs_B"] = NA # Fill missing rows with NA
boxplot(df_obs_B, medlty=0, names=c("Treatment", "Control"), xlab=TeX(sprintf("$\\hat{\\tau}_{Obs, B}=%.3f$", observed_effect_obs_B)), ylab="Group B")
points(1:2, c(mu_treat_obs_B, mu_control_obs_B), col='red', lwd=2)
text(1:2, c(mu_treat_obs_B, mu_control_obs_B)+0.35, c(round(mu_treat_obs_B,3), format(round(mu_control_obs_B,3), nsmall=3)))