# An Exemplary Treatment Assignment Probability and Conditional Expectations of the Potential/Observed Outcomes for SRD and FRD
cutoff = 60 # cutoff is set to be 60
# Treatment Assignment Probability Function in SRD design
# X varies from 0 to 100
x_0 = seq(from=0, to=cutoff, by=0.1) 
x_1 = seq(from=cutoff, to=100, by=0.1)
x = seq(from=0, to=100, by=0.1)

# Function values in SRD design
y_srd_0 = rep(0, length(x_0))
y_srd_1 = rep(1, length(x_1))

# The graph of Pr(T=1|X=x) in SRD design
plot(1, type="n", xlab="Assignment Variable (X)", ylab="Pr(T=1| X=x)", xlim=c(0, 100), ylim=c(0, 1))
lines(x_0, y_srd_0, lwd=1)
lines(x_1, y_srd_1, lwd=1)
abline(v=cutoff, lty="dotted")
points(60, 0, pch=21, bg='white')
points(60, 1, pch=19)

# Treatment Assignment Probability Function in FRD design
# Function values in FRD design
y_frd_0 = (0.3/3600)*x_0^2
y_frd_1 = 1 - (0.3/1600)*(x_1 - 100)^2

# The graph of Pr(T=1|X=x) in FRD design
plot(1, type="n", xlab="Assignment Variable (X)", ylab="Pr(T=1| X=x)", xlim=c(0, 100), ylim=c(0, 1))
lines(x_0, y_frd_0)
lines(x_1, y_frd_1)
abline(v=cutoff, lty="dotted")
points(60, y_frd_0[601], pch=21, bg='white')
points(60, y_frd_1[1], pch=19)

# E[Y(1)|X=x], and E[Y(0)|X=x] in a whole domain
Y_1 = (50/(100^0.3))*(x^0.3)
Y_0 = (25/(30*50*70))*(x-50)*(x-30)*(x-70)+25

# The graph of E[Y|X=x] in SRD design
plot(1, type="n", xlab="Assignment Variable (X)", ylab="Conditional Expectation", xlim=c(0, 100), ylim=c(0, 50))
mu_x_0 = (50/(100^0.3))*(x_0^0.3)*y_srd_0 + ((25/(30*50*70))*(x_0-50)*(x_0-30)*(x_0-70)+25)*(1-y_srd_0)
mu_x_1 = (50/(100^0.3))*(x_1^0.3)*y_srd_1 + ((25/(30*50*70))*(x_1-50)*(x_1-30)*(x_1-70)+25)*(1-y_srd_1)
lines(x_0, mu_x_0, lwd=1)
lines(x_1, mu_x_1, lwd=1)
abline(v=cutoff, lty="dotted")

# The graphs of E[Y(1)|X=x], and E[Y(0)|X=x] are drawn with dotted curves (Counterfactuals in SRD design)
lines(x_1, Y_0[601:1001], lty='dotted', lwd=1)
lines(x_0, Y_1[1:601], lty='dotted', lwd=1)

points(60, mu_x_0[601], pch=21, bg='white')
points(60, mu_x_1[1], pch=19)

# The graph of E[Y|X=x] in FRD design
plot(1, type="n", xlab="Assignment Variable (X)", ylab="Conditional Expectation", xlim=c(0, 100), ylim=c(0, 50))
mu_x_0 = (50/(100^0.3))*(x_0^0.3)*y_frd_0 + ((25/(30*50*70))*(x_0-50)*(x_0-30)*(x_0-70)+25)*(1-y_frd_0)
mu_x_1 = (50/(100^0.3))*(x_1^0.3)*y_frd_1 + ((25/(30*50*70))*(x_1-50)*(x_1-30)*(x_1-70)+25)*(1-y_frd_1)
lines(x_0, mu_x_0, lwd=1)
lines(x_1, mu_x_1, lwd=1)
abline(v=cutoff, lty="dotted")

points(60, mu_x_0[601], pch=21, bg='white')
points(60, mu_x_1[1], pch=19)

# The graphs of E[Y(1)|X=x], and E[Y(0)|X=x] are drawn with dotted curves
lines(x, Y_0, lty='dotted')
lines(x, Y_1, lty='dotted')

