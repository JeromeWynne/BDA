# Gelman 2.11 Ex 9

# The prior distribution for theta is Beta with mean .6 and sd .3.
# This prior is supposed to represent the proportion of Californians
# that support the death penalty.


# (a) Determine the parameters alpha and beta of this prior

# From given expressions for the expectation and variance in terms
# of the Beta parameters, I found
exp_th <- 0.6
sd_th <- 0.3

a <- exp_th*((1/exp_th - 1)/(sd_th^2/exp_th^2) - 1)
b <- a/exp_th - a

th <- seq(0, 1, 0.005)
plot(th, dbeta(x = th, shape1 = a, shape2 = b),
     type = 'l', xlab = 'theta', ylab = 'p(theta)',
     main = 'Prior')

n = 1000
k = 650

plot(th, dbeta(x = th, shape1 = a + k, shape2 = b + n - k),
     type = 'l', xlab ='theta', ylab = 'p(theta|survey)',
     main = 'Posterior')

