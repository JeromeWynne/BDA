# Gelman 2.11 Ex 9

# The prior distribution for theta is Beta with mean .6 and sd .3.
# This prior is supposed to represent the proportion of Californians
# that support the death penalty.


# (a) Determine the parameters alpha and beta of this prior

# From given expressions for the expectation and variance in terms
# of the Beta parameters, I found
exp_th <- c(0.6, 0.2)
sd_th <- c(0.3, 0.3)

a <- exp_th*((1/exp_th - 1)/(sd_th^2/exp_th^2) - 1)
b <- a/exp_th - a

th <- seq(0, 1, 0.005)
plot(th, dbeta(x = th, shape1 = a[1], shape2 = b[1]),
     type = 'l', xlab = 'theta', ylab = 'p(theta)',
     main = 'Prior', col = 'red')
lines(th, dbeta(x = th, shape1 = a[2], shape2 = b[2]),
                type = 'l', col = 'blue')
legend('topleft', legend = c(paste('exp_th = ', exp_th[1]),
                             paste('exp_th = ', exp_th[2])),
       col = c('red', 'blue'), lty = c(1, 1))

n = 1000
k = 650

plot(th, dbeta(x = th, shape1 = a[1] + k, shape2 = b[1] + n - k),
     type = 'l', xlab = 'theta', ylab = 'p(theta)',
     main = 'Posterior', col = 'red')
lines(th, dbeta(x = th, shape1 = a[2] + k, shape2 = b[2] + n - k),
      type = 'l', col = 'blue')
legend('topleft', legend = c(paste('exp_th = ', exp_th[1]),
                             paste('exp_th = ', exp_th[2])),
       col = c('red', 'blue'), lty = c(1, 1))

