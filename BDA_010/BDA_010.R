# BDA_010

# Censored and uncensored data in the exponential model:
# (a) Suppose y|theta is exponentially distributed with rate theta,
#     and the marginal distribution of theta is Gamma(alpha, beta). Suppose
#     we observe that y >= 10, but do not observe the exact value of y.
#     What is the posterior distribution, p(theta|y >= 100), as a function of
#     alpha and beta? Write down the posterior mean and vairance of theta.

# Analytical solution was Gamma(alpha, beta + 100)
# Took E[theta|y] = alpha/(beta + 100)
#      Var(theta|y) = alpha/(beta + 100)^2

# Prior on Exponential parameter was Gamma(alpha, beta)
# Take alpha = 2, beta = 5 
alpha <- 2
beta <- 5
n_pts <- 5000
th <- seq(0, 1, length.out = n_pts)
prior_pd <- dgamma(x = th, shape = alpha, rate = beta)

# Sampling distribution was Exponential
# Example of an Exponentially distributed parameter: times between
# emails, assuming a constant probability of receiving an email.
# Observation was y >= 100
# Analytically found that p(y|theta) was exp(-100*theta)
# by integrating theta*exp(-theta*y) between [100, inf].
sampling_pd <- sapply(X = th, FUN = function(theta) exp(-100*theta))

# Posterior distribution
unnorm_posterior_pd <- sampling_pd*prior_pd # Probability of each theta value given observation
posterior_pd <- unnorm_posterior_pd/sum(unnorm_posterior_pd*1/n_pts)


# Plot it
png('Analytic_vs_computational_posterior.png')
plot(th, posterior_pd, type = 'l', lwd = 2,
     xlab = 'Theta', ylab = 'Posterior probability density',
     main = 'Posterior distribution of parameter')
lines(th, dgamma(x = th, shape = alpha, rate = beta + 100),
     col = 'red', type = 'l', lty = 3, lwd = 2)

legend('topright', legend = c('Computational solution', 'Analytic solution'),
       col = c('red', 'black'), lty = c(1, 3))
dev.off()