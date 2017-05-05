# BDA_013

# Comparing the conjugate priors for the mean and variance of
# an r.v. that is Normally distributed.

# The conjugate prior for the mean of a Normal r.v. with known variance
# is the Normal distribution, parameterized by mu_zero and tau_zero.
theta <- seq(0, 10, 0.05) # Possible values of mean
mu_zero <- 5              # Average of prior mean estimates
tau_zero <- 1             # s.d. of prior mean estimates

# The conjugate prior for the variance of a Normal r.v. with known mean
# is the Inverse Gamma distribution, parameterized by alpha and beta.
sigma_zero <- 2                 # ?
nu_zero <- 2                    # ?
# By simulation
chisq_vals <- rchisq(n = 50000, df = nu_zero)
dist <- (sigma_zero^2)*nu_zero/chisq_vals
dist <- dist[dist < 500]
hist(dist, breaks = 5000, xlim = c(0, 10), probability = TRUE,
     main = 'Inverse Chi-square Distribution')
legend('topright', legend = c('Hist: Computational sampling', 'Curve: Analytical solution'))

# By a transformation of variables
x <- seq(0, 10, 0.005)
sigma_sq <- (nu_zero*sigma_zero^2)/x
jacob <- x/sigma_sq
lines(sigma_sq, jacob*dchisq(x = x, df = nu_zero), xlim = c(0, 10), type = 'l')

# Plots
plot(sigma_sq, ,
      lty = 2, type = 'l', lwd = 2)
lines(theta, dnorm(x = theta, mean = mu_zero, sd = tau_zero),
     xlab = 'Mean value', ylab = 'Prior probability density',
     lty = 1, type = 'l', lwd = 2)
