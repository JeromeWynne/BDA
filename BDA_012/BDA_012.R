# BDA_012

# Gelman, The Single Parameter Normal Distribution

# We're estimating the mean of a distribution for which
# the observations are modelled as being Normally distributed.

# We have a Normal sampling distribution with known variance sigma,
# and observations 1, ..., m. 
# Our justification for this
# distribution is that the deviations from the mean are caused by
# a sum of a large number of r.v.s, meaning that we can
# apply the Central Limit Theorem.

# The prior distribution of the parameter is modelled as being Normal;
# we define the mean and variance of this distribution as mu_zero
# and tau_zero respectively.
n_pts <- 1000
th <- seq(1, 100, length.out = n_pts)
true_mean <- 60
sigma <- 5

mu_zero <- 50
tau_zero <- 20
prior_pd <- dnorm(x = th, mean = mu_zero, sd = tau_zero)

sigma <- 5
m <- 10
obs_y <- rnorm(n = m, mean = true_mean, sd = sigma)
sample_pd <- sapply(X = th, FUN = function(theta) prod(dnorm(x = obs_y,
                                                        mean = theta,
                                                        sd = sigma)))

posterior_pd <- prior_pd*sample_pd
posterior_pd <- posterior_pd/sum(posterior_pd*100/n_pts)
plot(th, posterior_pd, type = 'l', col = 'blue', lwd = 3)
posterior_sd <- sqrt(1/(1/tau_zero^2 + m/sigma^2))
posterior_mean <- (mu_zero/tau_zero^2) + (mean(obs_y)*m/sigma^2)/(1/posterior_sd^2)
lines(th, dnorm(x = th, mean = posterior_mean, sd = posterior_sd),
      col = 'red', lty = 2, lwd = 3)

