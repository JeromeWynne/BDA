# BDA_013

# Comparing the conjugate priors for the mean and variance of
# an r.v. that is Normally distributed.

# The conjugate prior for the mean of a Normal r.v. with known variance
# is the Normal distribution, parameterized by mu_zero and tau_zero.
theta <- seq(0, 10, 0.05) # Possible values of mean
mu_zero <- 5              # Average of prior mean estimates
tau_zero <- 1             # s.d. of prior mean estimates

