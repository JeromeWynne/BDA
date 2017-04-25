# BDA 2.11 (1)
# What this script does
# 1.  Generates 5000 samples from a Beta distribution
# 2.  Uses these samples as the parameter of a
#     Binomial distribution to estimate the sampling distribution.
# 3.  Estimates the posterior distribution by integrating
#     the sampling distribution over the observed values of
#     y.

# Prior th ~ Beta(4, 4)
# Ten Bernoulli trials
# Less than 3 successes
# What is the exact posterior density?

n_sim <- 5000
n_trials <- 10
n_max <- 2 # At most this many successes

# Prior distribution of parameter, p(th)
# We randomly generate parameters according to the prior.
# More probable parameters are generated more frequently.
prior_params <- rbeta(n = n_sim, shape1 = 4, shape2 = 4)
prior_params <- sort(prior_params, decreasing = FALSE)
jpeg('Prior.jpg')
hist(prior_params, breaks = 50, main = 'Prior', 
     xlab = 'theta', probability = TRUE)
dev.off()

# Sampling distribution, p(y|th)
# Using the parameters generated, we calculate the
# probability of y = k successes, given each of the
# parameters in turn. This probability is the probability
# of observing the data, given the parameter is a particular
# value.
dsampling <- matrix(0, nrow = n_trials + 1, ncol = 1)
for (k in c(0, seq(n_trials))) {
  dsampling[k + 1, ] <- sum(dbinom(x = k, size = n_trials,
                        prob = prior_params))
}
jpeg('Sampling.jpg')
plot(c(0, seq(n_trials)), dsampling, type = 'h',
     xlab = '# successes', ylab = 'Unscaled p(y|th)',
     main = 'Sampling')
dev.off()

# Posterior distribution
# Integrate p(y|theta)p(theta) 
# over the observed results.
# Our results state the number of successes
# is y e {0, 1, .., n_max}.
# Finally, we condition on the what's actually observed -
# we sum 
# p(observation given parameter value)p(param_value)
# over all parameter values.
# The result is proportional to the probability that the
# parameter value is the population parameter, given the
# observed data.

dposterior <- matrix(0, nrow = n_sim)
for (k in c(0, seq(n_max))) {
  dposterior <- dposterior + 
                prior_params*dbinom(x = k, size = n_trials,
                                    prob = prior_params)
}
jpeg('Posterior.jpg')
plot(prior_params, dposterior, xlab = 'theta', 
     ylab = 'p(theta|y)', main = 'Posterior')
dev.off()
