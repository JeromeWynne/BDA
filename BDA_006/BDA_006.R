# BDA 006

# Simple example of Bayesian inference using a Binomial sampling distribution
# and a Beta conjugate prior distribution on the Binomial parameter.
# Demonstrates that the posterior is Beta(a + k, b + n - k) by sampling
# from a Beta prior, then combining this with a Binomial sampling distribution.

# Inspired by Blitzstein 8.4

# We have a coin. We want to know what the probability $theta$
# is that this coin lands Heads after being flipped.
# To do this, we collect some data - we flip the coin n times,
# and record that it landed Heads-up k of those times.

n_sim <- 10000
n_trials <- 10
obs_k <- 4
prior_successes <- 0
prior_failures <- 0
plot_title <- 'Uninformed prior'
  
# Our knowledge of $theta$ before we run this test is limited -
# we know that it is a number between 0 and 1, but, for all intents and purposes,
# it could equally well be any number in that range. We represent this in
# the prior distribution of theta using a Uniform distribution (i.e. Beta
# with a = 1, b = 1)
a <- 1 + prior_successes
b <- 1 + prior_failures

# Define the prior distribution
prior_samples <- seq(0, 1, length.out = n_sim)
prior_distribution <- dbeta(x = prior_samples, shape1 = a, shape2 = b)

# The sampling distribution - p(X = k | theta = th)
# X is Binomially distributed given each value of theta
sampling_distribution <- dbinom(x = obs_k,
                                size = n_trials,
                                prob = prior_samples)
# Each element's value is the probability of X = k, given the associated
# value of theta in prior_samples.

# Now we can combine these using an unnormalized form of Bayes theorem.
# p(th | X = k) oc P(X = k | th) p(th).
# i.e. the product of the sampling distribution with the prior probabilities.
# The result is a vector of probabilities for each prior sample.
posterior_distribution <- sampling_distribution*prior_distribution

# We can then normalize this distribution afterward,
posterior_distribution <- posterior_distribution/sum(posterior_distribution/n_sim)

# Let's look at the result
png(paste(plot_title, '.png', sep = ""))
plot(prior_samples, posterior_distribution, 
     type = 'l', pch = 20, xlab = 'Theta', ylab = 'Probability density',
     main = plot_title, col = rgb(1, 0, 0, 0.8), lwd = 3)
lines(prior_samples, prior_distribution,
      col = rgb(0, 0, 1, 0.5), type = 'l', lwd = 3)
lines(prior_samples, dbeta(x = prior_samples,
                           shape1 = a + obs_k,
                           shape2 = b + n_trials - obs_k),
      col = rgb(0, 1, 0, 0.8), type = 'l', lwd = 3, lty = 2)
legend('topright', legend = c('Prior', 'Posterior', 'Beta(a + k, b + n - k)'),
       lty = c(1, 1, 2), col = c('blue', 'red', 'green'))
dev.off()