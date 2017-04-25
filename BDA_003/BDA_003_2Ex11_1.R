# BDA 2.11 (1)

# Prior th ~ Beta(4, 4)
# Ten Bernoulli trials
# Less than 3 successes
# What is the exact posterior density?

n_sim <- 5000
n_trials <- 10
n_max <- 2 # At most this many successes

# Prior distribution of parameter, p(th)
prior_params <- rbeta(n = n_sim, shape1 = 4, shape2 = 4)
prior_params <- sort(prior_params, decreasing = FALSE)
jpeg('Prior.jpg')
hist(prior_params, breaks = 50, main = 'Prior', 
     xlab = 'theta', probability = TRUE)
dev.off()

# Sampling distribution, p(y|th)
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
