# BDA_008
# Gelman Ex 2.11 11
# Computing with a nonconjugate single-parameter model

# Y is an r.v. such that Y ~ Cauchy(theta, scale)
# p(y|theta) oc 1/(1 + (y - theta)^2)
# where theta is unkown and scale = 1

# We are asked to assue the prior distribution
# for theta is Uniform on [0, 100]

# We observe that (y1, ..., y5) = (43, 44, 45, 46.5, 47.5)

# (a) Compute the unnormalized posterior density function
set.seed(1)

# Begin by taking theta samples from the prior
m <- 50000
step_size <- 100/m
theta <- seq(0, 100, length.out = m)
prior_pd <- dunif(x = prior_samples, min = 0, max = 100)

# Next we need the sampling distribution, p(y|theta)
# We are told in the question that this is a Cauchy distribution,
# therefore
observations <- c(43, 44, 45, 46.5, 47.5)
sampling_pd <- sapply(X = theta, FUN = function(th) 
                                          prod(dcauchy(x = observations,
                                                       location = th,
                                                       scale = 1)) )

# Unnormalized posterior: p(theta|y) oc p(theta)p(y|theta)
unnorm_posterior_pd <- prior_pd*sampling_pd
plot(theta, unnorm_posterior_pd,
     type = 'l', xlab = 'Theta', ylab = 'oc p(theta|sample)',
     main = 'Unnormalized posterior distribution')

# Normalized posterior density function using the grid approximation
unnorm_integral <- sum(unnorm_posterior_pd*step_size)
posterior_pd <- unnorm_posterior_pd/unnorm_integral

plot(theta, posterior_pd,
     type = 'l', xlab = 'Theta', ylab = 'p(theta|sample)',
     main = 'Posterior distribution')
abline(v = sum(posterior_pd*prior_samples*step_size), lty = 2)


# (b) Sample 1000 draws of theta from the posterior density and plot
#     a histogram of the draws.
sample_draws <- 1000
posterior_samples <- sample(theta, sample_draws, prob = posterior_pd)
hist(posterior_samples, breaks = 100)


# (c) Use these samples from the posterior to obtain
#     1000 samples from the predictive distribution of a future
#     observation y6.
# We know that Y is distributed Cauchy, so it's just a case of
# plugging these samples into the Cauchy distribution with the original 
# scale parameter, 1.

predictive_samples <- sapply(X = posterior_samples,
                             FUN = function(th) 
                                   rcauchy(n = 1,location = th, scale = 1))
hist(predictive_samples, xlim = c(30, 60), breaks = 1000, prob = TRUE,
     xlab = 'Future sample value')
