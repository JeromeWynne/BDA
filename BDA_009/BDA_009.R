# BDA_009 - Modelling the rate of airline fatalities

library(readxl)
setwd('C:/Users/admin/Documents/Personal/Machine Learning/BDA/BDA_009')
data <- read_excel("./worldwide_airline_fatalities.xlsx")

# Death rate is passenger deaths per 100 million miles flown

# (a)
# Assume that the numbers of fatal accidents in each year are independent
# with a Poisson(theta) distribution. Set a prior distribution for theta
# and determine the posterior distribution based on the data from 1976
# through 1985. Under this model, give a 95% predictive interval for the
# number of fatal accidents in 1986.

# Theta -> Expected fatal accidents per year

# We're estimating p(theta|observations)

# Let y be the the number of fatal accidents in each year,
# so that the y1, ...,ym are the observed fatal accidents in years
# 1976-1985.

# The sampling distribution is therefore 
# y|theta ~ Poisson(theta)

# We'll assume that the prior distribution for theta is Uniform over
# 0 - 100: That is, it is assumed that the expected fatal accidents per year
# could not possibly be higher than 100 or lower than zero, and that
# all values between 0 and 100 are equally likely.

step_size <- .1
th <- seq(10, 50, step_size)

# Prior probability density
prior_pd <- dunif(x = th, min = 0, max = 100)

# Sample probabilities (p(y|theta) ~ Poisson(theta))
observations <- data$Fatal_accidents
sample_pd  <- sapply(X = th, FUN = function(lam) dpois(x = observations, lambda = lam))
# Each column corresponds to a theta value, each row to an observation.
# Element i,j is the probability of observation i given the expected number of fatal
# accidents theta_j.

# Unnormalized posterior probabilities
unnorm_posterior_pd <- apply(sample_pd, 2, prod)
posterior_pd <- unnorm_posterior_pd/sum(unnorm_posterior_pd*step_size)
plot(th, posterior_pd, type = 'l')

# A 95% predictive interval for fatal accidents in 1986
# p(y_pred|theta, y_obs) ~ Poisson(theta|y_obs)
# We sample from the posterior
n_samples <- 10000
th_post <- sample(x = th, size = n_samples, prob = posterior_pd, replace = TRUE)
posterior_predictive <- sapply(th_post, FUN = function(lam) rpois(n = 1, lambda = lam))
hist(posterior_predictive, breaks = 50)
pred_interval <- quantile(x = posterior_predictive, probs = c(.025, .975))
print(c('Posterior predictive interval is', pred_interval), quote = FALSE)
