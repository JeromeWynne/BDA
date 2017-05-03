# BDA_011

# Simple hierarchical modelling
library(foreign)
poll_data <- read.dta('./pew_research_center_june_elect_wknd_data.dta')
poll_data <- poll_data[poll_data$state != 'hawaii', ]

# Estimate the percentage of the population in each state
# (excluding Alaska and Hawaii) who label themselves as
# 'very liberal', replicating the procedure
# that was used in Section 2.7 to estimate cancer rates.

# Solution
#---------
# Let y_j be the number of polled people in a state that
# label themselves as 'very liberal'.
# Let theta_j be the underlying proportion of people in a state
# who label themselves as 'very liberal'.

# Assuming that the poll is unbiased then, 
#   y_j ~ Binomial(n_j, theta_j)
# Where n_j is the number of polled people in state j.
# This is our sampling distribution.
# It is binomial because if the poll is unbiased, then any given person
# has a probability of theta_j of labelling themselves as 'very liberal'.

# Before we can do anything, we need to count the number of polled people
# in each state, and the number of people in each state that identified as
# being 'very liberal'.

poll_sizes <- aggregate(x = poll_data$state, 
                        by = list(State = poll_data$state), 
                        FUN = length)
obs_sizes <- aggregate(x = (poll_data$ideo %in% c('very liberal', 'liberal')),
                       by = list(State = poll_data$state),
                       FUN = 'sum', na.rm = TRUE)
e_y <- mean(obs_sizes$x)
var_y <- var(obs_sizes$x)
e_n <- mean(poll_sizes$x)

# THESE PRIOR PARAMETERS ARE INCORRECT
# WHY?
a <- e_y
b <- e_n - e_y

# We'll use a uniform prior of the form Beta(1, 1) for all states
th <- seq(0, 1, length.out = 100)
prior_pd <- dbeta(x = th, shape1 = a, shape2 = b)
plot(th, prior_pd, type = 'l')

# Now we'll compute the sample probabilities for each state and theta value
# p(y|theta) = nCy theta^y * (1 - theta)^(n - y)
sample_p <- matrix(nrow = length(th), ncol = 49)
state_labels <- sort(unique(poll_data$state))
j = 1
for (state in state_labels) {
  state_n <- poll_sizes$x[poll_sizes$State == state]
  state_y <- obs_sizes$x[obs_sizes$State == state]
  sample_p[ , j] <- sapply(X = th,
                           FUN = function(theta)
                                          dbinom(x = state_y,
                                                size = state_n,
                                                prob = theta))
  j <- j + 1
}

# Using these we can get hold of the state posteriors
# p(theta|obs) oc P(obs|theta)p(theta)
posterior_pd <- apply(X = sample_p,
                          MARGIN = 2,
                          FUN = function(sample_prob)
                                  sample_prob*prior_pd)
posterior_pd <- data.frame(posterior_pd)
names(posterior_pd) <- state_labels
posterior_mean <- matrix(apply(posterior_pd, MARGIN = 2,
                         FUN = function(col) sum(col*th)/sum(col)))

# Now we compare the posterior distributions against the proportion
# of people that voted Obama
election_data <- read.csv('2008ElectionResult.csv')
election_data <- election_data[!election_data$state %in% c('Alaska', 'Hawaii'), ]
prop_obama <- election_data$vote_Obama_pct/100

# (a) Graph the proportion liberal in each state vs. Obaa vote share
plot(obs_sizes$x/poll_sizes$x, prop_obama,
     xlab = 'Proportion of state poll identifying as liberal',
     ylab = 'Obamas share of vote in state in 2008',
     pch = 21, bg = 'cornflowerblue', col = 'white',
     cex = 1.2,
     main = "Obama`s share of the vote by \n proportion of state identifying as liberal")

# (b) Graph of the Bayes posterior mean vs. Obama vote share
plot(x = posterior_mean, y = prop_obama)
