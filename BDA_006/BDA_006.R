# BDA 006

# Simple example of Bayesian inference using a Binomial sampling distribution
# and a Beta conjugate prior distribution on the Binomial parameter.

# Inspired by Blitzstein 8.4

# We have a coin. We want to know what the probability $theta$
# is that this coin lands Heads after being flipped.
# To do this, we collect some data - we flip the coin n times,
# and record that it landed Heads-up k of those times.

# Our knowledge of $theta$ before we run this test is limited -
# we know that it is a number between 0 and 1, but, for all intents and purposes,
# it could equally well be any number in that range. We represent this in
# the prior distribution of theta using a Uniform distribution (i.e. Beta
# with a = 1, b = 1)

n_trials <- 10
observed_k <- 4

# Prior distribution
theta <- 