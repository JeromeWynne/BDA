# Gelman 2.11 Ex 9

# The prior distribution for theta is Beta with mean .6 and sd .3.
# This prior is supposed to represent the proportion of Californians
# that support the death penalty.


# (a) Determine the parameters alpha and beta of this prior

#     We know that the prior distribution must integrate over the support
#     of theta to give 1. 
#     The support of theta is [0, 1].
#     We can set up a system of equations in alpha and beta using
#     the given values for the prior expectation and standard deviation
E_th <- 0.6
sd_th <- 0.3

