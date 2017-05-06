# BDA_015

# Multiparameter models
# Normal Data with a Noninformative Prior Distribution

# Often we'll have to deal with models containing multiple unknowns.
# It's usually the case that in these models we're only really 
# interested in the posterior distribution of one parameter, known
# as the marginal posterior distribution of that parameter.

# To obtain the marginal posterior it's first necessary to 
# model the joint posterior distribution over all unknowns.
# We can then integrate over the other unknowns to get the desired
# marginal posterior.

# The computational equivalent of this procedure is to draw many
# samples from the joint posterior, look at the parameter of interest,
# and build up a frequency distribution for the parameter.