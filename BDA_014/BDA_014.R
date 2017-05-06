#  BDA_014

# The Chi-square distribution

# Definition:
# Let Z1, ..., Zn be i.i.d. r.v.s ~ N(0, 1). Then
#   X = (Z1)^2 + ... + (Zn)^2
# is distributed Chi-squared with n degrees of freedom.

n_sim <- 10000
dof <- 3
Z <- sapply(X = seq(n_sim), FUN = function(x) rnorm(n = dof))
if (dof != 1) {
  X <- apply(X = Z, MARGIN = 2,
             FUN = function(col) sum(sapply(col, function(x) x^2)))
} else {
  if (dof == 1) {
    X <- sapply(X = Z, FUN = function(z) sum(z^2))
  } else {
    stop('dof must be an integer.')
  }
}
hist(X, breaks = 100, probability = TRUE,
     main = 'Chi-square distribution')
lines(seq(0, 20, 0.01), dgamma(x = seq(0, 20, 0.01), shape = dof/2, rate = 0.5))
