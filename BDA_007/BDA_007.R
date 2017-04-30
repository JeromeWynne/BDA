# BDA__007
# Investigating the Gamma function
layout(matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE))
x <- seq(0, 10, 0.1)

a <- c(1, 2, 1, 2)
lambda <- c(1, 1, 2, 2)

for (i in seq(4)) {
  f <- dgamma(x = x, shape = a[i], rate = lambda[i])
  plot(x, f, type = 'l', pch = 20)
  legend('topright', legend = c(paste('a = ', a[i], ',',
                                      'lambda =', lambda[i])), lty = 1)
}