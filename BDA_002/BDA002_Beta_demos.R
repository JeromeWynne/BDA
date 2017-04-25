# Beta distribution demos

x <- seq(0, 1, 0.005)

# Standard Uniform
a <- 1
b <- 1
png('Beta_Uniform.png')
plot(x, dbeta(x = x, shape1 = a, shape2 = b), type='l',
     main = paste(
       'Beta(a =', a, ', b = ', b, ')'), ylab = 'p(x)')
dev.off()

# Weighted in favour of larger x
a <- 1
b <- 0.5
plot(x, dbeta(x = x, shape1 = a, shape2 = b), type='l',
     main = paste(
       'Beta(a =', a, ', b = ', b, ')'), ylab = 'p(x)')

# Weighted in favour of smaller x
# Standard Uniform
png('Beta_Skew_Right.png')
a <- 0.5
b <- 1
plot(x, dbeta(x = x, shape1 = a, shape2 = b), type='l',
     main = paste(
       'Beta(a =', a, ', b = ', b, ')'), ylab = 'p(x)')
dev.off()

# Symmetric about x = 1/2 - Concave
a <- 0.5
b <- 0.5
png('Beta_Symmetric_Concave.png')
plot(x, dbeta(x = x, shape1 = a, shape2 = b), type='l',
     main = paste(
       'Beta(a =', a, ', b = ', b, ')'))
dev.off()

# Symmetric about x = 1/2 - Convex
a <- 1.5
b <- 1.5
png('Beta_Symmetrix_Convex.png')
plot(x, dbeta(x = x, shape1 = a, shape2 = b), type='l',
     main = paste(
       'Beta(a =', a, ', b = ', b, ')'), ylab = 'p(x)')
dev.off()


# Binomial
a <- 10 # n
b <- 10 - 3 # n - k
png('Beta_Binomial.png')
plot(x, dbeta(x = x, shape1 = a, shape2 = b), type='l',
     main = paste(
       'Beta(a =', a, ', b = ', b, ')'), ylab = 'p(x)')
dev.off()