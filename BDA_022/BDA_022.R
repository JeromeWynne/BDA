# BDA_022: Multiparameter Normal Model
library(plot3D)
set.seed(1)

# 1. Generate some normally distributed data
true_mu  <- 17.8
true_sd  <- 10
n_points <- 3
y        <- rnorm(n_points, mean = true_mu, sd = true_sd)

# 2. Define a grid of parameter values
step_size_1 <- 0.5
step_size_2 <- 0.5
th_1 <- seq(0.1, 30, step_size_1)
th_2 <- seq(0.1, 50, step_size_2)

# 3. Define functions for the prior and likelihood
dprior      <- function(th_1, th_2, mu, sig) {
                exp(-(th_1 - mu)^2/(2*sig^2))/sqrt(2*pi*sig^2)
               }
dlikelihood <- function(y, th_1, th_2) {
                    exp( -sum((y - th_1)^2)/(2*th_2^2) ) / (2*pi*th_2^2)^(length(y)/2)
              }

# 4. Select prior hyperparameters
mu  <- 15
sig <- 5

# 5. Get prior, likelihood, and posterior densities
prior_density      <- sapply(th_1, function(t1) sapply(th_2, function(t2) dprior(t1, t2, mu, sig)))
likelihood_density <- sapply(th_1, function(t1) sapply(th_2, function(t2) dlikelihood(y, t1, t2)))
posterior_density  <- prior_density * likelihood_density
posterior_density  <- posterior_density/sum(posterior_density*step_size_1*step_size_2)


# 5. Plot the prior
pdf('multiparameter_bayes.pdf', width = 7, height = 3, pointsize = 10)
par(mfrow = c(1, 3))
persp3D(z = prior_density, x = th_2, y = th_1, 
         border = NA, phi = 30, theta = -60, curtain = FALSE,
         facets = TRUE, col = ramp.col(c("purple", "yellow")),
         colkey = FALSE, shade = 0.4, contour = TRUE,
         zlim = c(-0.5, 0.5),
         ylab = 'Est. mean', xlab = 'Est. standard dev.',
         zlab = 'Probability density', main = 'Prior')

# 6. Plot the likelihood
persp3D(z = likelihood_density, x = th_2, y = th_1, 
        border = NA, phi = 30, theta = -60, curtain = FALSE,
        facets = TRUE, col = ramp.col(c("purple", "yellow")),
        colkey = FALSE, shade = 0.4, contour = TRUE,
        zlim = c(-5e-5, 5e-5),
        ylab = 'Est. mean', xlab = 'Est. standard dev.',
        zlab = 'Probability density', main = 'Likelihood')

# 7. Plot the posterior
persp3D(z = posterior_density, x = th_2, y = th_1, 
        border = NA, phi = 30, theta = -60, curtain = FALSE,
        facets = TRUE, col = ramp.col(c("purple", "yellow")),
        colkey = FALSE, shade = 0.4, contour = TRUE,
        zlim = c(-0.01, 0.01),
        ylab = 'Est. mean', xlab = 'Est. standard dev.',
        zlab = 'Probability density', main = 'Posterior')
dev.off()

