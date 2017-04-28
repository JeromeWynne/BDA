# BDA 004

# Binomial distrbution, n = 1000, p = 1/6.
# Comparing Normal approimation with Binomial.
p <- 1/6
n_trials <- 1000
x <- seq(0, n_trials)

# Plots
png(filename = 'Normal_approximation_to_Binomial.png')
plot(x, dbinom(size = n_trials, prob = p, x = x), 
     type = 'l', col = 'red', lwd = 3)
lines(x, dnorm(mean = n_trials*p, sd = sqrt(n_trials*p*(1-p)), x = x),
      type = 'l', col = 'blue', lty = 2, lwd = 3)
legend('topright',
       legend = c('Binomial w/ n = 1000, p = 1/6', 'Normal approximation'),
       lty = c(1, 2), col = c('red', 'blue'))
title('Normal approximation to the Binomial distribution')


query_q <- c(5, 25, 50, 75)
print(rbind(query_q, 
            pnorm(q = query_q, mean = n_trials*p, sd = n_trials*p*(1 - p))))
