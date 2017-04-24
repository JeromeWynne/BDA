# Page 28 from Gelman

# A clinic has three doctors. Patients come into the clinic
# at random, starting at 9am, according to a Poisson process
# with time parameter 10 minutes.
# When a patient arrivies, they wait until a doctor is available.
# The amount of time spent by each doctor with each patient
# is random variable, uniformaly distributed between 5 and 20 minutes..
# The office stops admitting new patients at 4pm, and closes when the 
# last patient is through with the doctor.

# We're going to simulate this process.

# Start with one doctor.
# Minutes between 9am and 4pm = 7*60 = 420

# The time between arrivals at the clinic is Exponentially distributed.
# We'll start with the number of patients that arrive at the clinic between 9 and 4.

n_simulations <- 100
results <- as.data.frame(matrix(0, nrow = n_simulations, ncol = 4))

for (m in seq(n_simulations)) {
  # Run this as an Exponential process, with an expected arrival time of 10 minutes
  arrival_intervals <- rexp(n = 84, rate = 0.1)
  arrival_intervals <- arrival_intervals[(cumsum(arrival_intervals) < 420)]
  arrival_times <- cumsum(arrival_intervals)
  n_patients <- length(arrival_intervals)
  
  # Now we generate 42 visit lengths according to a Uniform distribution
  appt_lengths <- runif(n = n_patients, min = 5, max = 20)
  
  # With three doctors
  appt_end <- c(0, 0, 0)
  wait_lengths <- matrix(0, nrow = n_patients, ncol = 1)
  for (j in seq(n_patients)) {
    k <- which.min(appt_end)
    wait_lengths[j] <- max(appt_end[k] - arrival_times[j], 0)
    appt_end[k] <- appt_lengths[j] + arrival_times[j] + wait_lengths[j]
  }
  
  n_waited <- sum(wait_lengths > 0)
  avg_wait <- mean(wait_lengths[wait_lengths > 0])
  office_closed <- 9 + max(appt_end)/60
  
  results[m, ] <- data.frame(n_waited, avg_wait, office_closed, n_patients)
}

names(results) <- c('n_waited', 'avg_wait', 'close_time', 'n_patients')
