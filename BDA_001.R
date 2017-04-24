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

# Run this as a Poisson process, with an expectation of 42 patients over the day.
patients_admitted <- rpois(n = 1, lambda = 42)

# Now we generate 42 visit lengths according to a Uniform distribution
visit_lengths <- runif(n = patients_admitted, min = 5, max = 20)

# A patient must wait if all doctors are occupied. The doctors are occupied if
# there are at least three patients in the clinic at once. We need to know the
# times at which the patients arrive!