prob = c(.969)
time = c(48/52)

rate = (-log(1-prob))/time
prob = 1 - exp(-rate*time)
annual.prob = 1 - exp(-rate)

time.to.suppress = 1/rate

rate
