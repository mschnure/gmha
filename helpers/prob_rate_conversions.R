# time = 1
# prob = .6
# rate = .001

rate = (-log(1-prob))/time
prob = 1 - exp(-rate*time)
prob

log.odds = log(prob/(1-prob))
prob = 1/(1+exp(-log.odds)) 
