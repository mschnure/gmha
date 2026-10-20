
sim.new = simset.new@simulations[[length(simset.new@simulations)]]
sim.old = simset.old@simulations[[length(simset.old@simulations)]]

source("calibration/likelihood/individual_likelihoods.R")


exp(LOCATION.DETAILS$likelihood.to.run(sim.new) - 
      LOCATION.DETAILS$likelihood.to.run(sim.old))


exp(full.lik(sim.new) - full.lik(sim.old))
exp(pop.lik(sim.new) - pop.lik(sim.old))
exp(incidence.lik(sim.new) - incidence.lik(sim.old)) 
exp(prev.lik(sim.new) - prev.lik(sim.old))
exp(aware.lik(sim.new) - aware.lik(sim.old)) # THIS IS BETTER
exp(eng.lik(sim.new) - eng.lik(sim.old))
exp(supp.lik(sim.new) - supp.lik(sim.old)) 
exp(hiv.mortality.lik(sim.new) - hiv.mortality.lik(sim.old))
exp(aware.trend.lik(sim.new) - aware.trend.lik(sim.old))
exp(total.mortality.lik(sim.new) - total.mortality.lik(sim.old)) # THIS IS WHAT CHANGED IT
