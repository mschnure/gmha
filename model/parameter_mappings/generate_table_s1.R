# Generate Table S1
source("model/run_systematic.R")

LOCATIONS = c("South Africa","Kenya","Mozambique","Nigeria","Tanzania","Uganda","Zambia","Zimbabwe","Malawi")

# Testing, Engagement, Suppression 
testing.table = generate.testing.parameter.table(LOCATIONS)
engagement.table = generate.engagement.parameter.table(LOCATIONS)
suppression.table = generate.suppression.parameter.table(LOCATIONS)


# Disengagement 
disengagement.probabilities.unsuppressed = list()
disengagement.probabilities.suppressed = list()
for(location in LOCATIONS){
    rates = get.disengagement.model(location)
    prob.unsuppressed = 1 - exp(-rates$disengagement.rate.unsuppressed)
    prob.suppressed = 1 - exp(-rates$disengagement.rate.suppressed)
    
    odds.unsupp = prob.unsuppressed/(1-prob.unsuppressed)
    odds.unsupp.lower = odds.unsupp/4
    odds.unsupp.upper = odds.unsupp*4
    prob.unsupp.lower = odds.unsupp.lower/(1+odds.unsupp.lower)
    prob.unsupp.upper = odds.unsupp.upper/(1+odds.unsupp.upper)
    
    odds.supp = prob.suppressed/(1-prob.suppressed)
    odds.supp.lower = odds.supp/4
    odds.supp.upper = odds.supp*4
    prob.supp.lower = odds.supp.lower/(1+odds.supp.lower)
    prob.supp.upper = odds.supp.upper/(1+odds.supp.upper)
    
    disengagement.probabilities.unsuppressed[[location]] = paste0(prob.unsuppressed," (",
                                                                  round(prob.unsupp.lower,3),"-",round(prob.unsupp.upper,3),")")
    disengagement.probabilities.suppressed[[location]] = paste0(prob.suppressed," (",
                                                                round(prob.supp.lower,3),"-",round(prob.supp.upper,3),")")
}


# Unsuppression (Viral Rebound)
unsuppression.probability = list()
for(location in LOCATIONS){
    rate = get.unsuppression.rates(location)
    prob = 1 - exp(-rate)
    
    odds = prob/(1-prob)
    odds.lower = odds/4
    odds.upper = odds*4
    prob.lower = odds.lower/(1+odds.lower)
    prob.upper = odds.upper/(1+odds.upper)
    
    unsuppression.probability[[location]] = paste0(round(prob,3)," (",
                                                   round(prob.lower,3),"-",round(prob.upper,3),")")
}








