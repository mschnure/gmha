## NEXTGEN (10/27/26): check YOUTH.CASCADE.LIKELIHOOD.WEIGHT only changes the youth cascade part of the likelihood.
## Scores 5 simulations of the ratio calibration on the South Africa likelihood with no youth weight, with the youth
## weight, and with the youth weight on a likelihood that has NO youth observations (should match "no weight" there).
source("model/run_systematic.R")
simset = get(load(max(list.files("mcmc_runs/nextgen_sabssm_ratio", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)))[1])
sims = simset@simulations[c(1, 250, 500, 750, 1000)]
score = function(){
    lik = set.likelihood.and.prior.by.location(location = "South Africa", params.start.values = simset@parameters[1,])$likelihood.to.run
    sapply(sims, lik)
}
YOUTH.CASCADE.LIKELIHOOD.WEIGHT = NULL;                                  a = score()
YOUTH.CASCADE.LIKELIHOOD.WEIGHT = c("0-14" = 0.25, "15-24" = 0.25);      b = score()
YOUTH.CASCADE.LIKELIHOOD.WEIGHT = c("an age group not in the data" = 0.25); c = score()
YOUTH.CASCADE.LIKELIHOOD.WEIGHT = NULL
print(round(rbind(no.weight = a, youth.weight.0.25 = b, weight.on.missing.age = c, difference = b - a), 1))
print(if(isTRUE(all.equal(a, c))) "OK: weighting an age group that isn't in the data changes nothing" else "PROBLEM: unexpected change")
print(if(all(b != a)) "OK: the youth weight changes the likelihood" else "PROBLEM: the youth weight has no effect")
