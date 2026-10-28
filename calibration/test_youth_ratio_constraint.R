## NEXTGEN (10/28/26): check the youth:adult ratio constraint on 20 simulations of the 10/27 calibration:
## prints each sim's projected youth:adult ratios (2025, 2030) and the penalty, and the change in total likelihood.
source("model/run_systematic.R")
simset = get(load(max(list.files("mcmc_runs/nextgen_sabssm_ratio_priors", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)))[1])
idx = round(seq(1, simset@n.sim, length.out = 20)); sims = simset@simulations[idx]
CON = list(years = c(2025, 2030), threshold = 1.2, log.sd = log(1.5)/2)
pen = do.call(create.likelihood.for.youth.ratio, CON)
ratios = t(sapply(sims, function(sim){
    get = function(type, ages) as.numeric(extract.data(sim, type, years = CON$years, ages = ages, keep.dimensions = "year"))
    casc = function(ages){ p = get("prevalence", ages); a = get("awareness", ages); c(a/p, get("engagement", ages)/a, get("suppression", ages)/a) }
    r = casc(c("15-19","20-24"))/casc(c("25-29","30-34","35-39","40-44","45-49"))
    setNames(c(r, pen(sim)), c(paste0(rep(c("aware.","on.art.","suppressed."), each = 2), CON$years), "penalty"))
}))
print(round(ratios, 3))
YOUTH.ADULT.RATIO.CONSTRAINT = NULL; a = sapply(sims, set.likelihood.and.prior.by.location("South Africa", params.start.values = simset@parameters[1,])$likelihood.to.run)
YOUTH.ADULT.RATIO.CONSTRAINT = CON;  b = sapply(sims, set.likelihood.and.prior.by.location("South Africa", params.start.values = simset@parameters[1,])$likelihood.to.run)
YOUTH.ADULT.RATIO.CONSTRAINT = NULL
print(paste0("Total likelihood change = penalty? ", isTRUE(all.equal(as.numeric(b - a), as.numeric(ratios[, "penalty"])))))
