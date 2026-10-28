## NEXTGEN (10/28/26): check the youth:adult ratio constraint on 20 simulations of the 10/27 calibration:
## prints each sim's projected youth:adult RATE ratios (testing, engagement, suppression; 2025, 2030) and the penalty, and the change in total likelihood.
source("model/run_systematic.R")
simset = get(load(max(list.files("mcmc_runs/nextgen_sabssm_ratio_priors", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)))[1])
idx = round(seq(1, simset@n.sim, length.out = 20)); sims = simset@simulations[idx]
CON = list(years = c(2025, 2030), reference.year = 2022, log.sd = log(1.5)/2, two.sided = TRUE)
pen = do.call(create.likelihood.for.youth.ratio, CON)
ratios = t(sapply(sims, function(sim){
    r = youth.adult.rate.ratios(sim, years = c(CON$reference.year, CON$years))
    setNames(c(as.numeric(t(r)), pen(sim)), c(paste0(rep(colnames(r), 2), ".", rep(rownames(r), each = ncol(r))), "penalty"))
}))
print("Youth (15-24) : adult (25-49) RATE ratios and the penalty, 20 sims of the 10/27 calibration:")
print(round(ratios, 3))
print("Medians:"); print(round(apply(ratios, 2, median), 3))
YOUTH.ADULT.RATIO.CONSTRAINT = NULL; a = sapply(sims, set.likelihood.and.prior.by.location("South Africa", params.start.values = simset@parameters[1,])$likelihood.to.run)
YOUTH.ADULT.RATIO.CONSTRAINT = CON;  b = sapply(sims, set.likelihood.and.prior.by.location("South Africa", params.start.values = simset@parameters[1,])$likelihood.to.run)
YOUTH.ADULT.RATIO.CONSTRAINT = NULL
print(paste0("Total likelihood change = penalty? ", isTRUE(all.equal(as.numeric(b - a), as.numeric(ratios[, "penalty"])))))
