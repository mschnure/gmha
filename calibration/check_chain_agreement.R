################################################################################################
# NEXTGEN: do the 4 Step 2 chains of a calibration agree? (default: the fresh retention run)
#   1. R-hat (potential scale reduction factor) for every parameter, from each chain's simset
#      (~1 = chains agree; above ~1.1 = they don't). Prints the worst 25.
#   2. Per-chain medians of the key parameters (dropout, youth multipliers, testing, suppression slope)
#   3. Per-chain log-likelihood (full South Africa likelihood) of 50 simulations each
#   4. Per-chain 15-24 cascade (2017, 2022) and 12-month dropout after starting ART (25-49, 2022)
# Reads the per-chain simsets simset_south_africa_step2_chain<k>_<date>.Rdata from RUN.DIR (these stay on the
# machine that ran the MCMC - run this there, or copy them over). If they're missing, falls back to splitting the
# merged simset into 4 equal blocks in order (assumes it was merged chain by chain - less reliable).
# Output: <RUN.DIR>/chain_agreement_<date>.csv (R-hat) and printed summary
################################################################################################
source("model/run_systematic.R")
if(!exists("RUN.DIR")) RUN.DIR = "mcmc_runs/nextgen_sabssm_ratio_retention"
N.LIK.SIMS = 50

files = list.files(RUN.DIR, pattern = "^simset_south_africa_step2_chain[1-4]_[0-9-]+\\.Rdata$", full.names = T)
chain.of = sub(".*_chain([1-4])_.*", "\\1", basename(files))
files = tapply(files, chain.of, max)   # newest file per chain
if(length(files) == 4){
    print("Using the per-chain simsets:"); print(unname(files))
    chains = lapply(files, function(f) get(load(f)[1]))
    pars = lapply(chains, function(s) s@parameters)
    sims = lapply(chains, function(s) s@simulations)
} else {
    merged = max(list.files(RUN.DIR, pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T))
    warning(paste0("Per-chain simsets not found in ", RUN.DIR, " - splitting ", merged, " into 4 blocks in order"))
    s = get(load(merged)[1]); n = s@n.sim; block = rep(1:4, each = ceiling(n/4))[1:n]
    pars = lapply(1:4, function(k) s@parameters[block == k,, drop = F])
    sims = lapply(1:4, function(k) s@simulations[block == k])
}
names(pars) = names(sims) = paste0("chain", 1:4)
print(paste0("Simulations per chain: ", paste(sapply(pars, nrow), collapse = ", ")))

## 1. R-hat
rhat = function(x.list){ # list of vectors, one per chain
    n = min(sapply(x.list, length)); x = sapply(x.list, function(v) v[seq_len(n)])
    B = n*var(colMeans(x)); W = mean(apply(x, 2, var))
    if(W == 0) return(NA)
    sqrt(((n-1)/n*W + B/n)/W)
}
par.names = colnames(pars[[1]])
r = sapply(par.names, function(p) rhat(lapply(pars, function(m) m[,p])))
r = sort(r, decreasing = T)
print("R-hat, worst 25 parameters (above ~1.1 = chains disagree):")
print(round(head(r, 25), 2))
print(paste0("Parameters with R-hat > 1.1: ", sum(r > 1.1, na.rm = T), " of ", length(r)))
write.csv(data.frame(parameter = names(r), rhat = r), file.path(RUN.DIR, paste0("chain_agreement_", Sys.Date(), ".csv")), row.names = F)

## 2. Per-chain medians of key parameters
KEY = c("unsuppressed.disengagement.rates","suppressed.disengagement.rates","unsuppression.rates",
        "log.OR.testing.intercept","log.OR.testing.slope","log.OR.engagement.slope","log.OR.suppression.slope",
        "age.15.24.awareness.multiplier","age.15.24.engagement.multiplier","age.15.24.suppression.multiplier",
        "log.OR.15.24.engagement.slope","log.OR.15.24.suppression.slope",
        "age.15.24.unsuppressed.disengagement.multiplier","age.15.24.suppressed.disengagement.multiplier",
        "age.15.24.unsuppression.multiplier",
        "age.0.14.awareness.multiplier","age.0.14.suppressed.disengagement.multiplier","trate.2","trate.3")
KEY = intersect(KEY, par.names)
print("Per-chain medians of key parameters:")
print(round(sapply(pars, function(m) apply(m[, KEY, drop = F], 2, median)), 3))

## 3. Per-chain log-likelihood
lik = set.likelihood.and.prior.by.location(location = "South Africa", params.start.values = pars[[1]][1,])$likelihood.to.run
set.seed(3)
ll = sapply(sims, function(sl){ idx = sort(sample(seq_along(sl), min(N.LIK.SIMS, length(sl)))); sapply(sl[idx], lik) })
print("Per-chain log-likelihood (full SA likelihood), median and range of sampled simulations:")
print(round(apply(ll, 2, function(x) c(median = median(x), min = min(x), max = max(x))), 1))

## 4. Per-chain 15-24 cascade and adult retention
cascade = function(sim){
    get = function(type, ages, yrs = c(2017, 2022)) as.numeric(extract.data(sim, type, years = yrs, ages = ages, keep.dimensions = "year"))
    y = c("15-19","20-24"); prev = get("prevalence", y); aware = get("awareness", y)
    c(aware.2017 = aware[1]/prev[1], aware.2022 = aware[2]/prev[2],
      on.art.2017 = get("engagement", y)[1]/aware[1], on.art.2022 = get("engagement", y)[2]/aware[2],
      suppressed.2017 = get("suppression", y)[1]/aware[1], suppressed.2022 = get("suppression", y)[2]/aware[2])
}
dropout.12mo = function(p){ # 25-49 adults (no youth multipliers): unsuppressed on ART -> suppressed / dropped out
    s = mean(p$SUPPRESSION.RATES$values[[which(p$SUPPRESSION.RATES$times == 2022)]][c("25-29","30-34","35-39","40-44","45-49"),,])
    du = mean(p$UNSUPPRESSED.DISENGAGEMENT.RATES$values[[1]][c("25-29","30-34","35-39","40-44","45-49"),,])
    ds = mean(p$SUPPRESSED.DISENGAGEMENT.RATES$values[[1]][c("25-29","30-34","35-39","40-44","45-49"),,])
    u = mean(p$UNSUPPRESSION.RATES$values[[1]][c("25-29","30-34","35-39","40-44","45-49"),,])
    Q = matrix(c(-(s+du), s, du, u, -(u+ds), ds, 0, 0, 0), 3, byrow = T)
    M = diag(3) + Q/2^30; for(j in 1:30) M = M %*% M
    M[1,3]
}
per.chain = sapply(sims, function(sl){
    idx = round(seq(1, length(sl), length.out = min(50, length(sl))))
    v = sapply(sl[idx], function(sim) c(cascade(sim), adult.dropout.12mo = dropout.12mo(sim$parameters$time.varying.parameters)))
    apply(v, 1, median)
})
print("Per-chain medians: 15-24 cascade (aware of PWH; on ART and suppressed of aware) and 25-49 12-month dropout after starting ART:")
print(round(per.chain, 3))
