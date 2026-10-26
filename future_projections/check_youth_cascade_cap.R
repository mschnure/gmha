################################################################################################
# NEXTGEN: does capping the 15-24 cascade rates at the adult rate (options(gmha.cap.youth.cascade = TRUE),
# model/parameters.R) leave the calibration fit alone? Reruns N.SIMS simulations of the ratio calibration
# with and without the cap (baseline, no LAI-ART) and compares:
#   - log-likelihood (the full South Africa likelihood)
#   - 15-24 and 25-49 cascade (aware of PWH; on ART and suppressed of aware) in 2017, 2022, 2025, 2030
#   - 15-24 vs 25-49 calibrated rates in 2030 (to show the cap binding)
#   - new infections 2022-2030 (all ages and 15-24)
################################################################################################
source("model/run_systematic.R")
source("model/diffeq_cpp.R")
options(gmha.use.cpp = TRUE)
N.SIMS = 100
SIMSET.FILE = max(list.files("mcmc_runs/nextgen_sabssm_ratio", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T))
simset = get(load(SIMSET.FILE)[1])
set.seed(2); idx = sort(sample(seq_len(simset@n.sim), N.SIMS))
lik = set.likelihood.and.prior.by.location(location = "South Africa", params.start.values = simset@parameters[1,])$likelihood.to.run
LOCATION = "South Africa"
params = create.model.parameters(location = LOCATION)
YEARS = c(2017, 2022, 2025, 2030)
AGES = list("15-24" = c("15-19","20-24"), "25-49" = c("25-29","30-34","35-39","40-44","45-49"))

summarize.sim = function(sim){
    get = function(type, ages, yrs = YEARS) as.numeric(extract.data(sim, type, years = yrs, ages = ages, keep.dimensions = "year"))
    rv = c()
    for(a in names(AGES)){
        prev = get("prevalence", AGES[[a]]); aware = get("awareness", AGES[[a]])
        rv[paste0("aware.", a, ".", YEARS)] = aware/prev
        rv[paste0("on.art.", a, ".", YEARS)] = get("engagement", AGES[[a]])/aware
        rv[paste0("suppressed.", a, ".", YEARS)] = get("suppression", AGES[[a]])/aware
    }
    rv["new.infections.2022.2030"] = sum(as.numeric(extract.data(sim, "incidence", years = 2022:2030, keep.dimensions = "year")))
    rv["new.infections.15.24.2022.2030"] = sum(get("incidence", AGES[["15-24"]], 2022:2030))
    rv["log.likelihood"] = lik(sim)
    tvp = sim$parameters$time.varying.parameters
    for(nm in c("TESTING.RATES","ENGAGEMENT.RATES","SUPPRESSION.RATES")){
        v = tvp[[nm]]$values[[which(tvp[[nm]]$times == 2030)]]
        rv[paste0(nm, ".15-24.vs.25-49.2030")] = mean(v[AGES[["15-24"]],,]) / mean(v[AGES[["25-49"]],,])
    }
    rv
}
out = list()
for(cap in c(FALSE, TRUE)){
    options(gmha.cap.youth.cascade = cap)
    print(paste0("cap = ", cap))
    out[[as.character(cap)]] = sapply(idx, function(i){
        sim = run.model.for.parameters(variable.parameters = simset@parameters[i,], parameters = params, location = LOCATION)
        summarize.sim(sim)
    })
}
options(gmha.cap.youth.cascade = FALSE)
med = function(m) apply(m, 1, median)
tab = data.frame(no.cap = med(out[["FALSE"]]), cap = med(out[["TRUE"]]))
tab$difference = tab$cap - tab$no.cap
print(paste0(N.SIMS, " simulations from ", SIMSET.FILE, " (medians):"))
print(round(tab, 3))
d.lik = out[["TRUE"]]["log.likelihood",] - out[["FALSE"]]["log.likelihood",]
print(paste0("Change in log-likelihood with the cap, per simulation: median ", round(median(d.lik), 2),
             ", range ", round(min(d.lik), 2), " to ", round(max(d.lik), 2)))
