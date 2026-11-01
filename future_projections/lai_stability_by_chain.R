################################################################################################
# NEXTGEN (11/01/26): is the LAI-ART impact stable across the 4 MCMC chains?
# Uses the outputs of future_projections/rerun_lai_sabssm_calibration.R (run that first, with the same NEW.RUN.DIR) -
# no new simulations. Each of the 1,000 merged simulations is matched to the chain it came from (by its parameter
# values, against the 4 per-chain simsets), then the LAI results are summarised per chain.
# Stable = the chains' medians are close to each other (and their intervals overlap a lot).
#
#   NEW.RUN.DIR = "mcmc_runs/nextgen_transmission_anchor"; source("future_projections/lai_stability_by_chain.R")
################################################################################################
if(!exists("NEW.RUN.DIR")) NEW.RUN.DIR = "mcmc_runs/nextgen_transmission_anchor"
OUT.DIR = file.path("cached", sub("^mcmc_runs/nextgen_", "nextgen_lai_rerun_", NEW.RUN.DIR))
newest = function(dir, pattern){
    f = list.files(dir, pattern = pattern, full.names = TRUE)
    if(length(f)==0) stop(paste0("Nothing matching ", pattern, " in ", dir))
    f[order(sub(".*_([0-9-]+)\\.Rdata$", "\\1", f))][length(f)]
}
merged.file = newest(NEW.RUN.DIR, "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$")
results.file = newest(OUT.DIR, "^full\\.results\\.array_[0-9-]+\\.Rdata$")
print(paste0("Merged simset: ", merged.file)); print(paste0("LAI results: ", results.file))

merged = get(load(merged.file)[1])
load(results.file) # full.results.array
if(dim(full.results.array)[which(names(dimnames(full.results.array))=="sim")] != merged@n.sim)
    stop("The LAI results don't have the same number of simulations as the merged simset - rerun rerun_lai_sabssm_calibration.R")

## which chain each merged simulation came from
key = function(p) apply(signif(p, 10), 1, paste, collapse = "|")
merged.key = key(merged@parameters)
chain.of.sim = rep(NA_integer_, merged@n.sim)
for(ch in 1:4){
    f = newest(NEW.RUN.DIR, paste0("^simset_south_africa_step2_chain", ch, "_[0-9-]+\\.Rdata$"))
    s = get(load(f)[1])
    chain.of.sim[merged.key %in% key(s@parameters[, colnames(merged@parameters), drop = FALSE])] = ch
    rm(s); gc()
}
print("Simulations per chain:"); print(table(chain = chain.of.sim, useNA = "ifany"))
if(any(is.na(chain.of.sim))) warning("Some simulations couldn't be matched to a chain - they're left out of the per-chain summaries")

## per-simulation outcomes, 2022-2030 (as in rerun_lai_joint_calibration.R)
YEARS = as.character(2022:2030)
inc = full.results.array[YEARS,,,"incidence",,,drop = FALSE]
new.all = apply(inc, c("sim","intervention"), sum)
new.1524 = apply(inc[,c("15-19","20-24"),,,,,drop = FALSE], c("sim","intervention"), sum)
SCENARIOS = c("all.25","all.direct.25","all.100","all.direct.100")
AIDS2026 = c(all.25 = 68721, all.direct.25 = 78053, all.100 = 191551, all.direct.100 = 220588)  # infections averted, all ages
AIDS2026.1524 = c(all.25 = 38881, all.direct.25 = 44284, all.100 = 108960, all.direct.100 = 125837)

summ = function(x) sprintf("%s (%s-%s)", format(round(median(x), -2), big.mark = ","),
                           format(round(quantile(x, .025), -2), big.mark = ","), format(round(quantile(x, .975), -2), big.mark = ","))
tables = list()
for(outcome in c("averted.all","averted.15.24","percent.averted.all")){
    tab = t(sapply(SCENARIOS, function(s){
        v = switch(outcome,
                   averted.all = new.all[,"no.int"] - new.all[,s],
                   averted.15.24 = new.1524[,"no.int"] - new.1524[,s],
                   percent.averted.all = 100*(new.all[,"no.int"] - new.all[,s])/new.all[,"no.int"])
        f = if(outcome=="percent.averted.all") function(x) sprintf("%.1f%% (%.1f-%.1f)", median(x), quantile(x,.025), quantile(x,.975)) else summ
        c(sapply(1:4, function(ch) f(v[which(chain.of.sim==ch)])), all.chains = f(v),
          spread = if(outcome=="percent.averted.all") sprintf("%.1f pts", diff(range(sapply(1:4, function(ch) median(v[which(chain.of.sim==ch)])))))
                   else sprintf("%.0f%%", 100*diff(range(sapply(1:4, function(ch) median(v[which(chain.of.sim==ch)]))))/median(v)))
    }))
    colnames(tab)[1:4] = paste0("chain", 1:4)
    if(outcome=="averted.all") tab = cbind(tab, aids2026 = format(AIDS2026[SCENARIOS], big.mark = ","))
    if(outcome=="averted.15.24") tab = cbind(tab, aids2026 = format(AIDS2026.1524[SCENARIOS], big.mark = ","))
    tables[[outcome]] = tab
    print(paste0("==== ", outcome, ", 2022-2030: median (95% interval) by chain; spread = (max - min chain median) / overall median ===="))
    print(noquote(tab))
}

## per-chain medians of the transmission parameters (what made LAI impact move before)
tp = intersect(c("trate.0","trate.1","trate.2","trate.3","trate.4","female.to.male.multiplier","age.assortativity",
                 "age.15.to.19.transmission.multiplier.3","age.20.to.29.transmission.multiplier.3","age.40.to.49.transmission.multiplier.3"),
               colnames(merged@parameters))
print("Per-chain medians of key transmission parameters:")
print(round(sapply(1:4, function(ch) apply(merged@parameters[which(chain.of.sim==ch), tp, drop = FALSE], 2, median)), 3))

write.csv(do.call(rbind, lapply(names(tables), function(o) data.frame(outcome = o, scenario = rownames(tables[[o]]), tables[[o]], check.names = FALSE))),
          file.path(OUT.DIR, paste0("lai_stability_by_chain_", Sys.Date(), ".csv")), row.names = FALSE)
print(paste0("Saved ", file.path(OUT.DIR, paste0("lai_stability_by_chain_", Sys.Date(), ".csv"))))
