################################################################################################
# NEXTGEN (10/30/26): build the master-anchored transmission priors for South Africa.
# For each transmission parameter in SA.TRANSMISSION.PRIOR.ORIGINAL (trate.0-4, the 20 age-by-time transmission
# multipliers, female.to.male.multiplier, age.assortativity):
#   meanlog = log(master's posterior median); sdlog = max(SD.MULTIPLIER x master's posterior SD of log(value), SDLOG.FLOOR)
# i.e. an independent lognormal centred on master's Oct 20 South Africa calibration, about twice as wide as master's
# posterior. Saves calibration/prior_distributions/south_africa_transmission_anchor.Rdata (used when
# USE.SA.TRANSMISSION.ANCHOR = TRUE in calibration/mcmc_settings.R) and prints the table.
# Run once on the nextgen branch:  source("calibration/make_master_anchored_transmission_prior.R")
################################################################################################
source("model/run_systematic.R")
MASTER.SIMSET.FILE = "mcmc_runs/simset_south_africa_step2_chains1234_2026-10-20.Rdata"
SD.MULTIPLIER = 2
SDLOG.FLOOR = 0.1   # never tighter than about x/÷1.2 at 2 SD
OUT = "calibration/prior_distributions/south_africa_transmission_anchor.Rdata"

master = get(load(MASTER.SIMSET.FILE)[1])
pars = SA.TRANSMISSION.PRIOR.ORIGINAL@var.names
missing = setdiff(pars, colnames(master@parameters))
if(length(missing) > 0) stop(paste0("Not in the master simset: ", paste(missing, collapse = ", ")))
SA.TRANSMISSION.ANCHOR = do.call(rbind, lapply(pars, function(p){
    x = log(master@parameters[, p])
    data.frame(parameter = p, meanlog = median(x), sdlog = max(SD.MULTIPLIER*sd(x), SDLOG.FLOOR),
               master.median = exp(median(x)), master.lower = exp(quantile(x, 0.025)), master.upper = exp(quantile(x, 0.975)))
}))
SA.TRANSMISSION.ANCHOR$prior.lower = exp(SA.TRANSMISSION.ANCHOR$meanlog - 2*SA.TRANSMISSION.ANCHOR$sdlog)
SA.TRANSMISSION.ANCHOR$prior.upper = exp(SA.TRANSMISSION.ANCHOR$meanlog + 2*SA.TRANSMISSION.ANCHOR$sdlog)
rownames(SA.TRANSMISSION.ANCHOR) = NULL
attr(SA.TRANSMISSION.ANCHOR, "source") = MASTER.SIMSET.FILE
save(SA.TRANSMISSION.ANCHOR, file = OUT)
print(paste0("Saved ", OUT, " from ", MASTER.SIMSET.FILE, " (sdlog = ", SD.MULTIPLIER, " x master posterior, floor ", SDLOG.FLOOR, ")"))
options(width = 200)
print(cbind(SA.TRANSMISSION.ANCHOR[, 1], round(SA.TRANSMISSION.ANCHOR[, -1], 3)), row.names = F)
print("Now restart R; south_africa_prior.R will use these (check: SA.TRANSMISSION.ANCHORED should be TRUE).")
