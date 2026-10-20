################################################################################################
# NEXTGEN: check a new South Africa calibration (run on master) before using it for the LAI analysis
#   1. fit to the 0-14 vs 15+ cascade data (master's calibration only fits 15+ cascade data)
#   2. the usual fit checks
#   3. re-runs a few sims on nextgen and compares them to the stored sims - should match, since the
#      baseline LAI rates are 0 (differences = R vs C++ ODE, or something off in the merge)
#   (uses the full simset for LAI: load NEW.SIMSET.FILE in future_projections/run_lai_interventions.R)
################################################################################################
source("model/run_systematic.R")

NEW.SIMSET.FILE = "mcmc_runs/simset_south_africa_step2_chains1234_2026-10-20.Rdata" 

simset = get(load(NEW.SIMSET.FILE)[1])
print(paste0(simset@n.sim, " distinct simulations, total weight ", sum(simset@weights)))

## 1. Youth cascade: if 0-14 is clearly off, recalibrate SA on nextgen (run_mcmc_pipeline.R) -
##    the 0-14 data and youth parameters are already set up there
simplot(simset,
        years = 2000:2040,
        data.types = c('awareness','engagement','suppression'),
        facet.by = 'age',
        ages = c('0-14','15+'),
        proportion = T)

## 2. Usual checks
simplot(simset, years = 1980:2040, data.types = c('incidence','prevalence'))
simplot(simset, years = 1980:2040, data.types = c('awareness','engagement','suppression'), proportion = T)

## 3. Re-run ~10 sims on nextgen (no intervention) and compare to the stored sims
simset.check = thin.simset(simset, thin = ceiling(sum(simset@weights)/10))
simset.rerun = run.intervention.on.simset(simset.check, end.year = 2040, interventions = NO.INTERVENTION)

check.years = 1990:2040
for(data.type in c('incidence','prevalence','hiv.mortality')){
    max.rel.diff = max(sapply(1:simset.check@n.sim, function(i){
        old = extract.data(simset.check@simulations[[i]], data.type = data.type, years = check.years)
        new = extract.data(simset.rerun@simulations[[i]], data.type = data.type, years = check.years)
        max(abs(new - old)/pmax(abs(old), 1))
    }))
    print(paste0(data.type, ": max relative difference, nextgen vs stored = ", signif(max.rel.diff, 3)))
}
# expect ~0 (tiny if the stored sims came from the C++ ODE); anything over ~0.01 is worth a look
