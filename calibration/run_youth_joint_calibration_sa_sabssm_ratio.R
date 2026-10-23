################################################################################################
# NEXTGEN: joint recalibration of South Africa with 15-24 cascade targets built from the national household
# surveys as YOUTH-TO-ADULT RATIOS x the UNAIDS 15+ values (SABSSM 2017 total + by sex, 2022 total; method = "ratio"
# in data_manager/add_sabssm_youth_cascade.R). Replaces the 10/22 run, which fit the survey values as they are and
# ended up with youth starting ART / suppressing faster than adults after ~2020 (see
# future_projections/check_youth_suppression_speed.R).
# Same set-up as calibration/run_youth_joint_calibration_sa.R (joint: all parameters recalibrated together,
# Step 2 only, 4 chains x 100,000 iterations -> 1,000 simulations), except:
#   - 5 new parameters for model ages 15-19 and 20-24, applied like the 0-14 ones:
#     age.15.24.awareness/engagement/suppression.multiplier (prior lognormal median 1, 95% range 1/8-8) and
#     log.OR.15.24.engagement/suppression.slope (prior normal mean 0, 95% range log(1/4)-log(4)); 10 youth parameters in all
#   - data manager: cached/data.manager_global_revised_ages_lai_2026-10-23.Rdata (loaded by source_code.R)
#   - warm start: the best of N.CANDIDATE.SIMS simulations from the 10/21 joint calibration, scored on the NEW
#     likelihood, 15-24 parameters starting at no effect (same start as the 10/22 run)
#   - its own folders (so the 10/21 run's complete caches aren't picked up as "already done"):
#     mcmc_cache/nextgen_sabssm_ratio, mcmc_runs/nextgen_sabssm_ratio, calibration/starting_values/nextgen_sabssm_ratio
#
# HOW TO RUN (Windows, in the gmha_nextgen worktree):
#     cd ../gmha_nextgen && git pull
#     RStudio: Jobs tab > Start Background Job > script = this file, working directory = gmha_nextgen.
#     Progress: mcmc_runs/nextgen_sabssm_ratio/logs/
# TO RESUME after a crash/restart: set RESUME = TRUE below
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

if(!identical(attr(DATA.MANAGER, "sabssm.youth.cascade.method"), "ratio"))
    stop("DATA.MANAGER doesn't have the ratio-based 15-24 SABSSM targets - check the load line in source_code.R")

MCMC.CACHE.DIR = "mcmc_cache/nextgen_sabssm_ratio"
MCMC.RUNS.DIR = "mcmc_runs/nextgen_sabssm_ratio"
STARTING.VALUES.DIR = "calibration/starting_values/nextgen_sabssm_ratio"
for(d in c(MCMC.CACHE.DIR, MCMC.RUNS.DIR, STARTING.VALUES.DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

RESUME = FALSE
RESUME = RESUME || "resume" %in% commandArgs(trailingOnly = TRUE)
N.CORES = 4

# Warm start: the best of N.CANDIDATE.SIMS random simulations from the 10/21 joint calibration, scored on the new
# likelihood, with the new 15-24 parameters at no effect (multipliers 1, slopes 0) - same start as the 10/22 run.
CANDIDATE.SIMSET.FILES = c("Oct 21 joint" = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata")
NEUTRAL.15.24 = c(age.15.24.awareness.multiplier = 1, age.15.24.engagement.multiplier = 1,
                  age.15.24.suppression.multiplier = 1, log.OR.15.24.engagement.slope = 0,
                  log.OR.15.24.suppression.slope = 0)
N.CANDIDATE.SIMS = 100

## Step 2 starting values (made once; a resumed run reuses them)
if(is.null(find.step2.start.values.file("South Africa"))){
    new.likelihood = NULL
    best = list(lik = -Inf)
    set.seed(20261023)
    for(nm in names(CANDIDATE.SIMSET.FILES)){
        simset = get(load(CANDIDATE.SIMSET.FILES[[nm]])[1])
        if(is.null(new.likelihood))
            new.likelihood = set.likelihood.and.prior.by.location(location = "South Africa",
                                                                  params.start.values = simset@parameters[1,])$likelihood.to.run
        candidates = sort(sample(1:simset@n.sim, min(N.CANDIDATE.SIMS, simset@n.sim)))
        candidate.lik = sapply(candidates, function(i){ new.likelihood(simset@simulations[[i]]) })
        print(paste0(nm, ": best log-likelihood on the new likelihood ", round(max(candidate.lik), 1),
                     " (median ", round(median(candidate.lik), 1), ")"))
        if(max(candidate.lik) > best$lik){
            pars = simset@parameters[candidates[which.max(candidate.lik)],]
            missing.15.24 = setdiff(names(NEUTRAL.15.24), names(pars))
            pars[missing.15.24] = NEUTRAL.15.24[missing.15.24]
            best = list(lik = max(candidate.lik), source = nm, index = candidates[which.max(candidate.lik)], pars = pars)
        }
        rm(simset); invisible(gc())
    }
    print(paste0("Warm start: simulation ", best$index, " of the ", best$source, " calibration (log-likelihood ",
                 round(best$lik, 1), ")"))
    params.start.values = best$pars
    start.file = step2.start.values.file("South Africa")
    save(params.start.values, file = start.file)
    print(paste0("Saved Step 2 starting values: ", start.file, " (", length(params.start.values), " parameters)"))
}
print(paste0("Starting values: ", find.step2.start.values.file("South Africa")))
missing = setdiff(get.location.prior("South Africa")@var.names, names(load.start.values.file(find.step2.start.values.file("South Africa"))))
if(length(missing) > 0) stop(paste0("Starting values are missing: ", paste0(missing, collapse = ", ")))

results = run.mcmc.pipeline(locations = "South Africa",
                            steps = 2,
                            n.cores = N.CORES,
                            resume = RESUME,
                            merge = TRUE,
                            max.retries = 1)
print("Joint South Africa youth calibration (SABSSM 15-24 ratio targets) finished")
