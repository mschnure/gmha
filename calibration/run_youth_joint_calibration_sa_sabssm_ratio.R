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
#   - warm start: the best of N.CANDIDATE.SIMS simulations from the 10/22 SABSSM calibration (which already has
#     the 15-24 parameters), scored on the NEW likelihood
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

JOINT.SIMSET.FILE = "mcmc_runs/nextgen_sabssm/simset_south_africa_step2_chains1234_2026-10-22.Rdata" # 10/22 SABSSM run (on the remote)
N.CANDIDATE.SIMS = 100

## Step 2 starting values (made once; a resumed run reuses them)
if(is.null(find.step2.start.values.file("South Africa"))){
    joint.simset = get(load(JOINT.SIMSET.FILE)[1])
    new.likelihood = set.likelihood.and.prior.by.location(location = "South Africa",
                                                          params.start.values = joint.simset@parameters[1,])$likelihood.to.run
    set.seed(20261022)
    candidates = sort(sample(1:joint.simset@n.sim, min(N.CANDIDATE.SIMS, joint.simset@n.sim)))
    candidate.lik = sapply(candidates, function(i){ new.likelihood(joint.simset@simulations[[i]]) })
    BASE.INDEX = candidates[which.max(candidate.lik)]
    print(paste0("Warm start: simulation ", BASE.INDEX, " of the 10/22 SABSSM calibration (log-likelihood ",
                 round(max(candidate.lik), 1), " on the new likelihood)"))
    params.start.values = joint.simset@parameters[BASE.INDEX,]
    start.file = step2.start.values.file("South Africa")
    save(params.start.values, file = start.file)
    print(paste0("Saved Step 2 starting values: ", start.file, " (", length(params.start.values), " parameters)"))
    rm(joint.simset); invisible(gc())
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
