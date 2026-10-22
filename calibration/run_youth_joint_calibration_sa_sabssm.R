################################################################################################
# NEXTGEN: joint recalibration of South Africa, adding the 15-24 cascade targets from the national
# household surveys (SABSSM 2017 total + by sex, SABSSM 2022 total; see data_manager/add_sabssm_youth_cascade.R).
# Same set-up as calibration/run_youth_joint_calibration_sa.R (all parameters + 5 youth parameters,
# Step 2 only, 4 chains x 100,000 iterations -> 1,000 simulations), except:
#   - data manager: cached/data.manager_global_revised_ages_lai_2026-10-22.Rdata (loaded by source_code.R)
#   - warm start: the best of N.CANDIDATE.SIMS simulations from the 10/21 joint calibration, scored on the
#     NEW likelihood (with the 15-24 targets)
#   - its own folders (so the 10/21 run's complete caches aren't picked up as "already done"):
#     mcmc_cache/nextgen_sabssm, mcmc_runs/nextgen_sabssm, calibration/starting_values/nextgen_sabssm
#
# HOW TO RUN (Windows, in the gmha_nextgen worktree):
#     cd ../gmha_nextgen && git pull
#     RStudio: Jobs tab > Start Background Job > script = this file, working directory = gmha_nextgen.
#     Progress: mcmc_runs/nextgen_sabssm/logs/
# TO RESUME after a crash/restart: set RESUME = TRUE below
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

if(!isTRUE(attr(DATA.MANAGER, "sabssm.youth.cascade")))
    stop("DATA.MANAGER doesn't have the 15-24 SABSSM targets - check the load line in source_code.R")

MCMC.CACHE.DIR = "mcmc_cache/nextgen_sabssm"
MCMC.RUNS.DIR = "mcmc_runs/nextgen_sabssm"
STARTING.VALUES.DIR = "calibration/starting_values/nextgen_sabssm"
for(d in c(MCMC.CACHE.DIR, MCMC.RUNS.DIR, STARTING.VALUES.DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

RESUME = FALSE
RESUME = RESUME || "resume" %in% commandArgs(trailingOnly = TRUE)
N.CORES = 4

JOINT.SIMSET.FILE = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata"
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
    print(paste0("Warm start: simulation ", BASE.INDEX, " of the 10/21 joint calibration (log-likelihood ",
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
print("Joint South Africa youth calibration (with SABSSM 15-24 targets) finished")
