################################################################################################
# NEXTGEN: joint recalibration of South Africa with the youth cascade - all parameters plus the 5
# youth (0-14) cascade parameters, full likelihood including the 0-14 cascade data. Same approach as
# the Oct 2025 youth calibration, now on the standard MCMC pipeline (calibration/mcmc_pipeline.R).
#
#   Step 2 only: 4 chains x 100,000 iterations (MCMC.STEP.SETTINGS$step2), each with its own seed,
#   merged into 1,000 simulations (burn 50,000, thin 200 per chain). No Step 1: all 4 chains are
#   warm-started from the new master calibration:
#     - master parameters: simulation 738 of the master Step 2 simset (best fit on master's likelihood
#       of 100 sampled simulations; picked in calibration/youth_cascade_calibration.R)
#     - youth parameters: posterior medians from that youth calibration (fitted to simulation 738)
#
# Kept apart from master's runs: caches in mcmc_cache/nextgen, outputs and logs in mcmc_runs/nextgen,
# starting values in calibration/starting_values/nextgen.
#
# Run from Terminal (in the gmha folder):
#   nohup caffeinate -i Rscript calibration/run_youth_joint_calibration_sa.R > mcmc_runs/logs/nextgen_joint_sa.log 2>&1 &
# To resume after a crash/sleep/restart, add "resume" at the end of the Rscript command.
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

MCMC.CACHE.DIR = "mcmc_cache/nextgen"
MCMC.RUNS.DIR = "mcmc_runs/nextgen"
STARTING.VALUES.DIR = "calibration/starting_values/nextgen"
for(d in c(MCMC.CACHE.DIR, MCMC.RUNS.DIR, STARTING.VALUES.DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

RESUME = "resume" %in% commandArgs(trailingOnly = TRUE)
N.CORES = 4

MASTER.SIMSET.FILE = "mcmc_runs/simset_south_africa_step2_chains1234_2026-10-20.Rdata"
BASE.INDEX = 738
YOUTH.START = c(age.0.14.awareness.multiplier = 0.737, age.0.14.engagement.multiplier = 0.704,
                age.0.14.suppression.multiplier = 0.323, log.OR.0.14.engagement.slope = 0.666,
                log.OR.0.14.suppression.slope = 0.615)

## Step 2 starting values (made once; a resumed run reuses them)
if(is.null(find.step2.start.values.file("South Africa"))){
    master.simset = get(load(MASTER.SIMSET.FILE)[1])
    params.start.values = master.simset@parameters[BASE.INDEX,]
    params.start.values[names(YOUTH.START)] = YOUTH.START
    start.file = step2.start.values.file("South Africa")
    save(params.start.values, file = start.file)
    print(paste0("Saved Step 2 starting values: ", start.file, " (", length(params.start.values), " parameters)"))
    rm(master.simset); invisible(gc())
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
print("Joint South Africa youth calibration finished")
