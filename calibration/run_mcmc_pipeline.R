################################################################################################
# Description: Runs the MCMC calibration for one or more locations, unattended:
#   Step 1 (1 chain) -> saves Step 2 starting values -> Step 2 (4 chains in parallel) -> merges them
#
# How to run: on the MCMC machine, open the project, set the options below, then run this file as
# an RStudio Background Job (Background Jobs tab > Start Background Job > this file, with the
# project folder as working directory). It keeps going if you close the session.
#
# Progress: summary in mcmc_runs/logs/pipeline_<date>_<time>.log, and one log per chain in
#           mcmc_runs/logs/<location>_step<step>_chain<chain>_<date>.log
#
# Settings for each step (iterations, weights, chains, seeds) and Nigeria's Step 1 starting values
# are in calibration/mcmc_settings.R.
################################################################################################

LOCATIONS = c("Nigeria")  # e.g. c("Kenya", "South Africa", "r1.low")
STEPS = c(1, 2)           # c(1, 2) = both; 2 = Step 2 only (uses the latest Step 2 starting values)
N.CORES = max(1, parallel::detectCores() - 1)  # parallel R sessions (Step 2 needs 4 per location).
                          # Running one location per RStudio window? Use N.CORES = 4 in each, and keep
                          # (number of windows x 4) at or below the machine's cores (and watch memory).
                          # Alternative: one window with all locations in LOCATIONS - the pipeline then
                          # shares N.CORES across them automatically.
RESUME = FALSE            # TRUE = pick up existing caches (e.g., after a crash or restart) instead
                          #        of starting fresh; FALSE overwrites caches for these runs
MERGE = TRUE              # merge the Step 2 chains at the end (each chain is also saved on its own,
                          # so you can review them individually either way - see review_mcmc_chains.R)

source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

results = run.mcmc.pipeline(locations = LOCATIONS,
                            steps = STEPS,
                            n.cores = N.CORES,
                            resume = RESUME,
                            merge = MERGE)
