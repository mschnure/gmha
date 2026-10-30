################################################################################################
# Description: Runs the MCMC calibration for one or more locations, unattended:
#   Step 1 (1 chain) -> saves Step 2 starting values -> Step 2 (4 chains) -> merges them
#
# All chains share the parallel R sessions, in priority order: Step 1 for every location, then
# Step 2 chain 1 for every location, then chain 2 for every location, and so on. A location's Step 2
# chains start as soon as its own Step 1 is done; a failed chain is retried (resuming from its
# cache); each location is merged as soon as its four Step 2 chains are done.
#
# How to run: on the MCMC machine, open the project in ONE RStudio window, set the options below, and
# run this file once as an RStudio Background Job (Background Jobs tab > Start Background Job > this
# file, working directory = the project folder). It starts its own parallel R sessions for the
# chains, and your console stays free (e.g., for mcmc.progress()). Keep RStudio open and the machine
# awake until it finishes; if it gets interrupted, start it again with RESUME = TRUE.
# (Or, independent of RStudio: Rscript calibration/run_mcmc_pipeline.R from a terminal in the project
# folder; on Mac/Linux, nohup Rscript calibration/run_mcmc_pipeline.R & keeps it running if the
# terminal closes.)
#
# Progress: - mcmc.progress() in any R session on this machine (after sourcing run_systematic.R and
#             calibration/mcmc_pipeline.R): % done for every location/step/chain
#           - status table: mcmc_runs/logs/pipeline_status_<...>.csv (updated as chains finish)
#           - summary log: mcmc_runs/logs/pipeline_<...>.log; one log per chain in
#             mcmc_runs/logs/<location>_step<step>_chain<chain>_<date>.log
#
# Settings for each step (iterations, weights, chains, seeds) and Nigeria's Step 1 starting values
# are in calibration/mcmc_settings.R.
################################################################################################

# source these before the settings, so nothing they load can overwrite them
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

LOCATIONS = "all"         # "all" = all 15 locations, or e.g. c("Kenya", "South Africa", "r1.low")
STEPS = c(1, 2)           # c(1, 2) = both; 2 = Step 2 only (uses the latest Step 2 starting values)
N.CORES = 23              # for running on remote
#N.CORES = max(1, parallel::detectCores() - 1)  # parallel R sessions (Step 2 needs 4 per location).
                          # Running one location per RStudio window? Use N.CORES = 4 in each, and keep
                          # (number of windows x 4) at or below the machine's cores (and watch memory).
                          # Alternative: one window with all locations in LOCATIONS - the pipeline then
                          # shares N.CORES across them automatically.
RESUME = FALSE            # TRUE = pick up existing caches (e.g., after a crash or restart) instead
                          #        of starting fresh; FALSE overwrites caches for these runs
MERGE = TRUE              # merge each location's Step 2 chains when they're done (each chain is also saved
                          # on its own, so you can review them individually either way - see review_mcmc_chains.R)
MAX.RETRIES = 1           # times to retry a failed chain (resuming from its cache) before giving up


results = run.mcmc.pipeline(locations = LOCATIONS,
                            steps = STEPS,
                            n.cores = N.CORES,
                            resume = RESUME,
                            merge = MERGE,
                            max.retries = MAX.RETRIES)
