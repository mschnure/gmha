################################################################################################
# 10/29/26 (branch master-convergence): South Africa calibration to test better MCMC mixing.
# Changes vs master (master's own calibration is untouched):
#   - sampling groups (calibration/prior_distributions/var_blocks.R): transmission parameters grouped by time point
#     (trate.k + every age group's multiplier .k), and female.to.male.multiplier + age.assortativity together
#   - Step 2 doubled: 4 chains x 200,000 iterations; burn 100,000, keep every 400th -> 250 per chain, 1,000 merged
#     (set below for this run only; calibration/mcmc_settings.R is unchanged)
#   - everything else as master: priors, likelihood, data manager, Step 1 (25,000 iterations from the prior medians)
# Folders (separate from master's runs): mcmc_cache/convergence, mcmc_runs/convergence,
#   calibration/starting_values/convergence
#
# HOW TO RUN (Windows, in a separate worktree so the master checkout is untouched):
#     git fetch origin
#     git worktree add ../gmha_convergence master-convergence      (once; later: cd ../gmha_convergence && git pull)
#     RStudio: Jobs tab > Start Background Job > script = this file, working directory = gmha_convergence.
#     Progress: MCMC.CACHE.DIR = "mcmc_cache/convergence"; mcmc.progress(locations = "South Africa")
#     When done: source calibration/check_chain_agreement.R (defaults to mcmc_runs/convergence)
# TO RESUME after a crash/restart: set RESUME = TRUE below
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

MCMC.CACHE.DIR = "mcmc_cache/convergence"
MCMC.RUNS.DIR = "mcmc_runs/convergence"
STARTING.VALUES.DIR = "calibration/starting_values/convergence"
for(d in c(MCMC.CACHE.DIR, MCMC.RUNS.DIR, STARTING.VALUES.DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

RESUME = FALSE
N.CORES = 4

MCMC.STEP.SETTINGS$step2$n.iter = 200000
STEP2.SIMSET.BURN = 100000
STEP2.SIMSET.THIN = 400
print(paste0("Step 2: ", MCMC.STEP.SETTINGS$step2$n.iter, " iterations, burn ", STEP2.SIMSET.BURN, ", thin ", STEP2.SIMSET.THIN))
print("Transmission sampling groups:"); print(PARAMETER.VAR.BLOCKS[grepl("^transmission|^trate4", names(PARAMETER.VAR.BLOCKS))])

if(length(list.files(STARTING.VALUES.DIR)) > 0 && !RESUME)
    stop(paste0(STARTING.VALUES.DIR, " already has starting values - empty it for a fresh run, or set RESUME = TRUE"))

results = run.mcmc.pipeline(locations = "South Africa",
                            steps = c(1, 2),
                            n.cores = N.CORES,
                            resume = RESUME,
                            merge = TRUE,
                            max.retries = 1)
print("South Africa convergence test finished")
