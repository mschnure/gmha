################################################################################################
# Description: Runs ONE chain of one MCMC step for one location in this R session
#
# For the usual workflow (Step 1, then all four Step 2 chains in parallel, then merging), use
# calibration/run_mcmc_pipeline.R instead. This script is for running or resuming a single chain.
#
# Step settings (iterations, weights, chains, seeds), Nigeria's Step 1 starting values, and folders
# are in calibration/mcmc_settings.R. Starting values are picked automatically:
#   Step 1 -> prior medians (or the file in STEP1.START.VALUE.FILES)
#   Step 2 -> most recent Step 2 starting values (saved automatically when Step 1 finishes)
################################################################################################

# source this before the settings, so nothing it loads can overwrite them
source("calibration/mcmc_worker_setup.R")

LOCATION = "Nigeria"
STEP = 1
CHAIN = 1          # Step 1: chain 1; Step 2: chains 1-4 (seed = MCMC.BASE.SEED + 1000*step + chain)
RESUME = FALSE     # TRUE = continue from this chain's existing cache

result = run.mcmc.step.chain(location = LOCATION, step = STEP, chain = CHAIN, resume = RESUME)

# Once all four Step 2 chains are finished (most recent file for each chain is used):
# merge.step2.chains(LOCATION)

# To make Step 2 starting values from a Step 1 run done before this pipeline existed:
# save.step2.start.values.from.file(LOCATION, "mcmc_runs/simset_nigeria_chain1_2026-10-14.Rdata")
