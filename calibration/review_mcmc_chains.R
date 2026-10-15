################################################################################################
# Description: Review MCMC chains individually, then merge the ones to keep
#
# Run on the machine that ran the MCMC (chains are read from mcmc_runs/, which doesn't sync).
# Uses the most recent file for each chain. Works before or after merging, and while chains are
# still running (shows whatever has been saved).
################################################################################################

LOCATION = "Nigeria"
STEP = 2

source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")
source("calibration/mcmc_review.R")

# how far along every location/step/chain is (works while the pipeline is still running)
mcmc.progress()

# summary table per chain, R-hats, likelihood + parameter traces, and each chain's fit to the data
review = review.mcmc.chains(LOCATION, step = STEP)

# other views, e.g.:
# review.mcmc.chains(LOCATION, step = STEP, chains = c(1, 2))                      # just some chains
# review.mcmc.chains(LOCATION, step = STEP, trace.parameters = c("trate.0", "trate.1"))
# simplot(review$loaded$simsets$`chain 3`, years = 1980:2040, data.types = "incidence", facet.by = "age")

# a proposal that errored during the run (rejected, and logged to mcmc_runs/logs/..._errors_<date>.csv):
# rerun.mcmc.error("mcmc_runs/logs/nigeria_step2_chain1_errors_2026-10-20.csv", location = LOCATION, row = 1)
# traceback()

# merge (if the pipeline didn't already, or to leave out a chain):
# merge.step2.chains(LOCATION)                     # all four chains
# merge.step2.chains(LOCATION, chains = c(1, 2, 4))  # leave out chain 3
