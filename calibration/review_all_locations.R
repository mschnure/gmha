################################################################################################
# Description: Review the calibration for every location at once, saved to PDFs
#
# For each location, one PDF in results/mcmc_review_<date>/ with:
#   - a summary page: per-chain acceptance rate, likelihoods, errored proposals; worst R-hats;
#     size of the merged simset
#   - likelihood and parameter trace plots (all 4 chains)
#   - the calibration plots from analyze_parallel_mcmc.R with each chain's simset on the same plot
#   - the same calibration plots for the merged Step 2 simset
# plus overview.csv: one row per location (acceptance, worst R-hat, merged simset size, problems).
#
# Run on the machine that ran the MCMC (it reads mcmc_runs/). Takes a few minutes per location.
# Simsets are thinned to ~MAX.PLOT.SIMS simulations for the plots only; saved simsets aren't changed.
# To look at one location interactively in RStudio instead, see calibration/review_mcmc_chains.R.
################################################################################################

source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")
source("calibration/mcmc_review.R")

LOCATIONS = "all"        # "all" = all 15, or e.g. c("Kenya", "Zambia")
MAX.PLOT.SIMS = 1000     # simulations per simset in the plots
REMAKE.MERGED.SIMSETS = TRUE  # first remake each merged simset from its merged mcmc with the current
                              # MERGED.SIMSET.BURN/THIN (calibration/mcmc_settings.R: burn 50,000, thin 200
                              # -> 1,000 sims); needed for simsets merged before that setting existed

mcmc.progress()          # a location whose chains aren't all done gets reviewed with what's saved so far

if (REMAKE.MERGED.SIMSETS)
    for (location in (if (identical(LOCATIONS, "all")) names(LOCATION.PRIOR.NAMES) else LOCATIONS))
        tryCatch(remake.merged.simset(location),
                 error = function(e) print(paste0(location, ": couldn't remake the merged simset (", conditionMessage(e), ")")))

overview = review.to.pdf(LOCATIONS, step = 2, max.plot.sims = MAX.PLOT.SIMS)
