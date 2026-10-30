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
# Run on the machine that ran the MCMC (it reads mcmc_runs/). The first review of a run loads each full
# mcmc file once to save a light copy without the simulations (runs from now on save these as they go);
# after that it takes about a minute per location. Reviewing only some locations updates just their
# PDFs and rows in overview.csv.
# Step 2 simsets have 250 simulations per chain and 1,000 merged (STEP2.SIMSET.BURN/THIN), so they're
# plotted in full; anything bigger is thinned to ~MAX.PLOT.SIMS for the plots only.
# To look at one location interactively in RStudio instead, see calibration/review_mcmc_chains.R.
################################################################################################

source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")
source("calibration/mcmc_review.R")

LOCATIONS = "all"        # "all" = all 15, or e.g. c("Kenya", "Zambia")
MAX.PLOT.SIMS = 1000     # simulations per simset in the plots
REMAKE.STEP2.SIMSETS = FALSE  # TRUE only after changing STEP2.SIMSET.BURN/THIN (mcmc_settings.R): remakes
                              # each location's Step 2 simsets from its mcmc files first (slow: loads every
                              # full mcmc file). The pipeline already makes them with the current settings.

mcmc.progress()          # a location whose chains aren't all done gets reviewed with what's saved so far

if (REMAKE.STEP2.SIMSETS)
    for (location in (if (identical(LOCATIONS, "all")) names(LOCATION.PRIOR.NAMES) else LOCATIONS))
        tryCatch(remake.step2.simsets(location),
                 error = function(e) print(paste0(location, ": couldn't remake the Step 2 simsets (", conditionMessage(e), ")")))

overview = review.to.pdf(LOCATIONS, step = 2, max.plot.sims = MAX.PLOT.SIMS)
