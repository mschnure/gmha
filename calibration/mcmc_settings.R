################################################################################################
# Description: Settings for the two-step MCMC calibration (edit these here, not in the run scripts)
#
#   Step 1: one chain, starting from the prior medians (or a saved file, see below),
#           prevalence weighted 4x. When it finishes, the parameters of its last simulation are
#           saved as the Step 2 starting values (calibration/starting_values/..._step2_start_values.Rdata)
#   Step 2: four chains, each with its own seed, all starting from those values
#
# Used by calibration/mcmc_pipeline.R (run_mcmc_pipeline.R, setup_run_mcmc.R) and
# set.likelihood.and.prior.by.location
################################################################################################

MCMC.STEP.SETTINGS = list(
    step1 = list(weighted.prevalence = TRUE,
                 total.mortality.weight = 1/1000,
                 total.weight = 1,
                 n.iter = 25000,
                 chains = 1),
    step2 = list(weighted.prevalence = FALSE,
                 total.mortality.weight = 1/1000,
                 total.weight = 1,
                 n.iter = 100000,
                 chains = 1:4)
)

# Seeds: each run's seed is MCMC.BASE.SEED + 1000*step + chain (e.g., Step 2 chain 3 = base + 2003),
# so every location/step/chain is reproducible and different. Change the base for a whole new set.
# (Errors from odd parameter values no longer crash a chain - see mcmc_pipeline.R - so you shouldn't
# need new seeds to get past a crash.)
MCMC.BASE.SEED = 20261015

# Locations whose Step 1 starts from saved values instead of the prior medians
STEP1.START.VALUE.FILES = c(
    "Nigeria" = "calibration/starting_values/2026_10_12_nigeria_start_values.Rdata"
)

# Sampler and output settings (same values as the previous setup_run_mcmc.R)
MCMC.UPDATE.FREQUENCY = 100
MCMC.CACHE.FREQUENCY = 200
MCMC.INITIAL.STEP.SIZE.DIVISOR = 20   # initial covariance = diag((prior sds / 20)^2)
# Thinning while sampling: the sampler stores the simulation of every MCMC.THIN-th iteration only.
# Doesn't change how the chain moves (same seed -> same chain); only what's saved. With 1, a Step 2
# chain stores all 100,000 simulations (~16 GB cache + ~16 GB mcmc file); with 20, ~1/20th of that.
MCMC.THIN = 20

# Simset burn and thin, in ITERATIONS (converted to the mcmc's own thinning when the simset is made,
# so e.g. burn 200 / thin 20 on an mcmc thinned by 20 = drop the first 10 stored, keep all after)
SIMSET.ADDITIONAL.BURN = 200   # Step 1 simsets (their last simulation = the Step 2 starting values)
SIMSET.ADDITIONAL.THIN = 20

# Step 2 simsets - each chain's and the merged one - also in ITERATIONS per chain: drop the first half
# of each chain, then keep every 200th -> (100,000 - 50,000) / 200 = 250 per chain, 1,000 merged
STEP2.SIMSET.BURN = 50000
STEP2.SIMSET.THIN = 200

# Folders. mcmc_cache and mcmc_runs stay on the machine that runs the MCMC; the Step 2 starting
# values go in calibration/starting_values, which syncs between machines.
MCMC.CACHE.DIR = "mcmc_cache"
MCMC.RUNS.DIR = "mcmc_runs"
STARTING.VALUES.DIR = "calibration/starting_values"

# NEXTGEN (10/27/26): weight on the South Africa youth cascade observations (awareness/engagement/suppression for
# surveillance ages 0-14 and 15-24), on top of the usual cascade weights; e.g. c("0-14" = 0.25, "15-24" = 0.25).
# NULL = no extra weighting (as before). Set in the run script.
YOUTH.CASCADE.LIKELIHOOD.WEIGHT = NULL

# NEXTGEN (10/28/26): soft one-sided constraint on PROJECTED South Africa youth (15-24) : adult (25-49) ratios of the
# testing, engagement and suppression rates, e.g. list(years = c(2025, 2030), reference.year = 2022, log.sd = log(1.5)/2, two.sided = FALSE)
# (see create.likelihood.for.youth.ratio in calibration/likelihood/likelihood.R). NULL = off. Set in the run script.
YOUTH.ADULT.RATIO.CONSTRAINT = NULL

# NEXTGEN (10/30/26): anchor South Africa's transmission priors (trates, age-by-time multipliers, female-to-male, age
# assortativity) to master's Oct 20 calibration - see calibration/prior_distributions/south_africa_prior.R. Only takes
# effect once calibration/prior_distributions/south_africa_transmission_anchor.Rdata exists (made by
# calibration/make_master_anchored_transmission_prior.R). Read when the priors are built (sourcing), so set it here.
USE.SA.TRANSMISSION.ANCHOR = TRUE

# Names of the settings above (the pipeline copies these to its worker R sessions, so any changes
# made in the current session after sourcing this file also apply to the workers)
MCMC.SETTING.NAMES = c("MCMC.STEP.SETTINGS", "MCMC.BASE.SEED", "STEP1.START.VALUE.FILES",
                       "MCMC.UPDATE.FREQUENCY", "MCMC.CACHE.FREQUENCY", "MCMC.INITIAL.STEP.SIZE.DIVISOR", "MCMC.THIN",
                       "SIMSET.ADDITIONAL.BURN", "SIMSET.ADDITIONAL.THIN", "STEP2.SIMSET.BURN", "STEP2.SIMSET.THIN",
                       "MCMC.CACHE.DIR", "MCMC.RUNS.DIR", "STARTING.VALUES.DIR", "YOUTH.CASCADE.LIKELIHOOD.WEIGHT",
                       "YOUTH.ADULT.RATIO.CONSTRAINT")
