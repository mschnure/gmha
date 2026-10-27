################################################################################################
# NEXTGEN (10/27/26): fresh joint recalibration of South Africa (Step 1 + Step 2, from the prior medians) with
#   - 15-24 cascade targets: SABSSM 2017/2022 youth-to-adult ratios x UNAIDS 15+ (data manager method = "ratio")
#   - youth (0-14, 15-24) parameters: awareness/engagement/suppression multipliers, engagement/suppression slopes,
#     and ONE dropout multiplier per age group (on both disengagement rates) - 12 youth parameters
#   - tighter priors (calibration/prior_distributions/south_africa_prior.R): all-ages dropout ~0.08-0.31/yr around
#     IeDEA 0.155; youth dropout multiplier median 1.5 (0.75-3); youth multipliers 1/4-4; youth slopes +/-log(2)
#   - youth cascade observations (0-14, 15-24) down-weighted to YOUTH.CASCADE.LIKELIHOOD.WEIGHT; all other
#     likelihood weights as before
#   - Step 1 doubled to 50,000 iterations; Step 2 as usual (4 chains x 100,000 -> 1,000 simulations)
# Replaces the 10/26 retention run (run_youth_joint_calibration_sa_sabssm_ratio_retention.R), whose 4 chains
# didn't agree (66 of 93 parameters with R-hat > 1.1; calibration/check_chain_agreement.R).
# Folders: mcmc_cache/nextgen_sabssm_ratio_priors, mcmc_runs/nextgen_sabssm_ratio_priors,
#          calibration/starting_values/nextgen_sabssm_ratio_priors
#
# HOW TO RUN (Windows, in the gmha_nextgen worktree):
#     cd ../gmha_nextgen && git pull
#     RStudio: Jobs tab > Start Background Job > script = this file, working directory = gmha_nextgen.
#     Progress: MCMC.CACHE.DIR = "mcmc_cache/nextgen_sabssm_ratio_priors"; mcmc.progress(locations = "South Africa")
#     When done: source calibration/check_chain_agreement.R with RUN.DIR = "mcmc_runs/nextgen_sabssm_ratio_priors"
# TO RESUME after a crash/restart: set RESUME = TRUE below
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

if(!identical(attr(DATA.MANAGER, "sabssm.youth.cascade.method"), "ratio"))
    stop("DATA.MANAGER doesn't have the ratio-based 15-24 SABSSM targets - check the load line in source_code.R")

MCMC.CACHE.DIR = "mcmc_cache/nextgen_sabssm_ratio_priors"
MCMC.RUNS.DIR = "mcmc_runs/nextgen_sabssm_ratio_priors"
STARTING.VALUES.DIR = "calibration/starting_values/nextgen_sabssm_ratio_priors"
for(d in c(MCMC.CACHE.DIR, MCMC.RUNS.DIR, STARTING.VALUES.DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

RESUME = FALSE
RESUME = RESUME || "resume" %in% commandArgs(trailingOnly = TRUE)
N.CORES = 4

YOUTH.CASCADE.LIKELIHOOD.WEIGHT = c("0-14" = 0.25, "15-24" = 0.25) # x the usual cascade weights, youth observations only
MCMC.STEP.SETTINGS$step1$n.iter = 50000                              # doubled (calibration/mcmc_settings.R: 25,000)
print(paste0("Youth cascade likelihood weight: ", paste(names(YOUTH.CASCADE.LIKELIHOOD.WEIGHT), YOUTH.CASCADE.LIKELIHOOD.WEIGHT, sep = " = ", collapse = ", "),
             "; Step 1 iterations: ", MCMC.STEP.SETTINGS$step1$n.iter))

# FRESH START: the full two-step pipeline, as for master - Step 1 (1 chain, 50,000 iterations here,
# prevalence weighted, from the prior medians) hands its last simulation to Step 2 (4 chains x 100,000).
# No warm start from an earlier calibration. Settings: calibration/mcmc_settings.R.
if(length(list.files(STARTING.VALUES.DIR)) > 0 && !RESUME)
    stop(paste0(STARTING.VALUES.DIR, " already has starting values - empty it for a fresh run, or set RESUME = TRUE"))

results = run.mcmc.pipeline(locations = "South Africa",
                            steps = c(1, 2),
                            n.cores = N.CORES,
                            resume = RESUME,
                            merge = TRUE,
                            max.retries = 1)
print("Joint South Africa youth calibration (SABSSM ratio targets, tighter priors, down-weighted youth cascade) finished")
