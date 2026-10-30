################################################################################################
# NEXTGEN (10/30/26): fresh joint recalibration of South Africa - everything as in the 10/28 run
# (calibration/run_youth_joint_calibration_sa_youth_dropout_prior.R: SABSSM ratio-based 15-24 cascade targets, 12 youth
# parameters, 15-24 dropout multiplier prior median 1.5 (~1.0-2.25), Step 1 50,000 iterations) PLUS
#   - master-anchored transmission priors: trate.0-4, the 20 age-by-time transmission multipliers, female.to.male.multiplier
#     and age.assortativity get lognormal priors centred on master's Oct 20 posterior median, ~2x master's posterior width
#     (calibration/make_master_anchored_transmission_prior.R -> calibration/prior_distributions/south_africa_transmission_anchor.Rdata)
#   - (once added to the data manager) UNAIDS 15-24 new infections and PLHIV by sex
# Why: the transmission structure isn't identified by the data (chains reach the same likelihood with different
# transmission parameters), so LAI impact moved between recalibrations. Anchoring keeps it near master's structure.
# Folders: mcmc_cache/nextgen_transmission_anchor, mcmc_runs/nextgen_transmission_anchor,
#          calibration/starting_values/nextgen_transmission_anchor
#
# HOW TO RUN (Windows, in the gmha_nextgen worktree, nextgen branch):
#     git pull (RStudio Git pane), then
#     RStudio: Jobs tab > Start Background Job > script = this file, working directory = gmha_nextgen.
#     Progress: MCMC.CACHE.DIR = "mcmc_cache/nextgen_transmission_anchor"; mcmc.progress(locations = "South Africa")
#     When done: RUN.DIR = "mcmc_runs/nextgen_transmission_anchor"; source("calibration/check_chain_agreement.R")
# TO RESUME after a crash/restart: set RESUME = TRUE below
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

if(!identical(attr(DATA.MANAGER, "sabssm.youth.cascade.method"), "ratio"))
    stop("DATA.MANAGER doesn't have the ratio-based 15-24 SABSSM targets - check the load line in source_code.R")

if(!isTRUE(SA.TRANSMISSION.ANCHORED))
    stop("South Africa's transmission priors aren't anchored to master - run calibration/make_master_anchored_transmission_prior.R, restart R, and check USE.SA.TRANSMISSION.ANCHOR = TRUE in calibration/mcmc_settings.R")
print("Transmission priors: anchored to master's Oct 20 South Africa calibration")

MCMC.CACHE.DIR = "mcmc_cache/nextgen_transmission_anchor"
MCMC.RUNS.DIR = "mcmc_runs/nextgen_transmission_anchor"
STARTING.VALUES.DIR = "calibration/starting_values/nextgen_transmission_anchor"
for(d in c(MCMC.CACHE.DIR, MCMC.RUNS.DIR, STARTING.VALUES.DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

RESUME = FALSE
RESUME = RESUME || "resume" %in% commandArgs(trailingOnly = TRUE)
N.CORES = 4

YOUTH.CASCADE.LIKELIHOOD.WEIGHT = NULL # cascade weights as before; e.g. c("0-14" = 0.25, "15-24" = 0.25) would down-weight the youth cascade
MCMC.STEP.SETTINGS$step1$n.iter = 50000                              # doubled (calibration/mcmc_settings.R: 25,000)
print(paste0("Youth cascade likelihood weight: ", if(is.null(YOUTH.CASCADE.LIKELIHOOD.WEIGHT)) "none (as before)" else
             paste(names(YOUTH.CASCADE.LIKELIHOOD.WEIGHT), YOUTH.CASCADE.LIKELIHOOD.WEIGHT, sep = " = ", collapse = ", "),
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
print("Joint South Africa youth calibration (master-anchored transmission priors) finished")
