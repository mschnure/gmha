################################################################################################
# NEXTGEN (10/26/26): same as calibration/run_youth_joint_calibration_sa_sabssm_ratio.R, plus youth multipliers on
# the other cascade rates - unsuppressed disengagement, suppressed disengagement, unsuppression - for ages 0-14 and
# 15-24 (6 new parameters; 16 youth parameters in all). Reason: in the joint calibrations the all-ages unsuppressed
# disengagement rate rose from ~0.3/yr (master) to 1.4-3.4/yr, giving ~45-65% dropout within 12 months of starting
# ART (data: ~14-23%), most likely to fit the youth cascades; youth multipliers let youth churn more without that.
#
# Original description:
# NEXTGEN: joint recalibration of South Africa with 15-24 cascade targets built from the national household
# surveys as YOUTH-TO-ADULT RATIOS x the UNAIDS 15+ values (SABSSM 2017 total + by sex, 2022 total; method = "ratio"
# in data_manager/add_sabssm_youth_cascade.R). Replaces the 10/22 run, which fit the survey values as they are and
# ended up with youth starting ART / suppressing faster than adults after ~2020 (see
# future_projections/check_youth_suppression_speed.R).
# Joint: all parameters recalibrated together. 10/26/26: FRESH START - Step 1 + Step 2 (see below), not a
# warm start. Otherwise as calibration/run_youth_joint_calibration_sa.R, except:
#   - 5 new parameters for model ages 15-19 and 20-24, applied like the 0-14 ones:
#     age.15.24.awareness/engagement/suppression.multiplier (prior lognormal median 1, 95% range 1/8-8) and
#     log.OR.15.24.engagement/suppression.slope (prior normal mean 0, 95% range log(1/4)-log(4)); 10 youth parameters in all
#   - data manager: cached/data.manager_global_revised_ages_lai_2026-10-23.Rdata (loaded by source_code.R)
#   - its own folders (so the 10/21 run's complete caches aren't picked up as "already done"):
#     mcmc_cache/nextgen_sabssm_ratio_retention, mcmc_runs/nextgen_sabssm_ratio_retention, calibration/starting_values/nextgen_sabssm_ratio_retention
#
# HOW TO RUN (Windows, in the gmha_nextgen worktree):
#     cd ../gmha_nextgen && git pull
#     RStudio: Jobs tab > Start Background Job > script = this file, working directory = gmha_nextgen.
#     Progress: mcmc_runs/nextgen_sabssm_ratio_retention/logs/
# TO RESUME after a crash/restart: set RESUME = TRUE below
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")

if(!identical(attr(DATA.MANAGER, "sabssm.youth.cascade.method"), "ratio"))
    stop("DATA.MANAGER doesn't have the ratio-based 15-24 SABSSM targets - check the load line in source_code.R")

MCMC.CACHE.DIR = "mcmc_cache/nextgen_sabssm_ratio_retention"
MCMC.RUNS.DIR = "mcmc_runs/nextgen_sabssm_ratio_retention"
STARTING.VALUES.DIR = "calibration/starting_values/nextgen_sabssm_ratio_retention"
for(d in c(MCMC.CACHE.DIR, MCMC.RUNS.DIR, STARTING.VALUES.DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

RESUME = FALSE
RESUME = RESUME || "resume" %in% commandArgs(trailingOnly = TRUE)
N.CORES = 4

# FRESH START (10/26/26): the full two-step pipeline, as for master - Step 1 (1 chain, 25,000 iterations,
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
print("Joint South Africa youth calibration (SABSSM 15-24 ratio targets + youth retention multipliers) finished")
