################################################################################################
# NEXTGEN: rerun the AIDS 2026 LAI-ART scenarios on the joint South Africa calibration with the SABSSM
# 15-24 targets and 15-24 multipliers/slopes (calibration/run_youth_joint_calibration_sa_sabssm.R),
# then compare with the AIDS 2026 abstract.
#
# Same scenarios and outputs as future_projections/rerun_lai_joint_calibration.R, written to
# cached/nextgen_lai_rerun_sabssm/ (figures tagged "sabssm"), plus
#   lai_comparison_3way_<date>.csv
# Run in RStudio (source this file) or Terminal; ~20-40 min.
################################################################################################
if(!exists("NEW.RUN.DIR")) NEW.RUN.DIR = "mcmc_runs/nextgen_youth_dropout_prior" # 10/28 run (earlier: nextgen_sabssm_ratio_priors)
new.files = list.files(NEW.RUN.DIR, pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)
if(length(new.files)==0) stop(paste0("No merged simset in ", NEW.RUN.DIR, "/"))
SIMSET.FILE = max(new.files)
OUT.DIR = file.path("cached", sub("^mcmc_runs/nextgen_", "nextgen_lai_rerun_", NEW.RUN.DIR)) # e.g. cached/nextgen_lai_rerun_sabssm_ratio
FIGURE.TAG = sub("^mcmc_runs/nextgen_", "", NEW.RUN.DIR)
source("future_projections/rerun_lai_joint_calibration.R")

## Comparison with AIDS 2026 only (10/29/26): AIDS 2026 abstract medians vs this calibration (median, 95% interval)
cmp = merge(comparison[, c("scenario","metric","aids2026")], long[, c("scenario","metric","median","lower","upper")],
            by = c("scenario","metric"), sort = F)
names(cmp)[names(cmp) %in% c("median","lower","upper")] = c("current.median","current.lower","current.upper")
cmp$pct.vs.aids2026 = round(100*(cmp$current.median - cmp$aids2026)/cmp$aids2026, 1)
cmp = cmp[order(match(cmp$scenario, c("no.int", SCENARIOS))), ]
write.csv(cmp, file.path(OUT.DIR, paste0("lai_comparison_aids2026_vs_current_", Sys.Date(), ".csv")), row.names = F)
print(paste0("AIDS 2026 vs current calibration (", NEW.RUN.DIR, "):"))
print(cmp, digits = 4, row.names = F)
