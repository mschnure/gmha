################################################################################################
# NEXTGEN: rerun the AIDS 2026 LAI-ART scenarios on the joint South Africa calibration with the SABSSM
# 15-24 targets and 15-24 multipliers/slopes (calibration/run_youth_joint_calibration_sa_sabssm.R),
# then compare three versions: AIDS 2026 abstract, 10/21 joint calibration, this calibration.
#
# Same scenarios and outputs as future_projections/rerun_lai_joint_calibration.R, written to
# cached/nextgen_lai_rerun_sabssm/ (figures tagged "sabssm"), plus
#   lai_comparison_3way_<date>.csv
# Run in RStudio (source this file) or Terminal; ~20-40 min.
################################################################################################
new.files = list.files("mcmc_runs/nextgen_sabssm", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)
if(length(new.files)==0) stop("No merged simset in mcmc_runs/nextgen_sabssm/")
SIMSET.FILE = max(new.files)
OUT.DIR = file.path("cached", "nextgen_lai_rerun_sabssm")
FIGURE.TAG = "sabssm"
source("future_projections/rerun_lai_joint_calibration.R")

## 3-way comparison: AIDS 2026 vs 10/21 joint vs SABSSM calibration (medians)
PREVIOUS.TABLE = "cached/nextgen_lai_rerun/lai_results_table_2026-10-21.csv"
previous = read.csv(PREVIOUS.TABLE)[, c("scenario","metric","median")]
names(previous)[3] = "joint.10.21.median"
three = merge(comparison[, c("scenario","metric","aids2026")], previous, by = c("scenario","metric"), sort = F)
three = merge(three, long[, c("scenario","metric","median","lower","upper")], by = c("scenario","metric"), sort = F)
names(three)[names(three) %in% c("median","lower","upper")] = c("sabssm.median","sabssm.lower","sabssm.upper")
three$pct.vs.aids2026 = round(100*(three$sabssm.median - three$aids2026)/three$aids2026, 1)
three$pct.vs.joint.10.21 = round(100*(three$sabssm.median - three$joint.10.21.median)/three$joint.10.21.median, 1)
three = three[order(match(three$scenario, c("no.int", SCENARIOS))), ]
write.csv(three, file.path(OUT.DIR, paste0("lai_comparison_3way_", Sys.Date(), ".csv")), row.names = F)
print("AIDS 2026 vs 10/21 joint vs SABSSM calibration (medians):")
print(three[, c("scenario","metric","aids2026","joint.10.21.median","sabssm.median","pct.vs.aids2026","pct.vs.joint.10.21")],
      digits = 4, row.names = F)
