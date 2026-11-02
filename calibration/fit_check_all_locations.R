################################################################################################
# Fit check only (no PDFs): every calibration target against each location's merged Step 2 simset - see fit.check()
# in calibration/mcmc_review.R. ~1-2 min per location. Writes results/fit_check_<SIMSET.DATE or "latest">/:
#   fit_check_all.csv (every target), fit_issues.csv (flagged, worst first), fit_summary.csv (flags by data type/stratum)
# SIMSET.DATE = NULL uses each location's newest merged simset; e.g. SIMSET.DATE = "2026-10-20" checks that run instead.
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")
source("calibration/mcmc_review.R")

if (!exists("LOCATIONS")) LOCATIONS = "all"
if (!exists("SIMSET.DATE")) SIMSET.DATE = NULL   # e.g. SIMSET.DATE = "2026-10-20" (set before sourcing) for the previous master calibration

if (identical(LOCATIONS, "all")) LOCATIONS = names(LOCATION.PRIOR.NAMES)
dir = file.path("results", paste0("fit_check_", if (is.null(SIMSET.DATE)) "latest" else SIMSET.DATE, "_checked_", Sys.Date()))
dir.create(dir, recursive = TRUE, showWarnings = FALSE)
fit.all = list()
for (location in LOCATIONS) {
    file = if (is.null(SIMSET.DATE)) find.latest.merged.simset(location) else {
        f = list.files(MCMC.RUNS.DIR, full.names = TRUE,
                       pattern = paste0("^simset_", gsub(".", "\\\\.", convert_string(location), fixed = TRUE), "_step2_chains[0-9]+_", SIMSET.DATE, "\\\\.Rdata$"))
        if (length(f) == 0) NA else f[1]
    }
    if (is.na(file)) { print(paste0(location, ": no merged simset", if (!is.null(SIMSET.DATE)) paste0(" from ", SIMSET.DATE))); next }
    print(paste0("==== ", location, ": ", basename(file), " ===="))
    LOCATION <<- location
    simset = load.saved.object(file)
    fit = tryCatch(fit.check(simset, location), error = function(e){ print(paste0("  fit check failed: ", conditionMessage(e))); NULL })
    if (!is.null(fit)) { fit$simset = basename(file); fit.all[[location]] = fit; print.fit.flags(fit) }
    rm(simset); gc()
}
fit.all = do.call(rbind, fit.all)
write.csv(fit.all, file.path(dir, "fit_check_all.csv"), row.names = FALSE)
flagged = fit.all[fit.all$flag != "", , drop = FALSE]
write.csv(flagged[order(-flagged$severity), ], file.path(dir, "fit_issues.csv"), row.names = FALSE)

# where the problems are: flagged / checked by data type and kind of stratum, across locations
fit.all$stratum = ifelse(fit.all$age == "All ages" & fit.all$sex == "both", "total",
                  ifelse(fit.all$sex == "both", "by age", ifelse(fit.all$age == "All ages", "by sex", "by age and sex")))
summary = aggregate(cbind(checked = 1, flagged = fit.all$flag != "") ~ data.type + stratum, data = fit.all, FUN = sum)
summary$pct.flagged = round(100 * summary$flagged / summary$checked)
write.csv(summary, file.path(dir, "fit_summary.csv"), row.names = FALSE)
print("Flagged targets by data type and stratum (all locations):"); print(summary, row.names = FALSE)
by.location = aggregate(cbind(checked = 1, flagged = fit.all$flag != "") ~ location, data = fit.all, FUN = sum)
print("By location:"); print(by.location, row.names = FALSE)
print(paste0("Results in ", normalizePath(dir)))
