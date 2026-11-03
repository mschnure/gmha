################################################################################################
# NEXTGEN (11/03/26): why does each LAI-ART user add less 15-24 suppression in 2030 now than in AIDS 2026?
# Uses the saved results arrays (no new simulations): AIDS 2026 (cached/full.results.array_2026-01-22.Rdata) vs the current
# run (cached/nextgen_lai_rerun_<CURRENT.RUN>/), plus the two calibrations' simsets for the retention parameters.
#
# Model facts this relies on (model/diffeq.R, model/diffeq_cpp.cpp): people on LAI-ART (lai_art) are always suppressed and have
# NO unsuppression or disengagement - their only exit is removal back to oral (engaged_suppressed) at 25+ (LAI.REMOVAL.RATES =
# 52/year from 2022). New LAI starts stop after 2027 (end.time in create_lai_interventions.R). So by 2030 the suppression gain is:
#   (people still on LAI) - (those of them who would have been suppressed on oral anyway) + (lasting effect on people who left LAI)
#
# Prints, for each scenario and calibration:
#   1. who starts LAI: total and % from unsuppressed (on oral ART unsuppressed or disengaged)
#   2. where they are by 2030: still on LAI vs aged out (per 1,000 who started)
#   3. the 2030 suppression gain (15-24): total, per 1,000 who started, per 1,000 unsuppressed who started, per person still on LAI
#   4. baseline room to gain: 15-24 suppressed (% of PWH) in 2022 and 2030 without LAI, unsuppressed 15-24 in 2030
#   5. how fast suppressed youth on oral ART drop out (baseline flows) and the calibrated retention parameters
#   6. the gain per 1,000 who started, year by year
# Source it:  source("future_projections/check_lai_suppression_gain.R")
################################################################################################
source("model/run_systematic.R")
if(!exists("CURRENT.RUN")) CURRENT.RUN = "transmission_anchor"
OUT.DIR = file.path("cached", paste0("nextgen_lai_rerun_", CURRENT.RUN))
arrays = list("AIDS 2026" = "cached/full.results.array_2026-01-22.Rdata",
              "Current" = max(list.files(OUT.DIR, pattern = "^full\\.results\\.array_[0-9-]+\\.Rdata$", full.names = TRUE)))
simsets = list("AIDS 2026" = "cached/simset.noint_25_1yr_2026-01-22.Rdata",
               "Current" = max(list.files(paste0("mcmc_runs/nextgen_", CURRENT.RUN), pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = TRUE)))
SCENARIOS = c("all.100" = "100%, requirement", "all.direct.100" = "100%, no requirement",
              "all.25" = "25%, requirement", "all.direct.25" = "25%, no requirement")
Y = as.character(2022:2030)
YOUTH = c("15-19","20-24")
med = function(x) median(x, na.rm = TRUE)
options(width = 200)

summary.rows = list(); by.year = list(); starts.by.year = list()
for(cal in names(arrays)){
    print(paste0(cal, ": ", arrays[[cal]]))
    arr = get(load(arrays[[cal]])[1])
    youth = function(outcome, int, years = Y) apply(arr[years, YOUTH,, outcome,, int, drop = FALSE], c("year","sim"), sum)   # year x sim
    all.ages = function(outcomes, int, years = Y) apply(arr[years,,, outcomes,, int, drop = FALSE], c("year","sim"), sum)

    prev.b = youth("prevalence", "no.int"); supp.b = youth("suppression", "no.int")
    dis.supp.b = youth("disengagement.suppressed", "no.int")
    for(s in names(SCENARIOS)){
        starts = all.ages(c("lai.art.es","lai.art.eu","lai.art.du"), s)              # LAI starts per year (all at 15-24)
        starts.unsupp = all.ages(c("lai.art.eu","lai.art.du"), s)
        on.lai = apply(all.ages("lai.art.total", s), 2, cumsum); rownames(on.lai) = Y  # on LAI (net of removal; ignores deaths)
        aged.out = colSums(all.ages("lai.removal", s))
        total.starts = colSums(starts); total.unsupp = colSums(starts.unsupp)
        gain = youth("suppression", s) - supp.b                                         # 15-24 suppressed, scenario - baseline
        summary.rows[[length(summary.rows) + 1]] = data.frame(
            calibration = cal, scenario = SCENARIOS[[s]],
            started.lai.k = med(total.starts)/1000,
            pct.started.from.unsuppressed = med(100*total.unsupp/total.starts),
            still.on.lai.2030.per.1000.started = med(1000*on.lai["2030",]/total.starts),
            aged.out.by.2030.per.1000.started = med(1000*aged.out/total.starts),
            gain.2030.k = med(gain["2030",])/1000,
            gain.2030.per.1000.started = med(1000*gain["2030",]/total.starts),
            gain.2030.per.1000.unsupp.started = med(1000*gain["2030",]/total.unsupp),
            gain.2030.per.person.on.lai = med(gain["2030",]/on.lai["2030",]),
            check.names = FALSE)
        if(s == "all.100"){
            by.year[[cal]] = round(apply(1000*gain/matrix(total.starts, nrow = length(Y), ncol = length(total.starts), byrow = TRUE), 1, med))
            starts.by.year[[cal]] = round(apply(starts, 1, med)/1000, 1)
        }
    }
    base = data.frame(calibration = cal,
                      pct.suppressed.of.pwh.2022 = med(100*supp.b["2022",]/prev.b["2022",]),
                      pct.suppressed.of.pwh.2030 = med(100*supp.b["2030",]/prev.b["2030",]),
                      unsuppressed.2030.k = med(prev.b["2030",] - supp.b["2030",])/1000,
                      pct.suppressed.leaving.care.per.year.2025 = med(100*dis.supp.b["2025",]/supp.b["2025",]))
    if(!exists("baseline.rows")) baseline.rows = list()
    baseline.rows[[cal]] = base
    rm(arr); invisible(gc())
}

cat("\n==== 1-3. LAI starts, where they are in 2030, and the 15-24 suppression gain in 2030 (medians) ====\n")
summ = do.call(rbind, summary.rows)
summ = summ[order(match(summ$scenario, SCENARIOS), summ$calibration), ]
print(format(summ, digits = 3), row.names = FALSE)

cat("\n==== 4-5. Baseline (no LAI), 15-24 ====\n")
print(format(do.call(rbind, baseline.rows), digits = 3), row.names = FALSE)

cat("\n==== 5. Calibrated retention parameters (median, 2.5%, 97.5%; yearly rates) ====\n")
pars = c("unsuppression.rates","suppressed.disengagement.rates","unsuppressed.disengagement.rates","age.15.24.disengagement.multiplier")
par.tab = sapply(names(simsets), function(cal){
    ss = get(load(simsets[[cal]])[1])
    sapply(pars, function(p) if(p %in% colnames(ss@parameters)) {
        q = quantile(ss@parameters[, p], c(.5, .025, .975)); sprintf("%.3f (%.3f-%.3f)", q[1], q[2], q[3]) } else "not a parameter")
})
print(noquote(par.tab))

cat("\n==== 6. 100%, requirement: LAI starts per year (thousands) and the 15-24 suppression gain per 1,000 who started, by year ====\n")
print(rbind(do.call(rbind, setNames(starts.by.year, paste0("starts (k): ", names(starts.by.year)))),
            do.call(rbind, setNames(by.year, paste0("gain per 1,000 started: ", names(by.year))))))

write.csv(summ, file.path(OUT.DIR, paste0("lai_suppression_gain_check_", Sys.Date(), ".csv")), row.names = FALSE)
print(paste0("Saved ", file.path(OUT.DIR, paste0("lai_suppression_gain_check_", Sys.Date(), ".csv"))))
