################################################################################################
# NEXTGEN (10/29/26): compare the transmission parameters of AIDS 2026 and the current calibration:
#   overall transmission rates (trate.0-4), age transmission multipliers by time point (.0-.3), sex and assortativity,
#   and the implied age-specific transmission level = trate at each time point x age multiplier at that time point.
# Time points: trate.0 / multiplier .0 = 1990 and before, .1 = 1997, .2 = 2008, .3 = 2018 (trate.4 = 2040).
# Output: printed tables (medians, 95% intervals) and cached/nextgen_lai_rerun_youth_dropout_prior/transmission_parameters_<date>.csv
################################################################################################
source("model/run_systematic.R")
SIMSETS = c("AIDS 2026" = "cached/simset.noint_25_1yr_2026-01-22.Rdata",
            "Current" = max(list.files("mcmc_runs/nextgen_youth_dropout_prior", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)))
AGES = c("age.15.to.19","age.20.to.29","age.30.to.39","age.40.to.49","age.50.and.over")
TIMES = c(".0" = "1990", ".1" = "1997", ".2" = "2008", ".3" = "2018")
TRATE = c(".0" = "trate.0", ".1" = "trate.1", ".2" = "trate.2", ".3" = "trate.3")
q = function(x) if(is.null(x)) rep(NA, 3) else quantile(x, c(0.5, 0.025, 0.975), na.rm = T)
fmt = function(v) ifelse(is.na(v[1]), "-", sprintf("%.3f (%.3f-%.3f)", v[1], v[2], v[3]))

rows = list()
for(cal in names(SIMSETS)){
    P = get(load(SIMSETS[[cal]])[1])@parameters
    get = function(nm) if(nm %in% colnames(P)) P[, nm] else NULL
    for(nm in c("trate.0","trate.1","trate.2","trate.3","trate.4","proportion.trate.change.by.3.5","female.to.male.multiplier","age.assortativity"))
        rows[[length(rows)+1]] = data.frame(calibration = cal, parameter = nm, time = "", value = fmt(q(get(nm))), median = q(get(nm))[1])
    for(a in AGES) for(t in names(TIMES)){
        m = get(paste0(a, ".transmission.multiplier", t))
        rows[[length(rows)+1]] = data.frame(calibration = cal, parameter = paste0(a, " multiplier"), time = TIMES[[t]], value = fmt(q(m)), median = q(m)[1])
        tr = get(TRATE[[t]])
        eff = if(is.null(m) || is.null(tr)) NULL else tr*m
        rows[[length(rows)+1]] = data.frame(calibration = cal, parameter = paste0(a, " trate x multiplier"), time = TIMES[[t]], value = fmt(q(eff)), median = q(eff)[1])
    }
}
d = do.call(rbind, rows)
wide = reshape(d[, c("calibration","parameter","time","value")], idvar = c("parameter","time"), timevar = "calibration", direction = "wide")
names(wide) = sub("^value\\.", "", names(wide))
med = reshape(d[, c("calibration","parameter","time","median")], idvar = c("parameter","time"), timevar = "calibration", direction = "wide")
wide$ratio.current.vs.aids2026 = round(med[["median.Current"]]/med[["median.AIDS 2026"]], 2)
dir.create("cached/nextgen_lai_rerun_youth_dropout_prior", showWarnings = F)
write.csv(wide, file.path("cached/nextgen_lai_rerun_youth_dropout_prior", paste0("transmission_parameters_", Sys.Date(), ".csv")), row.names = F)
options(width = 200)
print("Overall transmission parameters (median, 95% interval):")
print(wide[wide$time == "", ], row.names = F)
for(a in AGES){ cat("\n", a, ":\n", sep = ""); print(wide[grepl(paste0("^", a, " "), wide$parameter), ], row.names = F) }
