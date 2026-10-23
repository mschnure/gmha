################################################################################################
# NEXTGEN: review the joint South Africa calibration with the SABSSM 15-24 targets and the 15-24
# multipliers/slopes (calibration/run_youth_joint_calibration_sa_sabssm.R) against the 10/21 joint
# calibration it was warm-started from. Saves the plots to a PDF (1 = 10/21 joint, 2 = new).
################################################################################################
source("model/run_systematic.R")

# newest merged simset copied from the remote
if(!exists("NEW.RUN.DIR")) NEW.RUN.DIR = "mcmc_runs/nextgen_sabssm_ratio" # set to "mcmc_runs/nextgen_sabssm" for the 10/22 run
new.files = list.files(NEW.RUN.DIR, pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)
if(length(new.files)==0) stop(paste0("Copy the merged simset into ", NEW.RUN.DIR, "/ first"))
NEW.SIMSET.FILE = max(new.files)
OLD.SIMSET.FILE = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata"
PLOT.FILE = file.path(NEW.RUN.DIR, paste0("sabssm_youth_calibration_review_", Sys.Date(), ".pdf"))

YOUTH.PARAMS = c("age.0.14.awareness.multiplier","age.0.14.engagement.multiplier","age.0.14.suppression.multiplier",
                 "log.OR.0.14.engagement.slope","log.OR.0.14.suppression.slope",
                 "age.15.24.awareness.multiplier","age.15.24.engagement.multiplier","age.15.24.suppression.multiplier",
                 "log.OR.15.24.engagement.slope","log.OR.15.24.suppression.slope")

simset.new = get(load(NEW.SIMSET.FILE)[1])
simset.old = get(load(OLD.SIMSET.FILE)[1])
print(paste0("New (", NEW.SIMSET.FILE, "): ", simset.new@n.sim, " distinct simulations"))
print(paste0("10/21 joint: ", simset.old@n.sim, " distinct simulations"))

## Youth parameters (weighted quantiles)
wq = function(x, w, p){ o = order(x); cw = cumsum(w[o])/sum(w); sapply(p, function(pp) x[o][which(cw >= pp)[1]]) }
youth.summary = sapply(YOUTH.PARAMS, function(p) wq(simset.new@parameters[,p], simset.new@weights, c(0.025,0.5,0.975)))
rownames(youth.summary) = c("2.5%","50%","97.5%")
print("Youth parameters, new calibration:"); print(round(t(youth.summary), 3))

## How far the other parameters moved (new median - old median, as a fraction of old 95% interval width)
shared = setdiff(intersect(colnames(simset.new@parameters), colnames(simset.old@parameters)), YOUTH.PARAMS)
shift = sapply(shared, function(p){
    m = wq(simset.old@parameters[,p], simset.old@weights, c(0.025,0.5,0.975))
    j = wq(simset.new@parameters[,p], simset.new@weights, 0.5)
    (j - m[2]) / max(m[3] - m[1], 1e-12)
})
print("Parameters that moved most:"); print(round(head(shift[order(-abs(shift))], 15), 2))

## 15-24 cascade, model median vs SABSSM targets (aware of PWH; on ART and suppressed of aware)
wmed = function(x, w) wq(x, w, 0.5)
youth.cascade = function(simset, yrs = c(2017, 2022)){
    vals = sapply(seq_len(simset@n.sim), function(i){
        sim = simset@simulations[[i]]
        get = function(type) as.numeric(extract.data(sim, type, years = yrs, ages = c("15-19","20-24"), keep.dimensions = "year"))
        prev = get("prevalence"); aware = get("awareness")
        c(aware/prev, get("engagement")/aware, get("suppression")/aware)
    })
    rv = apply(vals, 1, wmed, w = simset@weights)
    names(rv) = paste0(rep(c("aware.","on.art.","suppressed."), each = length(yrs)), yrs)
    rv
}
# targets as stored in the data manager (aware of PWH; on ART and suppressed of aware)
tg = function(type, yr) DATA.MANAGER[[type]]$year.age.location[yr, "15-24", "South Africa"]
targets = c(aware.2017 = tg("awareness","2017"), aware.2022 = tg("awareness","2022"),
            on.art.2017 = tg("engagement","2017"), on.art.2022 = tg("engagement","2022"),
            suppressed.2017 = tg("suppression","2017"), suppressed.2022 = tg("suppression","2022"))
print(paste0("Targets: method = ", attr(DATA.MANAGER, "sabssm.youth.cascade.method")))
print("15-24 cascade (medians) vs SABSSM:")
print(round(rbind(targets = targets, joint.10.21 = youth.cascade(simset.old), new = youth.cascade(simset.new)), 3))

pdf(PLOT.FILE, width = 11, height = 8.5)
invisible(simplot(simset.old, simset.new, years = 2000:2030,
                  data.types = c('awareness','engagement','suppression'),
                  facet.by = 'age', ages = c('0-14','15-24','15+'), proportion = T))
invisible(tryCatch(simplot(simset.old, simset.new, years = 2000:2030,
                           data.types = c('awareness','engagement','suppression'),
                           facet.by = c('age','sex'), ages = c('15-24'), proportion = T),
                   error = function(e) print(paste0("Skipped 15-24 by sex plot: ", conditionMessage(e)))))
invisible(simplot(simset.old, simset.new, years = 1980:2030, data.types = c('incidence','prevalence')))
invisible(simplot(simset.old, simset.new, years = 1980:2030, data.types = c('incidence','prevalence'), facet.by = 'age'))
dev.off()
print(paste0("Plots saved to ", PLOT.FILE, " (1 = 10/21 joint, 2 = new with SABSSM 15-24 targets)"))
