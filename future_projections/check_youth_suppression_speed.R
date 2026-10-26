################################################################################################
# NEXTGEN: how fast do engaged-unsuppressed youth become suppressed (baseline, no LAI-ART)?
# Oct 21 joint calibration vs SABSSM 15-24 calibration (Oct 23), ages 15-24 vs 25-49, 2017/2022/2030.
# From the calibrated rates in each simulation (sim$parameters, re-mapped if not stored):
#   suppression rate (per year, engaged-unsuppressed -> suppressed), unsuppression (rebound) rate,
#   unsuppressed disengagement rate, engagement rate (aware -> on ART), testing rate
# and what they imply:
#   P(suppressed within 6 months / 1 year), if nothing else happens
#   median time to suppression (months) = 12*log(2)/rate
#   P(suppress before disengaging) = s/(s + d)
#   long-run share suppressed among those on ART ~ s/(s + u)
# Output: cached/nextgen_lai_rerun_sabssm/youth_suppression_speed_<date>.csv, printed summary
################################################################################################
source("model/run_systematic.R")
if(!exists("CHECK.CAP")) CHECK.CAP = FALSE # TRUE: re-map rates with options(gmha.cap.youth.cascade = TRUE)
options(gmha.cap.youth.cascade = CHECK.CAP)
N.SIMS = 200
YEARS = c("2017","2022","2030")
AGE.GROUPS = list("15-24" = c("15-19","20-24"), "25-49" = c("25-29","30-34","35-39","40-44","45-49"))
SIMSETS = c("Oct 21 joint" = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata",
            "Oct 22 SABSSM" = max(list.files("mcmc_runs/nextgen_sabssm", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)))
ratio.files = list.files("mcmc_runs/nextgen_sabssm_ratio", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)
if(length(ratio.files) > 0) SIMSETS["SABSSM ratio"] = max(ratio.files)
RATES = c(suppression = "SUPPRESSION.RATES", unsuppression = "UNSUPPRESSION.RATES",
          disengagement.unsuppressed = "UNSUPPRESSED.DISENGAGEMENT.RATES", disengagement.suppressed = "SUPPRESSED.DISENGAGEMENT.RATES",
          engagement = "ENGAGEMENT.RATES", testing = "TESTING.RATES")

get.tvp = function(sim, i, simset){
    p = sim$parameters
    if(is.null(p$time.varying.parameters) || CHECK.CAP){ # with the cap on, re-map so the stored (uncapped) rates aren't used
        p = map.model.parameters(create.model.parameters(location = "South Africa"), location = "South Africa",
                                 sampled.parameters = simset@parameters[i,])
    }
    p$time.varying.parameters
}
rate.at = function(tvp, name, year, ages){
    x = tvp[[name]]; t = x$times
    v = if(any(t == as.numeric(year))) x$values[[which(t == as.numeric(year))]] else x$values[[max(which(t <= as.numeric(year)))]]
    mean(v[ages,,])   # average over sexes/subgroups (equal weights)
}

rows = list()
for(cal in names(SIMSETS)){
    simset = get(load(SIMSETS[[cal]])[1])
    set.seed(1); idx = sort(sample(seq_len(simset@n.sim), min(N.SIMS, simset@n.sim)))
    print(paste0(cal, ": ", length(idx), " simulations"))
    for(i in idx){
        tvp = get.tvp(simset@simulations[[i]], i, simset)
        for(y in YEARS) for(a in names(AGE.GROUPS)){
            r = sapply(RATES, function(nm) rate.at(tvp, nm, y, AGE.GROUPS[[a]]))
            rows[[length(rows)+1]] = data.frame(calibration = cal, sim = i, year = y, age = a, t(r))
        }
    }
    rm(simset); invisible(gc())
}
d = do.call(rbind, rows)
d$p.suppressed.6mo = 1 - exp(-d$suppression/2)
d$p.suppressed.1yr = 1 - exp(-d$suppression)
d$median.months.to.suppression = 12*log(2)/d$suppression
d$p.suppress.before.disengage = d$suppression/(d$suppression + d$disengagement.unsuppressed)
d$longrun.suppressed.of.on.art = d$suppression/(d$suppression + d$unsuppression)
d$p.start.art.1yr.if.aware = 1 - exp(-d$engagement)
# Retention (10/26/26): someone who starts ART (engaged, unsuppressed) - chance they have dropped out of care at least
# once by 6 / 12 months. States: unsuppressed on ART (U), suppressed on ART (S), dropped out (D, absorbing; ignores
# re-engagement and death). Compare with clinic data, e.g. ~16% disengaged by 6 months and ~23% by 12 months among
# adults starting ART in SA public clinics from 2018 (medRxiv 2023.06.13.23291348).
p.dropped.out = function(s, du, u, ds, t){
    Q = matrix(c(-(s+du), s, du,
                 u, -(u+ds), ds,
                 0, 0, 0), nrow = 3, byrow = T)
    k = 30; M = diag(3) + Q*t/2^k        # matrix exponential by scaling and squaring
    for(j in 1:k) M = M %*% M
    M[1,3]
}
d$p.dropped.out.6mo.after.start = mapply(p.dropped.out, d$suppression, d$disengagement.unsuppressed, d$unsuppression, d$disengagement.suppressed, 0.5)
d$p.dropped.out.12mo.after.start = mapply(p.dropped.out, d$suppression, d$disengagement.unsuppressed, d$unsuppression, d$disengagement.suppressed, 1)
d$p.suppressed.dropped.out.1yr = 1 - exp(-d$disengagement.suppressed)

measures = c("suppression","unsuppression","disengagement.unsuppressed","engagement","testing",
             "p.suppressed.6mo","p.suppressed.1yr","median.months.to.suppression",
             "p.suppress.before.disengage","longrun.suppressed.of.on.art","p.start.art.1yr.if.aware",
             "disengagement.suppressed","p.dropped.out.6mo.after.start","p.dropped.out.12mo.after.start","p.suppressed.dropped.out.1yr")
summ = do.call(rbind, lapply(split(d, list(d$calibration, d$year, d$age), drop = T), function(x){
    q = sapply(measures, function(m) quantile(x[[m]], c(0.5, 0.025, 0.975)))
    data.frame(calibration = x$calibration[1], year = x$year[1], age = x$age[1],
               measure = measures, median = q[1,], lower = q[2,], upper = q[3,], row.names = NULL)
}))
summ = summ[order(summ$measure, summ$age, summ$year, summ$calibration), ]
dir.create("cached/nextgen_lai_rerun_sabssm_ratio", showWarnings = FALSE)
write.csv(summ, file.path("cached/nextgen_lai_rerun_sabssm_ratio", paste0("youth_suppression_speed_", Sys.Date(), ".csv")), row.names = F)

wide = reshape(summ[, c("calibration","year","age","measure","median")], idvar = c("measure","age","year"),
               timevar = "calibration", direction = "wide")
names(wide) = sub("median.", "", names(wide))
print("Medians (rates per year; probabilities; months):")
print(wide[wide$measure %in% c("suppression","p.suppressed.6mo","median.months.to.suppression",
                               "p.suppress.before.disengage","longrun.suppressed.of.on.art",
                               "engagement","p.start.art.1yr.if.aware","unsuppression","disengagement.unsuppressed","testing",
                               "disengagement.suppressed","p.dropped.out.6mo.after.start","p.dropped.out.12mo.after.start",
                               "p.suppressed.dropped.out.1yr"), ],
      digits = 3, row.names = F)
