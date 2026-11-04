################################################################################################
# NEXTGEN (11/04/26): sensitivity analysis for the LAI-ART results, in the style of Figure 4 of Schnure et al. (AIDS 2023,
# Kenya aging paper; old code: sensitivity_analysis_NEW.R):
#   outcome     = % of all new infections averted, 2022-2030, one value per simulation (default: 100% offered, no oral
#                 suppression requirement = "all.direct.100")
#   covariates  = the calibrated parameters of each simulation (simset@parameters)
#   1. PRCCs: partial rank correlation of each parameter with the outcome, holding the others fixed (rank-transform
#      everything, regress the parameter and the outcome on all the other parameters, correlate the residuals - the same
#      quantity as epiR::epi.prcc; written out here so no extra package is needed), with 95% CIs
#   2. low/high: outcome in the 20% of simulations with the lowest vs highest value of each parameter (median, IQR, 95%)
#   3. Figure: the top N.PLOT parameters, ordered by the low-high difference in median outcome, labelled with their PRCC,
#      dashed line = median of all simulations (as in Figure 4)
#   4. Scatterplots of the outcome against each plotted parameter, to check the relationships are monotonic (PRCCs assume it)
# Uses the saved LAI rerun (future_projections/rerun_lai_sabssm_calibration.R) - no new simulations.
#
#   source("future_projections/lai_sensitivity_analysis.R")
#   (before sourcing, optionally set SCENARIO, OUTCOME = "percent.averted.all" or "percent.averted.15.24", N.PLOT, NEW.RUN.DIR)
################################################################################################
library(ggplot2)
if(!exists("NEW.RUN.DIR")) NEW.RUN.DIR = "mcmc_runs/nextgen_transmission_anchor"
if(!exists("SCENARIO")) SCENARIO = "all.direct.100"          # 100% offered, no suppression requirement
if(!exists("OUTCOME")) OUTCOME = "percent.averted.all"       # or "percent.averted.15.24"
if(!exists("N.PLOT")) N.PLOT = 10                            # parameters in the figure
if(!exists("QUARTILE")) QUARTILE = 0.20                    # lowest / highest 20% of simulations (Figure 4 used 25%)
OUT.DIR = file.path("cached", sub("^mcmc_runs/nextgen_", "nextgen_lai_rerun_", NEW.RUN.DIR))
newest = function(dir, pattern){
    f = list.files(dir, pattern = pattern, full.names = TRUE)
    if(length(f)==0) stop(paste0("Nothing matching ", pattern, " in ", dir))
    f[order(sub(".*_([0-9-]+)\\.Rdata$", "\\1", f))][length(f)]
}
simset.file = newest(NEW.RUN.DIR, "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$")
results.file = newest(OUT.DIR, "^full\\.results\\.array_[0-9-]+\\.Rdata$")
print(paste0("Parameters: ", simset.file)); print(paste0("Outcomes: ", results.file))

## outcome, one value per simulation
load(results.file) # full.results.array [year, age, sex, outcome, sim, intervention]
if(!exists("IMPACT.YEARS")) IMPACT.YEARS = 2025:2035   # 11/04/26: was 2022:2030 (program now 2025-2030)
YEARS = as.character(IMPACT.YEARS); IMPACT.LABEL = paste0(min(IMPACT.YEARS), "-", max(IMPACT.YEARS))
ages = if(OUTCOME == "percent.averted.15.24") c("15-19","20-24") else dimnames(full.results.array)$age
inc = apply(full.results.array[YEARS, ages,, "incidence",, c("no.int", SCENARIO), drop = FALSE], c("sim","intervention"), sum)
outcome = 100*(inc[,"no.int"] - inc[, SCENARIO])/inc[,"no.int"]
rm(full.results.array); invisible(gc())

## covariates: calibrated parameters (drop any that don't vary)
simset = get(load(simset.file)[1])
if(simset@n.sim != length(outcome)) stop("The simset and the LAI results have different numbers of simulations")
params = simset@parameters
params = params[, apply(params, 2, function(x) length(unique(x)) > 1), drop = FALSE]
rm(simset); invisible(gc())
print(paste0(SCENARIO, ", ", OUTCOME, ": median ", round(median(outcome), 2), "% (", round(quantile(outcome, .025), 2), "-",
             round(quantile(outcome, .975), 2), "%), ", length(outcome), " simulations, ", ncol(params), " varying parameters"))

## 1. PRCCs
prcc = function(X, y){
    R = apply(X, 2, rank); ry = rank(y); n = nrow(R); k = ncol(R)
    est = sapply(seq_len(k), function(j){
        Z = cbind(1, R[, -j, drop = FALSE])
        rx = qr.resid(qr(Z), R[, j]); re = qr.resid(qr(Z), ry)
        cor(rx, re)
    })
    se = 1/sqrt(n - 3 - (k - 1))   # Fisher z, k-1 other variables held fixed
    z = atanh(est)
    data.frame(parameter = colnames(X), prcc = est, lower = tanh(z - 1.96*se), upper = tanh(z + 1.96*se), row.names = NULL)
}
prccs = prcc(params, outcome)

## 2. low / high quartiles
n.q = round(QUARTILE*length(outcome))
probs = c(0.025, 0.25, 0.5, 0.75, 0.975)
lh = do.call(rbind, lapply(colnames(params), function(p){
    o = order(params[, p])
    low = quantile(outcome[o[1:n.q]], probs); high = quantile(outcome[rev(o)[1:n.q]], probs)
    rbind(data.frame(parameter = p, subset = "low",  t(low)),
          data.frame(parameter = p, subset = "high", t(high)))
}))
names(lh)[3:7] = c("lower.2","lower.1","median","upper.1","upper.2")
diffs = sapply(colnames(params), function(p) lh$median[lh$parameter==p & lh$subset=="high"] - lh$median[lh$parameter==p & lh$subset=="low"])
summary = merge(prccs, data.frame(parameter = names(diffs), low.high.difference = diffs), by = "parameter")
summary$median.low = sapply(summary$parameter, function(p) lh$median[lh$parameter==p & lh$subset=="low"])
summary$median.high = sapply(summary$parameter, function(p) lh$median[lh$parameter==p & lh$subset=="high"])
summary = summary[order(-abs(summary$low.high.difference)), ]
options(width = 200)
cat(paste0("\n==== Parameters ordered by the difference in median outcome, highest vs lowest ", round(100*QUARTILE), "% of simulations (top 20) ====\n"))
print(format(head(summary, 20), digits = 2), row.names = FALSE)
cat("\n==== Largest PRCCs (top 15) ====\n")
print(format(head(prccs[order(-abs(prccs$prcc)), ], 15), digits = 2), row.names = FALSE)

## 3. Figure 4-style plot
label.parameter = function(p){
    special = c(age.15.24.disengagement.multiplier = "Youth (15-24) dropout multiplier",
                age.15.24.awareness.multiplier = "Youth (15-24) testing multiplier",
                age.15.24.engagement.multiplier = "Youth (15-24) ART start multiplier",
                age.15.24.suppression.multiplier = "Youth (15-24) suppression multiplier",
                unsuppressed.disengagement.rates = "Dropout, unsuppressed on ART",
                suppressed.disengagement.rates = "Dropout, suppressed on ART",
                unsuppression.rates = "Loss of suppression (oral ART)",
                female.to.male.multiplier = "Female-to-male transmission",
                age.assortativity = "Age assortativity of partnerships",
                log.OR.testing.intercept = "Testing (intercept)", log.OR.testing.slope = "Testing trend",
                log.OR.engagement.slope = "ART start trend", log.OR.suppression.slope = "Suppression trend",
                log.OR.15.24.engagement.slope = "Youth (15-24) ART start trend", log.OR.15.24.suppression.slope = "Youth (15-24) suppression trend",
                log.OR.0.14.engagement.slope = "Child (0-14) ART start trend", log.OR.0.14.suppression.slope = "Child (0-14) suppression trend",
                age.0.14.disengagement.multiplier = "Child (0-14) dropout multiplier", age.0.14.awareness.multiplier = "Child (0-14) testing multiplier",
                age.0.14.engagement.multiplier = "Child (0-14) ART start multiplier", age.0.14.suppression.multiplier = "Child (0-14) suppression multiplier",
                male.awareness.multiplier = "Testing, male vs female", male.engagement.multiplier = "ART start, male vs female",
                male.suppression.multiplier = "Suppression, male vs female",
                hiv.specific.mortality.rates.0 = "HIV mortality, 1990", hiv.specific.mortality.rates.1 = "HIV mortality, 2005",
                hiv.specific.mortality.rates.2 = "HIV mortality, 2020")
    knots = c("0" = "1990", "1" = "1997", "2" = "2008", "3" = "2018", "4" = "2040")
    if(p %in% names(special)) return(special[[p]])
    if(grepl("^trate\\.[0-4]$", p)) return(paste0("Transmission rate, ", knots[[sub("trate\\.", "", p)]]))
    m = regmatches(p, regexec("^age\\.(.+)\\.transmission\\.multiplier\\.([0-3])$", p))[[1]]
    if(length(m) == 3) return(paste0("Transmission, age ", gsub("\\.to\\.", "-", sub("\\.and\\.over", "+", m[2])), ", ", knots[[m[3]]]))
    gsub("\\.", " ", p)
}
top = summary$parameter[1:min(N.PLOT, nrow(summary))]
labels = sapply(top, function(p) sprintf("%s\n(PRCC: %.2f)", label.parameter(p), summary$prcc[summary$parameter == p]))
d = lh[lh$parameter %in% top, ]
d$label = factor(labels[d$parameter], levels = rev(labels))
d$subset = factor(d$subset, levels = c("low","high"))
cols = scales::hue_pal()(2)
outcome.label = if(OUTCOME == "percent.averted.15.24") "infections averted among ages 15-24" else "all new infections averted"
p = ggplot(d) +
    geom_boxplot(aes(y = label, xmiddle = median, xlower = lower.1, xupper = upper.1, xmin = lower.2, xmax = upper.2, fill = subset),
                 stat = "identity", position = "dodge") +
    geom_vline(xintercept = median(outcome), linetype = "dashed") +
    scale_fill_manual(name = element_blank(), values = cols,
                      labels = c(low = paste0("Simulations with the lowest ", round(100*QUARTILE), "% of parameter values"),
                                 high = paste0("Simulations with the highest ", round(100*QUARTILE), "% of parameter values"))) +
    labs(x = paste0("\n% of ", outcome.label, ", ", IMPACT.LABEL), y = "") +
    theme(panel.background = element_blank(), legend.position = "bottom", legend.direction = "vertical",
          legend.text = element_text(size = 10), axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10))
fig.file = file.path(OUT.DIR, paste0("sensitivity_", SCENARIO, "_", OUTCOME, "_", Sys.Date(), ".jpeg"))
jpeg(fig.file, width = 1600, height = 250 + 110*length(top), res = 200); print(p); dev.off()

## 4. monotonicity check
pdf(file.path(OUT.DIR, paste0("sensitivity_scatter_", SCENARIO, "_", OUTCOME, "_", Sys.Date(), ".pdf")), width = 10, height = 8)
par(mfrow = c(3, 4))
for(pp in top) plot(params[, pp], outcome, pch = 16, cex = 0.4, col = "#1F6F8B66", xlab = label.parameter(pp), ylab = "% averted",
                    main = sprintf("PRCC %.2f", summary$prcc[summary$parameter == pp]), log = if(all(params[, pp] > 0)) "x" else "")
dev.off()

write.csv(summary, file.path(OUT.DIR, paste0("sensitivity_", SCENARIO, "_", OUTCOME, "_", Sys.Date(), ".csv")), row.names = FALSE)
print(paste0("Figure: ", fig.file, "; table and scatterplots (monotonicity check) in ", OUT.DIR))
