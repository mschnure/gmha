################################################################################################
# Are some Step 2 chains stuck in a worse fit, and does it matter? (11/02/26)
# For each location, chain by chain:
#   1. log-likelihood (median over the kept second half, and best) and the gap to the best chain
#   2. fit: calibration targets flagged by the fit check (fit.check in calibration/mcmc_review.R), leaving out AIDS
#      deaths (known poor fit) and the 0-14 cascade (not a master target)
#   3. projections that matter for the papers: new infections and PLHIV in 2025/2030/2040, share of PLHIV aged 50+,
#      aware and suppressed (of aware) in 2025 and 2030 - medians, and each chain's difference from the best chain
#   4. the parameters that differ most between chains (spread of chain medians relative to the spread within chains)
# A chain is called "stuck" if its median log-likelihood is more than STUCK.GAP below the best chain's.
# If stuck chains clearly fit worse AND change the projections, one option is to re-merge without them:
#     merge.step2.chains("Tanzania", chains = c(1, 2))   (calibration/mcmc_pipeline.R; makes a new merged simset)
#
# Run on the machine with mcmc_runs/ (~3-5 min per location):
#     source("calibration/check_chain_modes.R")       (or set LOCATIONS first)
# Output: printed, and results/chain_modes_<date>/<location>_*.csv
################################################################################################
source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")
source("calibration/mcmc_review.R")

if (!exists("LOCATIONS")) LOCATIONS = c("Tanzania", "Zimbabwe", "Kenya", "Zambia", "r1.high")
if (identical(LOCATIONS, "all")) LOCATIONS = names(LOCATION.PRIOR.NAMES)
STUCK.GAP = 20
CHAINS = 1:4
dir = file.path("results", paste0("chain_modes_", Sys.Date()))
dir.create(dir, recursive = TRUE, showWarnings = FALSE)
options(width = 200)

projection.summary = function(sim){
    years = c(2025, 2030, 2040)
    by.age = function(dt) { x = extract.data(sim, data.type = dt, years = years, keep.dimensions = c("year","age")); x }
    inc = rowSums(by.age("incidence"))
    prev.age = by.age("prevalence"); prev = rowSums(prev.age)
    prev50 = rowSums(prev.age[, MODEL.TO.SURVEILLANCE.AGE.MAPPING[["50 and over"]], drop = FALSE])
    aware = rowSums(by.age("awareness")); supp = rowSums(by.age("suppression"))
    c(setNames(inc, paste0("new.infections.", years)),
      setNames(prev, paste0("plhiv.", years)),
      setNames(100 * prev50 / prev, paste0("pct.plhiv.50plus.", years)),
      setNames(100 * aware[1:2] / prev[1:2], paste0("pct.aware.", years[1:2])),
      setNames(100 * supp[1:2] / aware[1:2], paste0("pct.suppressed.of.aware.", years[1:2])))
}

all.summary = list()
for (location in LOCATIONS) {
    cat("\n================ ", location, " ================\n", sep = "")
    LOCATION <<- location
    mcmc.files = find.latest.mcmc.files(location, 2, CHAINS)
    simset.files = find.latest.simset.files(location, 2, CHAINS)
    if (any(is.na(mcmc.files)) || any(is.na(simset.files))) { print("  missing chain files - skipped"); next }

    ## 1. likelihoods
    ll = t(sapply(mcmc.files, function(f){
        m = load.light.mcmc(f); x = m@log.likelihoods[1, ]
        x = x[ceiling(length(x)/2):length(x)]
        c(median.loglik = median(x, na.rm = TRUE), best.loglik = max(x, na.rm = TRUE))
    }))
    ll = data.frame(chain = CHAINS, round(ll, 1), row.names = NULL)
    best.chain = ll$chain[which.max(ll$median.loglik)]
    ll$gap.to.best = round(ll$median.loglik - max(ll$median.loglik), 1)
    ll$stuck = ll$gap.to.best < -STUCK.GAP

    ## 2-4. per chain: fit, projections, parameters
    fit.rows = list(); proj = list(); pars = list()
    for (i in seq_along(CHAINS)) {
        simset = load.saved.object(simset.files[i])
        fit = tryCatch(fit.check(simset, location, max.sims = 100), error = function(e){ print(paste0("  fit check failed: ", conditionMessage(e))); NULL })
        if (!is.null(fit)) {
            fit = fit[fit$data.type != "hiv.mortality" & !(fit$age == "0-14" & fit$data.type %in% c("awareness","engagement","suppression")), ]
            fit$chain = CHAINS[i]
            fit.rows[[i]] = fit
        }
        idx = unique(round(seq(1, simset@n.sim, length.out = min(100, simset@n.sim))))
        p = t(sapply(simset@simulations[idx], projection.summary))
        proj[[i]] = apply(p, 2, median, na.rm = TRUE)
        pars[[i]] = simset@parameters
        rm(simset); gc()
    }
    fit.all = do.call(rbind, fit.rows)
    ll$targets.flagged = sapply(CHAINS, function(ch) sum(fit.all$chain == ch & fit.all$flag != ""))
    ll$targets.checked = sapply(CHAINS, function(ch) sum(fit.all$chain == ch))
    cat("\nChains (median log-likelihood over the kept half; gap to the best chain; targets flagged, excluding AIDS deaths and the 0-14 cascade):\n")
    print(ll, row.names = FALSE)

    # targets that fit badly in some chains but not in the best one
    worse = setdiff(unique(paste(fit.all$data.type, fit.all$age, fit.all$sex)[fit.all$flag != "" & fit.all$chain != best.chain]),
                    paste(fit.all$data.type, fit.all$age, fit.all$sex)[fit.all$flag != "" & fit.all$chain == best.chain])
    if (length(worse) > 0) {
        cat("\nTargets flagged in other chains but fine in the best chain (", best.chain, "):\n", sep = "")
        key = paste(fit.all$data.type, fit.all$age, fit.all$sex)
        w = fit.all[key %in% worse, c("chain","data.type","age","sex","median.sim.vs.data","last.sim.vs.data","flag")]
        print(head(w[order(w$data.type, w$age, w$sex, w$chain), ], 40), row.names = FALSE)
    } else cat("\nNo target fits badly in another chain but well in the best chain.\n")

    proj = do.call(cbind, proj); colnames(proj) = paste0("chain", CHAINS)
    best.col = paste0("chain", best.chain)
    pct.diff = round(100 * (proj - proj[, best.col]) / proj[, best.col], 1)
    is.pct = grepl("^pct", rownames(proj))
    pct.diff[is.pct, ] = round(proj[is.pct, ] - proj[is.pct, best.col], 1)  # percentage points for the percentages
    cat("\nProjections (chain medians):\n"); print(round(proj, ifelse(any(proj > 1000), 0, 1)))
    cat("\nDifference from the best chain (", best.chain, "): % for counts, percentage points for the percentages:\n", sep = "")
    print(pct.diff)

    # parameters: spread of chain medians vs within-chain spread (log scale for positive parameters)
    common = Reduce(intersect, lapply(pars, colnames))
    tr = function(x) if (all(x > 0, na.rm = TRUE)) log(x) else x
    meds = sapply(pars, function(P) apply(P[, common, drop = FALSE], 2, median))
    sep = sapply(common, function(v){
        within = mean(sapply(pars, function(P) sd(tr(P[, v]))))
        between = sd(sapply(pars, function(P) median(tr(P[, v]))))
        if (within == 0) NA else between / within
    })
    top = names(sort(sep, decreasing = TRUE))[1:min(12, length(sep))]
    par.tab = data.frame(parameter = top, separation = round(sep[top], 1), round(meds[top, , drop = FALSE], 3), row.names = NULL)
    names(par.tab)[-(1:2)] = paste0("chain", CHAINS)
    cat("\nParameters that differ most between chains (separation = spread of chain medians / typical spread within a chain):\n")
    print(par.tab, row.names = FALSE)

    write.csv(ll, file.path(dir, paste0(convert_string(location), "_chains.csv")), row.names = FALSE)
    write.csv(data.frame(projection = rownames(proj), proj, check.names = FALSE), file.path(dir, paste0(convert_string(location), "_projections.csv")), row.names = FALSE)
    write.csv(par.tab, file.path(dir, paste0(convert_string(location), "_parameters.csv")), row.names = FALSE)
    write.csv(fit.all, file.path(dir, paste0(convert_string(location), "_fit_by_chain.csv")), row.names = FALSE)

    biggest = max(abs(pct.diff[!is.pct, ][ , ll$stuck, drop = FALSE]), 0, na.rm = TRUE)
    all.summary[[location]] = data.frame(location = location, best.chain = best.chain,
                                         stuck.chains = paste(ll$chain[ll$stuck], collapse = ", "),
                                         worst.gap = min(ll$gap.to.best),
                                         extra.targets.flagged.in.stuck = if (any(ll$stuck)) max(ll$targets.flagged[ll$stuck]) - ll$targets.flagged[ll$chain == best.chain] else 0,
                                         biggest.projection.diff.in.stuck.pct = biggest)
}
summary = do.call(rbind, all.summary)
write.csv(summary, file.path(dir, "summary.csv"), row.names = FALSE)
cat("\n================ Summary ================\n")
print(summary, row.names = FALSE)
cat("stuck = median log-likelihood more than ", STUCK.GAP, " below the best chain's; projection diff = largest |% difference| from the best chain\n",
    "in new infections or PLHIV (2025/2030/2040) among the stuck chains. Files in ", normalizePath(dir), "\n", sep = "")
