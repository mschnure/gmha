################################################################################################
# NEXTGEN: why does each LAI-ART initiation avert fewer infections in the updated runs than in AIDS 2026?
# Compares the AIDS 2026 runs (Jan 2026, Oct 2025 calibration) with the updated runs (joint calibration),
# using saved results only (no model runs):
#   1. who goes onto LAI-ART: initiations by route (from engaged-suppressed, engaged-unsuppressed,
#      diagnosed-unengaged), 2022-2030, and infections averted per 1,000 initiated
#   2. the 15-24 cascade at baseline in 2022: PWH, aware, on ART, suppressed, unsuppressed
#   3. calibrated transmission parameters: age multipliers and transmission rates, old vs new
#   4. plots: % of PWH virally suppressed (oral ART or LAI-ART) by age group, 2010-2030, for baseline and
#      each LAI-ART scenario; and the gain over baseline (percentage points)
# Outputs: cached/nextgen_lai_rerun/lai_impact_comparison_<date>.csv (and printed)
#          cached/nextgen_lai_rerun/lai_suppression_by_age_<date>.pdf
################################################################################################
source("model/run_systematic.R")
OUT.DIR = file.path("cached", "nextgen_lai_rerun")
SCENARIOS = c("all.25","all.direct.25","all.100","all.direct.100")
YEARS = as.character(2022:2030)
AGES.15.24 = c("15-19","20-24")
q = function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm = T)
PLOT.YEARS = as.character(2010:2030)
SCENARIO.LABELS = c(no.int = "Baseline", all.25 = "25%, requirement", all.direct.25 = "25%, no requirement",
                    all.100 = "100%, requirement", all.direct.100 = "100%, no requirement")
CALIBRATION.LABELS = c(aids2026 = "AIDS 2026 (Oct 2025 calibration)", updated = "Updated (joint calibration)")
COLS = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#56B4E9")
supp.rows = list()
rows = list()
add = function(section, calibration, scenario, metric, x){
    v = q(x)
    rows[[length(rows)+1]] <<- data.frame(section = section, calibration = calibration, scenario = scenario, metric = metric,
                                           median = unname(v[2]), lower = unname(v[1]), upper = unname(v[3]))
}

arrays = list(aids2026 = "cached/full.results.array_2026-01-22.Rdata",
              updated = file.path(OUT.DIR, "full.results.array_2026-10-21.Rdata"))
for(cal in names(arrays)){
    arr = get(load(arrays[[cal]])[1])
    print(paste0(cal, ": interventions = ", paste0(dimnames(arr)$intervention, collapse = ", ")))
    inc = apply(arr[YEARS,,,"incidence",,,drop = F], c("sim","intervention"), sum)

    ## 1. initiations by route, and infections averted per 1,000 initiated
    for(s in SCENARIOS){
        by.route = sapply(c(es = "lai.art.es", eu = "lai.art.eu", du = "lai.art.du"), function(o)
            apply(arr[YEARS,,,o,,s,drop = F], "sim", sum))
        total = rowSums(by.route)
        averted = inc[,"no.int"] - inc[,s]
        add("1. initiations", cal, s, "initiated, total", total)
        add("1. initiations", cal, s, "initiated from engaged-suppressed", by.route[,"es"])
        add("1. initiations", cal, s, "initiated from engaged-unsuppressed", by.route[,"eu"])
        add("1. initiations", cal, s, "initiated from diagnosed-unengaged", by.route[,"du"])
        add("1. initiations", cal, s, "% of initiations from unsuppressed (EU + DU)", 100*(by.route[,"eu"] + by.route[,"du"])/total)
        add("1. initiations", cal, s, "infections averted per 1,000 initiated", 1000*averted/total)
        add("1. initiations", cal, s, "infections averted per 1,000 initiated from unsuppressed", 1000*averted/(by.route[,"eu"] + by.route[,"du"]))
    }

    ## 2. 15-24 cascade at baseline, 2022
    cas = sapply(c("population","prevalence","awareness","engagement","suppression"), function(o)
        apply(arr["2022", AGES.15.24,,o,,"no.int",drop = F], "sim", sum))
    add("2. 15-24 cascade, 2022", cal, "baseline", "PWH ages 15-24", cas[,"prevalence"])
    add("2. 15-24 cascade, 2022", cal, "baseline", "HIV prevalence ages 15-24 (%)", 100*cas[,"prevalence"]/cas[,"population"])
    add("2. 15-24 cascade, 2022", cal, "baseline", "% of PWH aware", 100*cas[,"awareness"]/cas[,"prevalence"])
    add("2. 15-24 cascade, 2022", cal, "baseline", "% of PWH on ART", 100*cas[,"engagement"]/cas[,"prevalence"])
    add("2. 15-24 cascade, 2022", cal, "baseline", "% of PWH suppressed", 100*cas[,"suppression"]/cas[,"prevalence"])
    add("2. 15-24 cascade, 2022", cal, "baseline", "unsuppressed PWH ages 15-24", cas[,"prevalence"] - cas[,"suppression"])
    add("2. 15-24 cascade, 2022", cal, "baseline", "new infections 15-24, 2022-2030", apply(arr[YEARS, AGES.15.24,,"incidence",,"no.int",drop = F], "sim", sum))
    ## 4. % of PWH suppressed by age group, by scenario (for the plots)
    ages = dimnames(arr)$age
    age.groups = list("0-14" = ages[1:3], "15-24" = AGES.15.24,
                      "25-49" = c("25-29","30-34","35-39","40-44","45-49"),
                      "50+" = ages[which(ages == "50-54"):length(ages)])
    for(g in names(age.groups)){
        pct = lapply(c("no.int", SCENARIOS), function(s){
            sup = apply(arr[PLOT.YEARS, age.groups[[g]],,"suppression",,s,drop = F], c("year","sim"), sum)
            prev = apply(arr[PLOT.YEARS, age.groups[[g]],,"prevalence",,s,drop = F], c("year","sim"), sum)
            100*sup/prev
        })
        names(pct) = c("no.int", SCENARIOS)
        for(s in names(pct)){
            for(type in c("level","gain")){
                if(type == "gain" && s == "no.int") next
                x = if(type == "level") pct[[s]] else pct[[s]] - pct[["no.int"]]
                qs = apply(x, 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
                supp.rows[[length(supp.rows)+1]] = data.frame(calibration = CALIBRATION.LABELS[[cal]], age.group = g,
                                                              scenario = SCENARIO.LABELS[[s]], type = type,
                                                              year = as.numeric(PLOT.YEARS), lower = qs[1,], median = qs[2,], upper = qs[3,])
            }
        }
    }
    rm(arr); invisible(gc())
}

## 3. calibrated transmission parameters
simsets = list(aids2026 = "cached/simset.noint_25_1yr_2026-01-22.Rdata",
               updated = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata")
for(cal in names(simsets)){
    pars = get(load(simsets[[cal]])[1])@parameters
    keep = grep("transmission.multiplier|^trate|proportion.trate", colnames(pars), value = T)
    for(p in keep) add("3. transmission parameters", cal, "", p, pars[,p])
    rm(pars); invisible(gc())
}

out = do.call(rbind, rows)
file = file.path(OUT.DIR, paste0("lai_impact_comparison_", Sys.Date(), ".csv"))
write.csv(out, file, row.names = F)
wide = reshape(out[, c("section","scenario","metric","calibration","median")], idvar = c("section","scenario","metric"),
               timevar = "calibration", direction = "wide")
wide$ratio.updated.to.aids2026 = round(wide$median.updated / wide$median.aids2026, 2)
options(width = 200)
print(wide, digits = 4, row.names = F)
print(paste0("Saved ", file))

## 4. suppression plots
supp = do.call(rbind, supp.rows)
supp$scenario = factor(supp$scenario, levels = SCENARIO.LABELS)
supp$calibration = factor(supp$calibration, levels = CALIBRATION.LABELS)
supp$age.group = factor(supp$age.group, levels = c("0-14","15-24","25-49","50+"))
write.csv(supp, file.path(OUT.DIR, paste0("lai_suppression_by_age_", Sys.Date(), ".csv")), row.names = F)
plot.file = file.path(OUT.DIR, paste0("lai_suppression_by_age_", Sys.Date(), ".pdf"))
pdf(plot.file, width = 14, height = 8.5)
print(ggplot(subset(supp, type == "level"), aes(x = year, y = median, color = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + geom_line(linewidth = 0.8) +
    facet_grid(calibration ~ age.group) + scale_color_manual(values = COLS) + scale_fill_manual(values = COLS) +
    theme_bw(base_size = 16) + theme(legend.position = "bottom", legend.title = element_blank()) +
    labs(x = NULL, y = "% of PWH virally suppressed (oral ART or LAI-ART)",
         title = "Viral suppression by age group and scenario, 2010-2030"))
print(ggplot(subset(supp, type == "gain" & year >= 2020), aes(x = year, y = median, color = scenario, fill = scenario)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + geom_line(linewidth = 0.8) +
    facet_grid(calibration ~ age.group) + scale_color_manual(values = COLS[-1]) + scale_fill_manual(values = COLS[-1]) +
    theme_bw(base_size = 16) + theme(legend.position = "bottom", legend.title = element_blank()) +
    labs(x = NULL, y = "Gain over baseline (percentage points)",
         title = "Increase in viral suppression over baseline, by age group and scenario, 2020-2030"))
dev.off()
print(paste0("Saved ", plot.file))
