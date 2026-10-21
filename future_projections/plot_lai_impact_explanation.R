################################################################################################
# NEXTGEN: plots explaining why LAI-ART averts fewer infections in the updated runs than in AIDS 2026
#   1. expl_cascade_15-24.png       baseline 15-24 cascade, 2010-2030: % of PWH aware / on ART / suppressed, and unsuppressed PWH
#   2. expl_incidence_baseline.png  baseline new infections (no LAI-ART), 2010-2030, all ages and ages 15-24, with data
#   3. expl_transmission_rates.png  calibrated overall transmission rate at each time point
#   4. expl_suppression_gain.png    15-24 suppression gain over baseline, by scenario, 2020-2030
#   5. expl_averted_per_unsuppressed.png  infections averted per 1,000 unsuppressed youth initiated on LAI-ART
# AIDS 2026 = Jan 2026 runs (Oct 2025 calibration, 200 sims); Updated = joint calibration (1,000 sims)
################################################################################################
source("model/run_systematic.R")
OUT.DIR = file.path("cached", "nextgen_lai_rerun")
CAL.LABELS = c(aids2026 = "AIDS 2026 (Oct 2025 calibration)", updated = "Updated (joint calibration)")
CAL.COLS = c("AIDS 2026 (Oct 2025 calibration)" = "#B45F1B", "Updated (joint calibration)" = "#1F6F8B")
SCENARIOS = c("all.25","all.direct.25","all.100","all.direct.100")
SC.LABELS = c(all.25 = "25%, requirement", all.direct.25 = "25%, no requirement",
              all.100 = "100%, requirement", all.direct.100 = "100%, no requirement")
AGES.15.24 = c("15-19","20-24")
YEARS = as.character(2010:2030)
LAI.YEARS = as.character(2022:2030)
qdf = function(m, ...) { # m: year x sim matrix -> data frame of quantiles by year
    qs = apply(m, 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
    data.frame(year = as.numeric(rownames(m)), lower = qs[1,], median = qs[2,], upper = qs[3,], ...)
}
cascade = list(); incidence = list(); gain = list(); per.unsupp = list()

arrays = list(aids2026 = "cached/full.results.array_2026-01-22.Rdata",
              updated = file.path(OUT.DIR, "full.results.array_2026-10-21.Rdata"))
for(cal in names(arrays)){
    arr = get(load(arrays[[cal]])[1]); lab = CAL.LABELS[[cal]]
    yr.sum = function(outcome, ages, intervention = "no.int", years = YEARS)
        apply(arr[years, ages,, outcome,, intervention, drop = F], c("year","sim"), sum)
    prev = yr.sum("prevalence", AGES.15.24)
    cascade[[length(cascade)+1]] = qdf(100*yr.sum("awareness", AGES.15.24)/prev, calibration = lab, measure = "Aware (%)")
    cascade[[length(cascade)+1]] = qdf(100*yr.sum("engagement", AGES.15.24)/prev, calibration = lab, measure = "On ART (%)")
    cascade[[length(cascade)+1]] = qdf(100*yr.sum("suppression", AGES.15.24)/prev, calibration = lab, measure = "Suppressed (%)")
    cascade[[length(cascade)+1]] = qdf((prev - yr.sum("suppression", AGES.15.24))/1000, calibration = lab, measure = "Unsuppressed (1,000s)")
    incidence[[length(incidence)+1]] = qdf(yr.sum("incidence", dimnames(arr)$age)/1000, calibration = lab, panel = "All ages")
    incidence[[length(incidence)+1]] = qdf(yr.sum("incidence", AGES.15.24)/1000, calibration = lab, panel = "Ages 15-24")
    base.pct = 100*yr.sum("suppression", AGES.15.24, "no.int", as.character(2020:2030))/yr.sum("prevalence", AGES.15.24, "no.int", as.character(2020:2030))
    inc.total = apply(arr[LAI.YEARS,,,"incidence",,,drop = F], c("sim","intervention"), sum)
    for(s in SCENARIOS){
        pct = 100*yr.sum("suppression", AGES.15.24, s, as.character(2020:2030))/yr.sum("prevalence", AGES.15.24, s, as.character(2020:2030))
        gain[[length(gain)+1]] = qdf(pct - base.pct, calibration = lab, scenario = SC.LABELS[[s]])
        unsupp.init = apply(arr[LAI.YEARS,,,c("lai.art.eu","lai.art.du"),,s,drop = F], "sim", sum)
        x = 1000*(inc.total[,"no.int"] - inc.total[,s])/unsupp.init
        v = quantile(x, c(0.025, 0.5, 0.975), na.rm = T)
        per.unsupp[[length(per.unsupp)+1]] = data.frame(calibration = lab, scenario = SC.LABELS[[s]], lower = v[1], median = v[2], upper = v[3])
    }
    rm(arr); invisible(gc())
}
fac = function(d){ d$calibration = factor(d$calibration, levels = CAL.LABELS); d }
cascade = fac(do.call(rbind, cascade)); incidence = fac(do.call(rbind, incidence))
gain = fac(do.call(rbind, gain)); per.unsupp = fac(do.call(rbind, per.unsupp))
cascade$measure = factor(cascade$measure, levels = c("Aware (%)","On ART (%)","Suppressed (%)","Unsuppressed (1,000s)"))
incidence$panel = factor(incidence$panel, levels = c("All ages","Ages 15-24"))
gain$scenario = factor(gain$scenario, levels = SC.LABELS); per.unsupp$scenario = factor(per.unsupp$scenario, levels = SC.LABELS)

# incidence data points (UNAIDS), where available
inc.data = do.call(rbind, lapply(list(c("All ages","All ages"), c("15-24","Ages 15-24")), function(a){
    tryCatch({
        d = if(a[1] == "All ages")
                get.surveillance.data(DATA.MANAGER, "incidence", years = as.numeric(YEARS), locations = "South Africa", keep.dimensions = c("year","location"))
            else get.surveillance.data(DATA.MANAGER, "incidence", years = as.numeric(YEARS), ages = a[1], locations = "South Africa",
                                       keep.dimensions = c("year","age","location"))
        data.frame(year = as.numeric(dimnames(d)$year), value = as.numeric(d)/1000, panel = a[2])
    }, error = function(e) NULL)
}))
if(!is.null(inc.data)) inc.data$panel = factor(inc.data$panel, levels = levels(incidence$panel))

th = theme_bw(base_size = 22) + theme(legend.position = "bottom", legend.title = element_blank(),
                                      strip.text = element_text(size = 20), panel.grid.minor = element_blank())
save.png = function(p, name, w = 3000, h = 1500){
    png(file.path(OUT.DIR, name), width = w, height = h, res = 200); print(p); dev.off()
}
ribbon.lines = function(d) list(geom_ribbon(aes(ymin = lower, ymax = upper, fill = calibration), data = d, alpha = 0.25, color = NA),
                                geom_line(aes(color = calibration), data = d, linewidth = 1.1),
                                scale_color_manual(values = CAL.COLS), scale_fill_manual(values = CAL.COLS))

save.png(ggplot(cascade, aes(x = year, y = median)) + ribbon.lines(cascade) +
             geom_vline(xintercept = 2022, linetype = "dashed", color = "grey50") +
             facet_wrap(~measure, nrow = 1, scales = "free_y") + expand_limits(y = 0) + th + labs(x = NULL, y = NULL),
         "expl_cascade_15-24.png", w = 3600, h = 1500)

p.inc = ggplot(incidence, aes(x = year, y = median)) + ribbon.lines(incidence) +
    facet_wrap(~panel, nrow = 1, scales = "free_y") + expand_limits(y = 0) + th +
    labs(x = NULL, y = "New HIV infections (thousands)")
if(!is.null(inc.data)) p.inc = p.inc + geom_point(data = inc.data, aes(x = year, y = value), color = "grey10", size = 2.5, inherit.aes = F)
save.png(p.inc, "expl_incidence_baseline.png")

knots = c(trate.0 = "1990 and before", trate.1 = "1997", trate.2 = "2008", trate.3 = "2018", trate.4 = "2040")
simsets = list(aids2026 = "cached/simset.noint_25_1yr_2026-01-22.Rdata",
               updated = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata")
tr = do.call(rbind, lapply(names(simsets), function(cal){
    pars = get(load(simsets[[cal]])[1])@parameters
    do.call(rbind, lapply(names(knots), function(k){
        v = quantile(pars[,k], c(0.025, 0.5, 0.975))
        data.frame(calibration = CAL.LABELS[[cal]], knot = knots[[k]], lower = v[1], median = v[2], upper = v[3])
    }))
}))
tr = fac(tr); tr$knot = factor(tr$knot, levels = knots)
save.png(ggplot(tr, aes(x = knot, y = median, color = calibration)) +
             geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), size = 1.1, linewidth = 1.3) +
             scale_color_manual(values = CAL.COLS) + scale_y_log10() + th +
             labs(x = "Transmission rate time point", y = "Overall transmission rate (log scale)"),
         "expl_transmission_rates.png")

save.png(ggplot(gain, aes(x = year, y = median)) + ribbon.lines(gain) + geom_hline(yintercept = 0, color = "grey50") +
             facet_wrap(~scenario, nrow = 1) + th + scale_x_continuous(breaks = c(2020, 2025, 2030)) +
             labs(x = NULL, y = "Suppression gain, ages 15-24\n(percentage points)"),
         "expl_suppression_gain.png", w = 3600, h = 1500)

save.png(ggplot(per.unsupp, aes(x = scenario, y = median, fill = calibration)) +
             geom_col(position = position_dodge(width = 0.75), width = 0.7, color = "black") +
             geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.75), width = 0.2) +
             scale_fill_manual(values = CAL.COLS) + th +
             labs(x = NULL, y = "Infections averted per 1,000\nunsuppressed youth initiated"),
         "expl_averted_per_unsuppressed.png")

write.csv(rbind(cbind(type = "cascade", cascade[, c("calibration","measure","year","median","lower","upper")])),
          file.path(OUT.DIR, "expl_cascade_15-24.csv"), row.names = F)
print(tr); print(per.unsupp)
print(paste0("Saved explanation plots in ", OUT.DIR))
