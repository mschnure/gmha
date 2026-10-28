################################################################################################
# NEXTGEN (10/28/26): why LAI-ART averts fewer infections now than in AIDS 2026. Two calibrations only:
#   AIDS 2026 (Oct 2025 calibration, 200 sims) vs Current (Oct 27 fresh calibration with SABSSM 15-24 ratio targets,
#   tighter priors; 1,000 sims). Outputs in cached/nextgen_lai_rerun_sabssm_ratio_priors/:
#   cmp_cascade_15-24.png          baseline 15-24 cascade 2010-2030 (aware of PWH; on ART and suppressed of aware) + targets
#   cmp_pwh_breakdown_2022.png     15-24 PWH in 2022 (baseline): unaware / aware not on ART / on ART unsuppressed / suppressed
#   cmp_initiations_by_source.png  LAI-ART initiations 2022-2030 by where people came from
#   cmp_averted_per_unsuppressed.png infections averted per 1,000 unsuppressed youth initiated
#   cmp_suppression_gain.png       15-24 suppression gain (% of PWH) over baseline, 2020-2030
#   cmp_incidence_baseline.png     baseline new infections, all ages and 15-24, with UNAIDS data
#   cmp_transmission_rates.png     calibrated transmission rate at each time point
#   cmp_summary_<date>.csv         numbers behind the bar charts
################################################################################################
source("model/run_systematic.R")
OUT.DIR = file.path("cached", "nextgen_lai_rerun_sabssm_ratio_priors")
CAL.LABELS = c(aids2026 = "AIDS 2026", current = "Current (Oct 27)")
CAL.COLS = c("AIDS 2026" = "#B45F1B", "Current (Oct 27)" = "#1F6F8B")
arrays = list(aids2026 = "cached/full.results.array_2026-01-22.Rdata",
              current = max(list.files(OUT.DIR, pattern = "^full\\.results\\.array_[0-9-]+\\.Rdata$", full.names = T)))
SIMSETS = c(aids2026 = "cached/simset.noint_25_1yr_2026-01-22.Rdata",
            current = max(list.files("mcmc_runs/nextgen_sabssm_ratio_priors", pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = T)))
SCENARIOS = c("all.25","all.direct.25","all.100","all.direct.100")
SC.LABELS = c(all.25 = "25%, requirement", all.direct.25 = "25%, no requirement",
              all.100 = "100%, requirement", all.direct.100 = "100%, no requirement")
SOURCE.LABELS = c(lai.art.es = "From suppressed on oral ART", lai.art.eu = "From unsuppressed on oral ART",
                  lai.art.du = "From disengaged")
AGES.15.24 = c("15-19","20-24")
YEARS = as.character(2010:2030)
LAI.YEARS = as.character(2022:2030)
q3 = function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm = T)
qdf = function(m, ...) { qs = apply(m, 1, q3); data.frame(year = as.numeric(rownames(m)), lower = qs[1,], median = qs[2,], upper = qs[3,], ...) }
cascade = list(); breakdown = list(); sources = list(); per.unsupp = list(); gain = list()

for(cal in names(arrays)){
    arr = get(load(arrays[[cal]])[1]); lab = CAL.LABELS[[cal]]
    print(paste0(lab, ": ", arrays[[cal]]))
    yr.sum = function(outcome, intervention = "no.int", years = YEARS, ages = AGES.15.24)
        apply(arr[years, ages,, outcome,, intervention, drop = F], c("year","sim"), sum)
    prev = yr.sum("prevalence"); aware = yr.sum("awareness"); eng = yr.sum("engagement"); supp = yr.sum("suppression")
    cascade[[length(cascade)+1]] = qdf(aware/prev, calibration = lab, measure = "Aware (of PWH)")
    cascade[[length(cascade)+1]] = qdf(eng/aware, calibration = lab, measure = "On ART (of aware)")
    cascade[[length(cascade)+1]] = qdf(supp/aware, calibration = lab, measure = "Suppressed (of aware)")

    # 2022 breakdown of 15-24 PWH (thousands, medians)
    y = "2022"
    parts = list("Unaware" = prev[y,] - aware[y,], "Aware, not on ART" = aware[y,] - eng[y,],
                 "On ART, unsuppressed" = eng[y,] - supp[y,], "Suppressed" = supp[y,])
    for(p in names(parts)){ v = q3(parts[[p]]/1000)
        breakdown[[length(breakdown)+1]] = data.frame(calibration = lab, group = p, median = v[2], lower = v[1], upper = v[3],
                                                      pct.of.pwh = median(100*parts[[p]]/prev[y,])) }

    inc.total = apply(arr[LAI.YEARS,,,"incidence",,,drop = F], c("sim","intervention"), sum)
    base.pct = supp[as.character(2020:2030),]/prev[as.character(2020:2030),]
    for(s in SCENARIOS){
        for(src in names(SOURCE.LABELS)){ v = q3(apply(arr[LAI.YEARS,,,src,,s,drop = F], "sim", sum)/1000)
            sources[[length(sources)+1]] = data.frame(calibration = lab, scenario = SC.LABELS[[s]], source = SOURCE.LABELS[[src]],
                                                      median = v[2], lower = v[1], upper = v[3]) }
        unsupp.init = apply(arr[LAI.YEARS,,,c("lai.art.eu","lai.art.du"),,s,drop = F], "sim", sum)
        all.init = apply(arr[LAI.YEARS,,,c("lai.art.es","lai.art.eu","lai.art.du"),,s,drop = F], "sim", sum)
        averted = inc.total[,"no.int"] - inc.total[,s]
        v = q3(1000*averted/unsupp.init)
        per.unsupp[[length(per.unsupp)+1]] = data.frame(calibration = lab, scenario = SC.LABELS[[s]], median = v[2], lower = v[1], upper = v[3],
                                                        pct.initiated.unsuppressed = median(100*unsupp.init/all.init),
                                                        unsuppressed.initiated.thousands = median(unsupp.init)/1000,
                                                        averted.thousands = median(averted)/1000)
        pct = yr.sum("suppression", s, as.character(2020:2030))/yr.sum("prevalence", s, as.character(2020:2030))
        gain[[length(gain)+1]] = qdf(100*(pct - base.pct), calibration = lab, scenario = SC.LABELS[[s]])
    }
    rm(arr); invisible(gc())
}
fac = function(d){ d$calibration = factor(d$calibration, levels = CAL.LABELS); d }
cascade = fac(do.call(rbind, cascade)); breakdown = fac(do.call(rbind, breakdown)); sources = fac(do.call(rbind, sources))
per.unsupp = fac(do.call(rbind, per.unsupp)); gain = fac(do.call(rbind, gain))
cascade$measure = factor(cascade$measure, levels = c("Aware (of PWH)","On ART (of aware)","Suppressed (of aware)"))
breakdown$group = factor(breakdown$group, levels = rev(c("Unaware","Aware, not on ART","On ART, unsuppressed","Suppressed")))
sources$source = factor(sources$source, levels = rev(SOURCE.LABELS))
for(nm in c("sources","per.unsupp","gain")){ d = get(nm); d$scenario = factor(d$scenario, levels = SC.LABELS); assign(nm, d) }

# calibration targets in the loaded data manager (ratio method: survey youth/adult ratio x UNAIDS 15+)
tg = function(type, suffix = "") sapply(c("2017","2022"), function(y) DATA.MANAGER[[paste0(type, suffix)]]$year.age.location[y, "15-24", "South Africa"])
survey = data.frame(year = rep(c(2017, 2022), 3),
                    median = c(tg("awareness"), tg("engagement"), tg("suppression")),
                    lower = c(tg("awareness",".lowers"), tg("engagement",".lowers"), tg("suppression",".lowers")),
                    upper = c(tg("awareness",".uppers"), tg("engagement",".uppers"), tg("suppression",".uppers")),
                    measure = factor(rep(levels(cascade$measure), each = 2), levels = levels(cascade$measure)))

th = theme_bw(base_size = 22) + theme(legend.position = "bottom", legend.title = element_blank(),
                                      strip.text = element_text(size = 20), panel.grid.minor = element_blank())
save.png = function(p, name, w = 3600, h = 1500){ png(file.path(OUT.DIR, name), width = w, height = h, res = 200); print(p); dev.off() }
ribbon.lines = function(d) list(geom_ribbon(aes(ymin = lower, ymax = upper, fill = calibration), data = d, alpha = 0.25, color = NA),
                                geom_line(aes(color = calibration), data = d, linewidth = 1.1),
                                scale_color_manual(values = CAL.COLS), scale_fill_manual(values = CAL.COLS))

save.png(ggplot(cascade, aes(x = year, y = median)) + ribbon.lines(cascade) +
             geom_pointrange(data = survey, aes(x = year, y = median, ymin = lower, ymax = upper), color = "grey10", size = 0.8, inherit.aes = F) +
             geom_vline(xintercept = 2022, linetype = "dashed", color = "grey50") +
             facet_wrap(~measure, nrow = 1) + coord_cartesian(ylim = c(0, 1)) + th + labs(x = NULL, y = NULL,
             caption = paste0("Black points: 15-24 calibration targets (", attr(DATA.MANAGER, "sabssm.youth.cascade.method"), " method), 2017 and 2022, 95% CI. Dashed line: LAI-ART starts.")),
         "cmp_cascade_15-24.png")

save.png(ggplot(breakdown, aes(x = calibration, y = median, fill = group)) + geom_col(color = "black", width = 0.65) +
             geom_text(aes(label = paste0(round(pct.of.pwh), "%")), position = position_stack(vjust = 0.5), size = 7) +
             scale_fill_manual(values = c("Unaware" = "#C9C9C9", "Aware, not on ART" = "#E6A15C",
                                          "On ART, unsuppressed" = "#C8553D", "Suppressed" = "#4E8F6A")) +
             th + labs(x = NULL, y = "PWH aged 15-24 in 2022 (thousands)"),
         "cmp_pwh_breakdown_2022.png", w = 2400, h = 1700)

save.png(ggplot(sources, aes(x = calibration, y = median, fill = source)) + geom_col(color = "black", width = 0.7) +
             facet_wrap(~scenario, nrow = 1) +
             scale_fill_manual(values = setNames(c("#4E8F6A","#C8553D","#E6A15C"), SOURCE.LABELS)) +
             th + theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
             labs(x = NULL, y = "Initiated on LAI-ART, 2022-2030\n(thousands)"),
         "cmp_initiations_by_source.png", w = 3600, h = 1700)

save.png(ggplot(per.unsupp, aes(x = scenario, y = median, fill = calibration)) +
             geom_col(position = position_dodge(width = 0.8), width = 0.75, color = "black") +
             geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.8), width = 0.2) +
             scale_fill_manual(values = CAL.COLS) + th +
             labs(x = NULL, y = "Infections averted per 1,000\nunsuppressed youth initiated"),
         "cmp_averted_per_unsuppressed.png", w = 3000, h = 1500)

save.png(ggplot(gain, aes(x = year, y = median)) + ribbon.lines(gain) + geom_hline(yintercept = 0, color = "grey50") +
             facet_wrap(~scenario, nrow = 1) + th + scale_x_continuous(breaks = c(2020, 2025, 2030)) +
             labs(x = NULL, y = "Suppression gain, ages 15-24\n(percentage points of PWH)"),
         "cmp_suppression_gain.png")

gain.2030 = gain[gain$year == 2030, c("calibration","scenario","median","lower","upper")]
summary.tab = rbind(
    cbind(table = "pwh_15_24_2022_thousands", breakdown[, c("calibration","group","median","lower","upper")], extra = round(breakdown$pct.of.pwh, 1)),
    cbind(table = "initiated_by_source_thousands", setNames(sources[, c("calibration","source","median","lower","upper")], c("calibration","group","median","lower","upper")),
          extra = as.character(sources$scenario)),
    cbind(table = "averted_per_1000_unsuppressed_initiated", setNames(per.unsupp[, c("calibration","scenario","median","lower","upper")], c("calibration","group","median","lower","upper")),
          extra = paste0(round(per.unsupp$pct.initiated.unsuppressed, 1), "% of initiations unsuppressed")),
    cbind(table = "suppression_gain_2030_pct_points", setNames(gain.2030, c("calibration","group","median","lower","upper")), extra = ""))
write.csv(summary.tab, file.path(OUT.DIR, paste0("cmp_summary_", Sys.Date(), ".csv")), row.names = F)

print("15-24 PWH in 2022 (baseline), thousands and % of PWH:")
print(breakdown[, c("calibration","group","median","pct.of.pwh")], digits = 3, row.names = F)
print("Initiations from unsuppressed, and infections averted per 1,000 of them:")
print(per.unsupp[, c("calibration","scenario","pct.initiated.unsuppressed","unsuppressed.initiated.thousands","averted.thousands","median")], digits = 3, row.names = F)
print("15-24 suppression gain in 2030 (percentage points of PWH):")
print(gain.2030, digits = 3, row.names = F)
print(paste0("Saved cmp_* plots and summary in ", OUT.DIR))

## Baseline incidence (all ages, 15-24) and transmission rates
inc = list()
for(cal in names(arrays)){
    arr = get(load(arrays[[cal]])[1]); lab = CAL.LABELS[[cal]]
    yr.sum2 = function(ages) apply(arr[YEARS, ages,, "incidence",, "no.int", drop = F], c("year","sim"), sum)
    inc[[length(inc)+1]] = qdf(yr.sum2(dimnames(arr)$age)/1000, calibration = lab, panel = "All ages")
    inc[[length(inc)+1]] = qdf(yr.sum2(AGES.15.24)/1000, calibration = lab, panel = "Ages 15-24")
    rm(arr); invisible(gc())
}
inc = fac(do.call(rbind, inc)); inc$panel = factor(inc$panel, levels = c("All ages","Ages 15-24"))
inc.data = do.call(rbind, lapply(list(c("All ages","All ages"), c("15-24","Ages 15-24")), function(a) tryCatch({
    d = if(a[1] == "All ages") get.surveillance.data(DATA.MANAGER, "incidence", years = as.numeric(YEARS), locations = "South Africa", keep.dimensions = c("year","location"))
        else get.surveillance.data(DATA.MANAGER, "incidence", years = as.numeric(YEARS), ages = a[1], locations = "South Africa", keep.dimensions = c("year","age","location"))
    data.frame(year = as.numeric(dimnames(d)$year), value = as.numeric(d)/1000, panel = a[2]) }, error = function(e) NULL)))
p.inc = ggplot(inc, aes(x = year, y = median)) + ribbon.lines(inc) + facet_wrap(~panel, nrow = 1, scales = "free_y") +
    expand_limits(y = 0) + th + labs(x = NULL, y = "New HIV infections (thousands)")
if(!is.null(inc.data)){ inc.data$panel = factor(inc.data$panel, levels = levels(inc$panel))
    p.inc = p.inc + geom_point(data = inc.data, aes(x = year, y = value), color = "grey10", size = 2.5, inherit.aes = F) }
save.png(p.inc, "cmp_incidence_baseline.png", w = 3000, h = 1500)

knots = c(trate.0 = "1990 and before", trate.1 = "1997", trate.2 = "2008", trate.3 = "2018", trate.4 = "2040")
tr = do.call(rbind, lapply(names(SIMSETS), function(cal){
    pars = get(load(SIMSETS[[cal]])[1])@parameters
    do.call(rbind, lapply(names(knots), function(k){ v = q3(pars[,k])
        data.frame(calibration = CAL.LABELS[[cal]], knot = knots[[k]], lower = v[1], median = v[2], upper = v[3]) }))
}))
tr = fac(tr); tr$knot = factor(tr$knot, levels = knots)
save.png(ggplot(tr, aes(x = knot, y = median, color = calibration)) +
             geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), size = 1.1, linewidth = 1.3) +
             scale_color_manual(values = CAL.COLS) + scale_y_log10() + th +
             labs(x = "Transmission rate time point", y = "Overall transmission rate (log scale)"),
         "cmp_transmission_rates.png", w = 3000, h = 1500)
print("Transmission rates (medians):"); print(reshape(tr[, c("calibration","knot","median")], idvar = "knot", timevar = "calibration", direction = "wide"), digits = 3, row.names = F)

## Decomposition: infections averted = unsuppressed youth initiated x infections averted per unsuppressed initiated
dec = per.unsupp[, c("calibration","scenario","unsuppressed.initiated.thousands","median","averted.thousands")]
names(dec)[4] = "averted.per.1000.unsuppressed"
print("Decomposition (medians): averted (thousands) ~ unsuppressed initiated (thousands) x averted per 1,000 unsuppressed initiated / 1000")
print(dec, digits = 3, row.names = F)
