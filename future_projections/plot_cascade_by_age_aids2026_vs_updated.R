################################################################################################
# NEXTGEN: baseline cascade by age group (0-14, 15+, 15-24), AIDS 2026 (Oct 2025 calibration, 200 sims)
# vs updated (joint calibration, 1,000 sims), 2010-2030, with UNAIDS data where it exists (0-14, 15+).
# Definitions match the UNAIDS data: aware = of PWH; on ART and suppressed = of those aware.
# y-axis 0-1 on every panel.  Output: cached/nextgen_lai_rerun/expl_cascade_by_age.png
################################################################################################
source("model/run_systematic.R")
OUT.DIR = file.path("cached", "nextgen_lai_rerun")
CAL.LABELS = c(aids2026 = "AIDS 2026 (Oct 2025 calibration)", updated = "Updated (joint calibration)")
CAL.COLS = setNames(c("#B45F1B", "#1F6F8B"), CAL.LABELS)
YEARS = as.character(2010:2030)
AGE.LEVELS = c("Ages 0-14", "Ages 15+", "Ages 15-24")
MEASURES = c(awareness = "Aware (of PWH)", engagement = "On ART (of aware)", suppression = "Suppressed (of aware)")

arrays = list(aids2026 = "cached/full.results.array_2026-01-22.Rdata",
              updated = file.path(OUT.DIR, "full.results.array_2026-10-21.Rdata"))
model.rows = list()
for(cal in names(arrays)){
    arr = get(load(arrays[[cal]])[1])
    ages = dimnames(arr)$age
    groups = list("Ages 0-14" = ages[1:3], "Ages 15+" = ages[4:length(ages)], "Ages 15-24" = c("15-19","20-24"))
    for(g in names(groups)){
        tot = function(o) apply(arr[YEARS, groups[[g]],, o,, "no.int", drop = F], c("year","sim"), sum)
        prev = tot("prevalence"); aware = tot("awareness")
        vals = list(awareness = aware/prev, engagement = tot("engagement")/aware, suppression = tot("suppression")/aware)
        for(m in names(vals)){
            qs = apply(vals[[m]], 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
            model.rows[[length(model.rows)+1]] = data.frame(calibration = CAL.LABELS[[cal]], age = g, measure = MEASURES[[m]],
                                                            year = as.numeric(YEARS), lower = qs[1,], median = qs[2,], upper = qs[3,])
        }
    }
    rm(arr); invisible(gc())
}
model.df = do.call(rbind, model.rows)

# UNAIDS data (point, lower, upper) for 0-14 and 15+
get.dat = function(type, age) tryCatch(
    as.numeric(get.surveillance.data(DATA.MANAGER, type, years = as.numeric(YEARS), ages = age, locations = "South Africa",
                                     keep.dimensions = c("year","age","location"))), error = function(e) rep(NA, length(YEARS)))
data.df = do.call(rbind, lapply(names(MEASURES), function(m) do.call(rbind, lapply(c("0-14","15+"), function(a)
    data.frame(age = paste0("Ages ", a), measure = MEASURES[[m]], year = as.numeric(YEARS),
               value = get.dat(m, a), lower = get.dat(paste0(m, ".lowers"), a), upper = get.dat(paste0(m, ".uppers"), a))))))
data.df = data.df[!is.na(data.df$value),]

for(d in c("model.df","data.df")){
    x = get(d); x$age = factor(x$age, levels = AGE.LEVELS); x$measure = factor(x$measure, levels = MEASURES); assign(d, x)
}
model.df$calibration = factor(model.df$calibration, levels = CAL.LABELS)

p = ggplot() +
    geom_ribbon(data = model.df, aes(x = year, ymin = lower, ymax = upper, fill = calibration), alpha = 0.25) +
    geom_line(data = model.df, aes(x = year, y = median, color = calibration), linewidth = 1.1) +
    geom_errorbar(data = data.df, aes(x = year, ymin = lower, ymax = upper), width = 0, color = "grey40") +
    geom_point(data = data.df, aes(x = year, y = value), color = "grey10", size = 2.2) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "grey50") +
    facet_grid(age ~ measure) +
    scale_color_manual(values = CAL.COLS) + scale_fill_manual(values = CAL.COLS) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), oob = scales::squish) +
    scale_x_continuous(breaks = c(2010, 2020, 2030)) +
    theme_bw(base_size = 22) +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(),
          strip.text = element_text(size = 20)) +
    labs(x = NULL, y = NULL)
png(file.path(OUT.DIR, "expl_cascade_by_age.png"), width = 3000, height = 2400, res = 200)
print(p)
dev.off()
print(subset(model.df, year == 2022 & age == "Ages 15-24"))
print(paste0("Saved ", file.path(OUT.DIR, "expl_cascade_by_age.png")))
