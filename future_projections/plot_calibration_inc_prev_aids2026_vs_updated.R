################################################################################################
# NEXTGEN: calibration fit for new HIV infections and PWH, all ages and ages 15-24, 2010-2030,
# AIDS 2026 (Oct 2025 calibration, 200 sims) vs updated (joint calibration, 1,000 sims), with UNAIDS data.
# Output: cached/nextgen_lai_rerun/expl_calibration_inc_prev.png
################################################################################################
source("model/run_systematic.R")
OUT.DIR = file.path("cached", "nextgen_lai_rerun")
CAL.LABELS = c(aids2026 = "AIDS 2026 (Oct 2025 calibration)", updated = "Updated (joint calibration)")
CAL.COLS = setNames(c("#B45F1B", "#1F6F8B"), CAL.LABELS)
YEARS = as.character(2010:2030)
PANELS = c("New HIV infections, all ages", "New HIV infections, ages 15-24", "PWH, all ages", "PWH, ages 15-24")
arrays = list(aids2026 = "cached/full.results.array_2026-01-22.Rdata",
              updated = file.path(OUT.DIR, "full.results.array_2026-10-21.Rdata"))
model.rows = list()
for(cal in names(arrays)){
    arr = get(load(arrays[[cal]])[1])
    groups = list("all ages" = dimnames(arr)$age, "ages 15-24" = c("15-19","20-24"))
    for(o in c(incidence = "New HIV infections", prevalence = "PWH")) for(g in names(groups)){
        v = apply(arr[YEARS, groups[[g]],, names(which(c(incidence = "New HIV infections", prevalence = "PWH") == o)),, "no.int", drop = F],
                  c("year","sim"), sum)/1000
        qs = apply(v, 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
        model.rows[[length(model.rows)+1]] = data.frame(calibration = CAL.LABELS[[cal]], panel = paste0(o, ", ", g),
                                                        year = as.numeric(YEARS), lower = qs[1,], median = qs[2,], upper = qs[3,])
    }
    rm(arr); invisible(gc())
}
model.df = do.call(rbind, model.rows)
get.dat = function(type, age) tryCatch({
    d = if(is.null(age)) get.surveillance.data(DATA.MANAGER, type, years = as.numeric(YEARS), locations = "South Africa", keep.dimensions = c("year","location"))
        else get.surveillance.data(DATA.MANAGER, type, years = as.numeric(YEARS), ages = age, locations = "South Africa", keep.dimensions = c("year","age","location"))
    as.numeric(d)/1000 }, error = function(e) rep(NA, length(YEARS)))
data.df = do.call(rbind, lapply(list(c("incidence","New HIV infections"), c("prevalence","PWH")), function(t)
    do.call(rbind, lapply(list(list(NULL, "all ages"), list("15-24", "ages 15-24")), function(a)
        data.frame(panel = paste0(t[2], ", ", a[[2]]), year = as.numeric(YEARS), value = get.dat(t[1], a[[1]]),
                   lower = get.dat(paste0(t[1], ".lowers"), a[[1]]), upper = get.dat(paste0(t[1], ".uppers"), a[[1]]))))))
data.df = data.df[!is.na(data.df$value),]
model.df$panel = factor(model.df$panel, levels = PANELS); data.df$panel = factor(data.df$panel, levels = PANELS)
model.df$calibration = factor(model.df$calibration, levels = CAL.LABELS)
p = ggplot() +
    geom_ribbon(data = model.df, aes(x = year, ymin = lower, ymax = upper, fill = calibration), alpha = 0.25) +
    geom_line(data = model.df, aes(x = year, y = median, color = calibration), linewidth = 1.1) +
    geom_errorbar(data = data.df, aes(x = year, ymin = lower, ymax = upper), width = 0, color = "grey40") +
    geom_point(data = data.df, aes(x = year, y = value), color = "grey10", size = 2.2) +
    facet_wrap(~panel, nrow = 2, scales = "free_y") + expand_limits(y = 0) +
    scale_color_manual(values = CAL.COLS) + scale_fill_manual(values = CAL.COLS) +
    theme_bw(base_size = 22) +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(),
          strip.text = element_text(size = 20)) +
    labs(x = NULL, y = "Thousands")
png(file.path(OUT.DIR, "expl_calibration_inc_prev.png"), width = 3000, height = 2100, res = 200)
print(p)
dev.off()
print(paste0("Saved ", file.path(OUT.DIR, "expl_calibration_inc_prev.png")))
