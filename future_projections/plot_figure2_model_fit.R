################################################################################################
# NEXTGEN (11/04/26): manuscript Figure 2 - model fit over the calibration period (no projections).
# 10 panels: new HIV infections, people living with HIV, aware (% of PWH), on ART (% of aware), virally suppressed
# (% of aware), each for all ages and ages 15-24, South Africa. Model: median and 95% credible interval of the 1,000
# calibrated simulations; points and bars: calibration targets with their 95% intervals
#   - all ages: UNAIDS AIDSinfo 2025 estimates
#   - 15-24 incidence and PLHIV: UNAIDS; 15-24 cascade: SABSSM 2017/2022 youth-to-adult ratio x UNAIDS 15+ (Table 2, §)
# Years: incidence and PLHIV 1995-2025, cascade 2010-2025 (UNAIDS cascade data start in 2015).
#   source("future_projections/plot_figure2_model_fit.R")
# Output: cached/nextgen_lai_rerun_<run>/Figure2_model_fit_<date>.jpeg (and .pdf)
################################################################################################
source("model/run_systematic.R")   # also loads DATA.MANAGER (the calibration targets)
library(ggplot2)
if(!exists("NEW.RUN.DIR")) NEW.RUN.DIR = "mcmc_runs/nextgen_transmission_anchor"
OUT.DIR = file.path("cached", sub("^mcmc_runs/nextgen_", "nextgen_lai_rerun_", NEW.RUN.DIR))
dir.create(OUT.DIR, showWarnings = FALSE, recursive = TRUE)
f = list.files(NEW.RUN.DIR, pattern = "^simset_south_africa_step2_chains1234_[0-9-]+\\.Rdata$", full.names = TRUE)
simset = get(load(f[order(f)][length(f)])[1])
LOCATION = "South Africa"
EPI.YEARS = 1995:2025; CASCADE.YEARS = 2010:2025
AGES = list("All ages" = NULL, "Ages 15-24" = c("15-19","20-24"))
MEASURES = list(  # label, numerator, denominator (NULL = count), scale, years
    list(label = "New HIV infections (thousands)", num = "incidence",   den = NULL,         scale = 1/1000, years = EPI.YEARS),
    list(label = "People living with HIV (thousands)", num = "prevalence", den = NULL,     scale = 1/1000, years = EPI.YEARS),
    list(label = "Aware of status (% of PWH)",       num = "awareness",   den = "prevalence", scale = 100,  years = CASCADE.YEARS),
    list(label = "On ART (% of aware)",              num = "engagement",  den = "awareness",  scale = 100,  years = CASCADE.YEARS),
    list(label = "Virally suppressed (% of aware)",  num = "suppression", den = "awareness",  scale = 100,  years = CASCADE.YEARS))

## model: median and 95% interval across simulations
by.age = function(sim, dt, years, ages){
    x = extract.data(sim, data.type = dt, years = years, keep.dimensions = c("year","age"))
    if(is.null(ages)) rowSums(x) else rowSums(x[, ages, drop = FALSE])
}
model = list(); targets = list()
for(m in MEASURES) for(a in names(AGES)){
    panel = paste0(m$label, ": ", a)
    vals = sapply(simset@simulations, function(sim){
        v = by.age(sim, m$num, m$years, AGES[[a]])
        if(!is.null(m$den)) v = v / by.age(sim, m$den, m$years, AGES[[a]])
        v * m$scale })
    q = apply(vals, 1, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE)
    model[[panel]] = data.frame(panel = panel, measure = m$label, age = a, year = m$years, lower = q[1,], median = q[2,], upper = q[3,])
    ## targets (point, lower, upper) from the data manager
    yrs = as.character(m$years)
    get.target = function(type){
        if(is.null(AGES[[a]])) { x = DATA.MANAGER[[type]]$year.location; if(is.null(x) || !LOCATION %in% colnames(x)) return(NULL)
                                 x = x[intersect(yrs, rownames(x)), LOCATION] }
        else { x = DATA.MANAGER[[type]]$year.age.location; if(is.null(x) || !"15-24" %in% dimnames(x)$age) return(NULL)
               x = x[intersect(yrs, dimnames(x)$year), "15-24", LOCATION] }
        x }
    pt = get.target(m$num)
    if(!is.null(pt)){
        lo = get.target(paste0(m$num, ".lowers")); up = get.target(paste0(m$num, ".uppers"))
        s = if(is.null(m$den)) m$scale else 100   # cascade targets are stored as proportions
        d = data.frame(panel = panel, measure = m$label, age = a, year = as.numeric(names(pt)), value = as.numeric(pt)*s,
                       lower = if(is.null(lo)) NA else as.numeric(lo[names(pt)])*s, upper = if(is.null(up)) NA else as.numeric(up[names(pt)])*s)
        targets[[panel]] = d[!is.na(d$value), ]
    }
}
model = do.call(rbind, model); targets = do.call(rbind, targets)
levels.panel = unlist(lapply(MEASURES, function(m) paste0(m$label, ": ", names(AGES))))
model$panel = factor(model$panel, levels = levels.panel); targets$panel = factor(targets$panel, levels = levels.panel)

p = ggplot(model, aes(x = year)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1F6F8B", alpha = 0.25) +
    geom_line(aes(y = median), color = "#1F6F8B", linewidth = 0.8) +
    geom_errorbar(data = targets, aes(ymin = lower, ymax = upper), width = 0, color = "grey35", linewidth = 0.4) +
    geom_point(data = targets, aes(y = value), color = "black", size = 1.4) +
    facet_wrap(~ panel, ncol = 2, scales = "free") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(strip.background = element_blank(), strip.text = element_text(size = 11, hjust = 0),
          axis.text = element_text(size = 9), panel.grid.minor = element_blank())
out = file.path(OUT.DIR, paste0("Figure2_model_fit_", Sys.Date()))
jpeg(paste0(out, ".jpeg"), width = 2400, height = 3300, res = 300); print(p); dev.off()
pdf(paste0(out, ".pdf"), width = 8, height = 11); print(p); dev.off()
print(paste0("Saved ", out, ".jpeg and .pdf. Line and band: model median and 95% CrI (", simset@n.sim,
             " simulations); points and bars: calibration targets and 95% intervals."))
