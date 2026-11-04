################################################################################################
# NEXTGEN (11/04/26): manuscript Figure 2 - annual new HIV infections 2010-2030, baseline and the four LAI-ART scenarios,
# (A) all ages and (B) ages 15-24, with the UNAIDS estimates (2025 round) as points. Same style as the AIDS 2026 poster
# panel: median lines, 95% credible bands, legend box "Simulations".
# Uses the saved LAI rerun (future_projections/rerun_lai_sabssm_calibration.R) - no new simulations.
#   source("future_projections/plot_figure2_new_infections.R")
# Output: cached/nextgen_lai_rerun_<run>/Figure2_new_infections_<date>.jpeg (and .pdf)
################################################################################################
source("model/run_systematic.R")   # loads DATA.MANAGER (UNAIDS estimates for the points)
library(ggplot2)
if(!exists("NEW.RUN.DIR")) NEW.RUN.DIR = "mcmc_runs/nextgen_transmission_anchor"
OUT.DIR = file.path("cached", sub("^mcmc_runs/nextgen_", "nextgen_lai_rerun_", NEW.RUN.DIR))
f = list.files(OUT.DIR, pattern = "^full\\.results\\.array_[0-9-]+\\.Rdata$", full.names = TRUE)
load(f[order(f)][length(f)])   # full.results.array [year, age, sex, outcome, sim, intervention]
YEARS = as.character(2010:2030)
SCEN = c(no.int = "Baseline", all.25 = "25%, suppression requirement", all.direct.25 = "25%, no suppression requirement",
         all.100 = "100%, suppression requirement", all.direct.100 = "100%, no suppression requirement")
COLS = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#56B4E9"); names(COLS) = SCEN   # as in the AIDS 2026 poster
PANELS = list("(A) All ages" = dimnames(full.results.array)$age, "(B) Ages 15-24" = c("15-19","20-24"))

sims = do.call(rbind, lapply(names(PANELS), function(panel){
    do.call(rbind, lapply(names(SCEN), function(s){
        x = apply(full.results.array[YEARS, PANELS[[panel]],, "incidence",, s, drop = FALSE], c("year","sim"), sum)
        q = apply(x, 1, quantile, c(0.025, 0.5, 0.975))
        data.frame(panel = panel, scenario = SCEN[[s]], year = as.numeric(YEARS), lower = q[1,], median = q[2,], upper = q[3,])
    }))
}))
data.years = as.character(2010:2025)
inc.all = DATA.MANAGER$incidence$year.location[intersect(data.years, dimnames(DATA.MANAGER$incidence$year.location)$year), "South Africa"]
inc.1524 = DATA.MANAGER$incidence$year.age.location[intersect(data.years, dimnames(DATA.MANAGER$incidence$year.age.location)$year), "15-24", "South Africa"]
pts = rbind(data.frame(panel = "(A) All ages", year = as.numeric(names(inc.all)), value = as.numeric(inc.all)),
            data.frame(panel = "(B) Ages 15-24", year = as.numeric(names(inc.1524)), value = as.numeric(inc.1524)))
pts = pts[!is.na(pts$value), ]
sims$scenario = factor(sims$scenario, levels = SCEN)

p = ggplot(sims, aes(x = year)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.25) +
    geom_line(aes(y = median, color = scenario), linewidth = 0.8) +
    geom_point(data = pts, aes(y = value), color = "grey25", size = 1.8) +
    facet_wrap(~ panel, scales = "free_y") +
    scale_color_manual(name = "Simulations", values = COLS) + scale_fill_manual(name = "Simulations", values = COLS) +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = seq(2010, 2030, 5)) +
    labs(x = NULL, y = "New HIV infections") +
    theme_bw() +
    theme(strip.background = element_blank(), strip.text = element_text(size = 15, hjust = 0),
          axis.text = element_text(size = 12), axis.title.y = element_text(size = 13),
          legend.position = c(0.015, 0.04), legend.justification = c(0, 0),   # c() works with old and new ggplot2
          legend.background = element_rect(color = "black"), legend.title = element_text(size = 11),
          legend.text = element_text(size = 10), legend.key.size = unit(0.45, "cm"))
out = file.path(OUT.DIR, paste0("Figure2_new_infections_", Sys.Date()))
jpeg(paste0(out, ".jpeg"), width = 3600, height = 1600, res = 300); print(p); dev.off()
pdf(paste0(out, ".pdf"), width = 12, height = 5.3); print(p); dev.off()
print(paste0("Saved ", out, ".jpeg and .pdf"))
