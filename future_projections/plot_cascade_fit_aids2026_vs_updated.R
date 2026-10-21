################################################################################################
# NEXTGEN: cascade fit for ages 0-14 and 15+ (where there is UNAIDS data), AIDS 2026 baseline
# (Oct 2025 calibration, 200 sims) vs updated (joint calibration, 1,000 sims), 2010-2030.
# Same colors as the other AIDS 2026 vs updated comparison plots.
# Output: cached/nextgen_lai_rerun/expl_cascade_0-14_15plus.png
################################################################################################
source("model/run_systematic.R")
OUT.DIR = file.path("cached", "nextgen_lai_rerun")
CAL.LABELS = c("1" = "AIDS 2026 (Oct 2025 calibration)", "2" = "Updated (joint calibration)")
CAL.COLS = c("1" = "#B45F1B", "2" = "#1F6F8B")
simset.aids2026 = get(load("cached/simset.noint_25_1yr_2026-01-22.Rdata")[1])
simset.updated = get(load("mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata")[1])

pdf(NULL) # simplot prints as it builds
p = simplot(simset.aids2026, simset.updated,
            years = 2010:2030,
            data.types = c('awareness','engagement','suppression'),
            facet.by = 'age', ages = c('0-14','15+'),
            proportion = T, ncol = 2)
dev.off()
p = p +
    scale_color_manual(values = c(CAL.COLS, "truth" = "grey10"), breaks = names(CAL.LABELS), labels = CAL.LABELS) +
    scale_fill_manual(values = CAL.COLS, breaks = names(CAL.LABELS), labels = CAL.LABELS) +
    guides(shape = "none", color = "none") +
    theme_bw(base_size = 22) +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(),
          strip.text = element_text(size = 20)) +
    labs(x = NULL, y = NULL)
png(file.path(OUT.DIR, "expl_cascade_0-14_15plus.png"), width = 3000, height = 2100, res = 200)
print(p)
dev.off()
print(paste0("Saved ", file.path(OUT.DIR, "expl_cascade_0-14_15plus.png")))
