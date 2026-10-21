################################################################################################
# NEXTGEN: poster Panel A (annual new HIV infections, 2010-2030, baseline + 4 LAI-ART scenarios) for
# ages 15-24, for the AIDS 2026 runs (Jan 2026, Oct 2025 calibration, 200 sims) and the updated runs
# (joint calibration, 1,000 sims). Same style and colors as the poster; both on the same y-axis.
# Outputs: cached/nextgen_lai_rerun/PanelA_15-24_aids2026.jpeg and PanelA_15-24_updated.jpeg
################################################################################################
library(colorspace)
source("model/run_systematic.R")
OUT.DIR = file.path("cached", "nextgen_lai_rerun")
AGES = "15-24"
YEARS = 2010:2030
cols = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#56B4E9")
load.one = function(file) get(load(file)[1])

aids = list(no.int = "cached/simset.noint_25_1yr_2026-01-22.Rdata",
            all.25 = "cached/simset.all_25_1yr_2026-01-22.Rdata",
            all.direct.25 = "cached/simset.all.direct_25_1yr_2026-01-22.Rdata",
            all.100 = "cached/simset.all_100_1yr_2026-01-22.Rdata",
            all.direct.100 = "cached/simset.all.direct_100_1yr_2026-01-22.Rdata")
updated = lapply(names(aids), function(nm) file.path(OUT.DIR, paste0("simset_", nm, "_2026-10-21.Rdata")))
names(updated) = names(aids)

make.panel = function(files){
    s = lapply(files, load.one)
    pdf(NULL) # simplot prints as it builds
    p = simplot(s$no.int, s$all.25, s$all.direct.25, s$all.100, s$all.direct.100,
                data.types = "incidence", ages = AGES, facet.by = "age", years = YEARS,
                show.individual.sims = F, for.paper = T, ncol = 1)
    dev.off()
    point_idx = which(sapply(p$layers, function(l) inherits(l$geom, "GeomPoint")))
    for(i in point_idx){
        p$layers[[i]]$mapping$colour = NULL
        p$layers[[i]]$aes_params$colour = "grey10"
    }
    p
}
p.aids = make.panel(aids)
p.updated = make.panel(updated)

# same y-axis for both: the larger of the two plots' maxima
ymax = max(sapply(list(p.aids, p.updated), function(p){
    b = ggplot_build(p)
    max(unlist(lapply(b$data, function(d) c(d$ymax, d$y))), na.rm = T)
}))

style = function(p, title){
    p + theme(strip.text.x = element_blank(), axis.title.x = element_blank(),
              axis.title.y = element_text(size = 30, margin = margin(r = 15)),
              text = element_text(size = 28), legend.position = "none",
              plot.title = element_text(size = 30, face = "bold")) +
        scale_y_continuous(labels = function(x){ paste0(x / 1000, "k") }, limits = c(0, ymax)) +
        scale_color_manual(values = cols) +
        scale_fill_manual(values = lighten(cols, amount = 0.3)) +
        ylab("Annual new HIV infections,\n ages 15-24") + ggtitle(title)
}
jpeg(file.path(OUT.DIR, "PanelA_15-24_aids2026.jpeg"), width = 3000, height = 1500, res = 200)
print(style(p.aids, "AIDS 2026 (Oct 2025 calibration, 200 simulations)"))
dev.off()
jpeg(file.path(OUT.DIR, "PanelA_15-24_updated.jpeg"), width = 3000, height = 1500, res = 200)
print(style(p.updated, "Updated (joint calibration, 1,000 simulations)"))
dev.off()
print(paste0("Saved PanelA_15-24_aids2026.jpeg and PanelA_15-24_updated.jpeg in ", OUT.DIR))
