################################################################################################
# NEXTGEN: rerun the AIDS 2026 LAI-ART scenarios on the joint South Africa + youth cascade calibration,
# and compare with the AIDS 2026 abstract numbers.
#
#   Scenarios (as in AIDS 2026): baseline (no LAI-ART), and LAI-ART from all three arrows with
#   (all) and without (all.direct) the oral suppression requirement, at 25% and 100% offered each year;
#   LAI users switch back to oral ART at 25. Outcomes summed over 2022-2030.
#   1,000 simulations, C++ ODE.
#
# Outputs, all in OUT.DIR (cached/ is gitignored; nothing is written to the poster folders):
#   simsets for the 5 scenarios, the full results array, lai_results_table_<date>.csv (AIDS 2026 table
#   layout), lai_comparison_aids2026_<date>.csv, PanelA_joint_<date>.jpeg, PanelB_joint_<date>.jpeg
#
# Run from the repo folder on the nextgen branch (~20-40 min, one core):
#   nohup caffeinate -i Rscript future_projections/rerun_lai_joint_calibration.R > mcmc_runs/logs/nextgen_lai_rerun.log 2>&1 &
################################################################################################
library(colorspace)
source("model/run_systematic.R")
source("model/diffeq_cpp.R")
options(gmha.use.cpp = TRUE)

# defaults = the 10/21 joint calibration; a wrapper script can set these before sourcing this file
# (e.g. future_projections/rerun_lai_sabssm_calibration.R)
if(!exists("SIMSET.FILE")) SIMSET.FILE = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata"
if(!exists("OUT.DIR")) OUT.DIR = file.path("cached", "nextgen_lai_rerun")
if(!exists("FIGURE.TAG")) FIGURE.TAG = "joint"
RUN.SIMSET.FILE = SIMSET.FILE; RUN.OUT.DIR = OUT.DIR; RUN.FIGURE.TAG = FIGURE.TAG # create_lai_interventions.R re-sources the model
dir.create(OUT.DIR, recursive = TRUE, showWarnings = FALSE)
END.YEAR = 2040
YEARS.TO.SUMMARIZE = as.character(2022:2030)
SCENARIOS = c("all.25","all.direct.25","all.100","all.direct.100")

simset = get(load(SIMSET.FILE)[1])
print(paste0("Loaded ", simset@n.sim, " simulations from ", SIMSET.FILE))
if(length(unique(simset@weights)) > 1)
    warning("Simulations have unequal weights; the quantiles below treat every simulation equally")
simset.joint = simset

##-- 1. Run the scenarios --##
simset.list = list()
for(prob in c(0.25, 1)){
    PROB.1.YEAR = prob
    source("future_projections/create_lai_interventions.R") # interventions for this uptake (re-sources the model)
    SIMSET.FILE = RUN.SIMSET.FILE; OUT.DIR = RUN.OUT.DIR; FIGURE.TAG = RUN.FIGURE.TAG
    simset = simset.joint
    label = ifelse(prob == 1, "100", "25")
    if(is.null(simset.list$no.int)){
        print("==== running baseline (no LAI-ART) ====")
        simset.list$no.int = run.intervention.on.simset(simset, end.year = END.YEAR, interventions = NO.INTERVENTION)
    }
    print(paste0("==== running ", label, "% offered, with suppression requirement ===="))
    simset.list[[paste0("all.", label)]] = run.intervention.on.simset(simset, end.year = END.YEAR,
                                                                        interventions = list(lai.from.all, lai.removal.by.age))
    print(paste0("==== running ", label, "% offered, no suppression requirement ===="))
    simset.list[[paste0("all.direct.", label)]] = run.intervention.on.simset(simset, end.year = END.YEAR,
                                                                               interventions = list(lai.from.all.direct, lai.removal.by.age))
}
simset.list = simset.list[c("no.int", SCENARIOS)]
for(nm in names(simset.list)){
    simset = simset.list[[nm]]
    save(simset, file = file.path(OUT.DIR, paste0("simset_", nm, "_", Sys.Date(), ".Rdata")))
}
print("Saved the 5 scenario simsets")

##-- 2. Results array (2010-2040) --##
full.results.array = generate.full.results.array(simset.list = simset.list, years = as.character(2010:END.YEAR))
save(full.results.array, file = file.path(OUT.DIR, paste0("full.results.array_", Sys.Date(), ".Rdata")))
print("Saved the results array")

##-- 3. AIDS 2026 table: new infections, infections averted, initiated on LAI-ART, 2022-2030 --##
inc = full.results.array[YEARS.TO.SUMMARIZE,,,"incidence",,,drop = F]
new.all = apply(inc, c("sim","intervention"), sum)
new.1524 = apply(inc[,c("15-19","20-24"),,,,,drop = F], c("sim","intervention"), sum)
initiated = apply(full.results.array[YEARS.TO.SUMMARIZE,,,c("lai.art.es","lai.art.eu","lai.art.du"),,,drop = F],
                  c("sim","intervention"), sum)

q = function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm = T)
metric.list = list()
for(s in c("no.int", SCENARIOS)){
    m = list(new.infections.all = q(new.all[,s]), new.infections.15.24 = q(new.1524[,s]))
    if(s != "no.int"){
        av.all = new.all[,"no.int"] - new.all[,s]
        av.1524 = new.1524[,"no.int"] - new.1524[,s]
        m = c(m, list(initiated.on.lai = q(initiated[,s]),
                      averted.all = q(av.all),
                      percent.averted.all = q(100*av.all/new.all[,"no.int"]),
                      initiated.per.averted.all = q(initiated[,s]/av.all),
                      averted.15.24 = q(av.1524),
                      percent.averted.15.24 = q(100*av.1524/new.1524[,"no.int"]),
                      initiated.per.averted.15.24 = q(initiated[,s]/av.1524)))
    }
    metric.list[[s]] = m
}
long = do.call(rbind, lapply(names(metric.list), function(s) do.call(rbind, lapply(names(metric.list[[s]]), function(m){
    v = metric.list[[s]][[m]]
    data.frame(scenario = s, metric = m, median = unname(v[2]), lower = unname(v[1]), upper = unname(v[3]))
}))))
write.csv(long, file.path(OUT.DIR, paste0("lai_results_table_", Sys.Date(), ".csv")), row.names = F)
print("Results (medians and 95% intervals across simulations):")
print(long, digits = 4, row.names = F)

##-- 4. Comparison with the AIDS 2026 abstract (medians, Jan 2026 runs on the Oct 2025 calibration, 200 sims) --##
aids2026 = rbind(
    data.frame(scenario = "no.int", metric = c("new.infections.all","new.infections.15.24"), aids2026 = c(1131909, 412442)),
    data.frame(scenario = rep(SCENARIOS, each = 9),
               metric = rep(c("initiated.on.lai","new.infections.all","averted.all","percent.averted.all","initiated.per.averted.all",
                              "new.infections.15.24","averted.15.24","percent.averted.15.24","initiated.per.averted.15.24"), 4),
               aids2026 = c(322028, 1062438, 68721, 6.06, 4.67, 373874, 38881, 9.43, 8.25,
                            351932, 1052995, 78053, 6.89, 4.49, 368485, 44284, 10.76, 7.92,
                            823770, 939915, 191551, 16.86, 4.30, 303721, 108960, 26.43, 7.55,
                            868439, 910832, 220588, 19.43, 3.93, 286796, 125837, 30.58, 6.90)))
comparison = merge(aids2026, long[, c("scenario","metric","median","lower","upper")], by = c("scenario","metric"), sort = F)
names(comparison)[names(comparison) == "median"] = "joint.median"
comparison$difference = comparison$joint.median - comparison$aids2026
comparison$percent.change = round(100*comparison$difference/comparison$aids2026, 1)
write.csv(comparison, file.path(OUT.DIR, paste0("lai_comparison_aids2026_", Sys.Date(), ".csv")), row.names = F)
print("Comparison with AIDS 2026 (joint calibration median vs abstract median):")
print(comparison[, c("scenario","metric","aids2026","joint.median","percent.change")], digits = 4, row.names = F)

##-- 5. Figures, same style as the poster panels --##
cols = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#56B4E9")
scenario.labels = c("25% uptake,\n With suppression \n requirement", "25% uptake,\n No suppression \n requirement",
                    "100% uptake,\n With suppression \n requirement", "100% uptake,\n No suppression \n requirement")
df = data.frame(scenario = factor(scenario.labels, levels = scenario.labels),
                est = sapply(SCENARIOS, function(s) metric.list[[s]]$averted.all[2]),
                lower = sapply(SCENARIOS, function(s) metric.list[[s]]$averted.all[1]),
                upper = sapply(SCENARIOS, function(s) metric.list[[s]]$averted.all[3]))
jpeg(file = file.path(OUT.DIR, paste0("PanelB_", FIGURE.TAG, "_", Sys.Date(), ".jpeg")), width = 3000, height = 1500, res = 200)
print(ggplot(df, aes(x = scenario, y = est)) +
    geom_col(fill = cols[2:5], color = "black", width = 0.6) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
    theme_minimal() + ylab("Cumulative infections averted,\n2022-2030") + xlab(NULL) +
    scale_y_continuous(labels = function(x){ paste0(x / 1000, "k") }) +
    theme(axis.title.y = element_text(size = 30, margin = margin(r = 15)),
          axis.text.x = element_text(size = 25), axis.text.y = element_text(size = 28)))
dev.off()

pdf(NULL) # simplot prints as it builds; keep that off the jpeg below
p = simplot(simset.list$no.int, simset.list$all.25, simset.list$all.direct.25, simset.list$all.100, simset.list$all.direct.100,
            data.types = "incidence", ages = c("All ages"), facet.by = "age", years = 2010:2030,
            show.individual.sims = F, for.paper = T, ncol = 1) +
    theme(strip.text.x = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_text(size = 30, margin = margin(r = 15)),
          text = element_text(size = 28), legend.position = "none") +
    scale_y_continuous(labels = function(x){ paste0(x / 1000, "k") }, limits = c(0, NA))
dev.off()
point_idx = which(sapply(p$layers, function(l) inherits(l$geom, "GeomPoint")))
p$layers[[point_idx]]$mapping$colour = NULL
p$layers[[point_idx]]$aes_params$colour = "grey10"
jpeg(file = file.path(OUT.DIR, paste0("PanelA_", FIGURE.TAG, "_", Sys.Date(), ".jpeg")), width = 3000, height = 1500, res = 200)
print(p + scale_color_manual(values = cols) + scale_fill_manual(values = lighten(cols, amount = 0.3)) +
      ylab("Annual New HIV infections,\n 2010-2030"))
dev.off()

print(paste0("LAI rerun finished - outputs in ", OUT.DIR))
