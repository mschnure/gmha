################################################################################################
# NEXTGEN: review the joint South Africa + youth cascade calibration (calibration/run_youth_joint_calibration_sa.R)
# against the master calibration it was warm-started from. Saves the plots to a PDF.
#
# Run on the nextgen branch, from the repo folder:
#   Rscript future_projections/compare_joint_youth_calibration.R
################################################################################################
source("model/run_systematic.R")

JOINT.SIMSET.FILE = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-20.Rdata"  # copied from the remote
MASTER.SIMSET.FILE = "mcmc_runs/simset_south_africa_step2_chains1234_2026-10-20.Rdata"
PLOT.FILE = paste0("mcmc_runs/nextgen/joint_youth_calibration_review_", Sys.Date(), ".pdf")

YOUTH.PARAMS = c("age.0.14.awareness.multiplier","age.0.14.engagement.multiplier","age.0.14.suppression.multiplier",
                 "log.OR.0.14.engagement.slope","log.OR.0.14.suppression.slope")

simset.joint = get(load(JOINT.SIMSET.FILE)[1])
simset.master = get(load(MASTER.SIMSET.FILE)[1])
print(paste0("Joint: ", simset.joint@n.sim, " distinct simulations (total weight ", sum(simset.joint@weights), ")"))
print(paste0("Master: ", simset.master@n.sim, " distinct simulations (total weight ", sum(simset.master@weights), ")"))

## Youth parameters (weighted quantiles)
wq = function(x, w, p){ o = order(x); cw = cumsum(w[o])/sum(w); sapply(p, function(pp) x[o][which(cw >= pp)[1]]) }
youth.summary = sapply(YOUTH.PARAMS, function(p) wq(simset.joint@parameters[,p], simset.joint@weights, c(0.025,0.5,0.975)))
rownames(youth.summary) = c("2.5%","50%","97.5%")
print("Youth parameters, joint calibration:"); print(round(t(youth.summary), 3))

## How far the other parameters moved: joint median vs master median, in master 95% interval widths
shared = setdiff(intersect(colnames(simset.joint@parameters), colnames(simset.master@parameters)), YOUTH.PARAMS)
shift = sapply(shared, function(p){
    m = wq(simset.master@parameters[,p], simset.master@weights, c(0.025,0.5,0.975))
    j = wq(simset.joint@parameters[,p], simset.joint@weights, 0.5)
    (j - m[2]) / max(m[3] - m[1], 1e-12)
})
print("Parameters that moved most (joint median - master median, as a fraction of master's 95% interval width):")
print(round(head(shift[order(-abs(shift))], 15), 2))

pdf(PLOT.FILE, width = 11, height = 8.5)
# names in the legend: the simsets in the order given (1 = master, 2 = joint)
invisible(simplot(simset.master, simset.joint, years = 2000:2040,
                  data.types = c('awareness','engagement','suppression'),
                  facet.by = 'age', ages = c('0-14','15+'), proportion = T))
invisible(simplot(simset.master, simset.joint, years = 1980:2040, data.types = c('incidence','prevalence')))
invisible(simplot(simset.master, simset.joint, years = 1980:2040, data.types = c('incidence','prevalence'), facet.by = 'age'))
invisible(simplot(simset.master, simset.joint, years = 1980:2040, data.types = c('hiv.mortality')))
dev.off()
print(paste0("Plots saved to ", PLOT.FILE, " (1 = master, 2 = joint)"))
