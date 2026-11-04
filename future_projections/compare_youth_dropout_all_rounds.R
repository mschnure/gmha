################################################################################################
# NEXTGEN (11/03/26): youth vs adult dropout from care, in AIDS 2026 and every South Africa nextgen calibration since.
# Runs future_projections/check_youth_suppression_speed.R on all of them (100 simulations each; ~5-10 min), then prints:
#   - dropout after starting ART: chance of having left care at least once by 12 months (ages 15-24 vs 25-49)
#   - dropout while suppressed: chance a suppressed person on oral ART leaves care within a year
#   - youth / adult ratio of each, and the calibrated 15-24 dropout multiplier (only in runs from 10/26 on)
# All in 2022 (also 2017 and 2030 in the saved csv). Excludes re-engagement and death.
################################################################################################
SIMSETS = c("AIDS 2026"                          = "cached/simset.noint_25_1yr_2026-01-22.Rdata",
            "Oct 21: joint (0-14 data)"          = "mcmc_runs/nextgen/simset_south_africa_step2_chains1234_2026-10-21.Rdata",
            "Oct 22: SABSSM 15-24 as is"         = "mcmc_runs/nextgen_sabssm/simset_south_africa_step2_chains1234_2026-10-22.Rdata",
            "Oct 23: SABSSM ratio targets"       = "mcmc_runs/nextgen_sabssm_ratio/simset_south_africa_step2_chains1234_2026-10-23.Rdata",
            "Oct 26: + youth dropout multiplier" = "mcmc_runs/nextgen_sabssm_ratio_retention/simset_south_africa_step2_chains1234_2026-10-26.Rdata",
            "Oct 27: tighter priors"             = "mcmc_runs/nextgen_sabssm_ratio_priors/simset_south_africa_step2_chains1234_2026-10-27.Rdata",
            "Oct 28: tighter youth dropout prior"= "mcmc_runs/nextgen_youth_dropout_prior/simset_south_africa_step2_chains1234_2026-10-28.Rdata",
            "Nov 2: current (anchored)"          = "mcmc_runs/nextgen_transmission_anchor/simset_south_africa_step2_chains1234_2026-11-02.Rdata")
SIMSETS = SIMSETS[file.exists(SIMSETS)]
N.SIMS = 100
source("future_projections/check_youth_suppression_speed.R")

y = "2022"
pick = function(measure, age) sapply(names(SIMSETS), function(cal){
    x = summ[summ$calibration == cal & summ$year == y & summ$age == age & summ$measure == measure, ]
    if(nrow(x) == 0) NA else x$median })
tab = data.frame(
    calibration = names(SIMSETS),
    youth.dropout.12mo.after.start = pick("p.dropped.out.12mo.after.start", "15-24"),
    adult.dropout.12mo.after.start = pick("p.dropped.out.12mo.after.start", "25-49"),
    youth.suppressed.dropout.1yr = pick("p.suppressed.dropped.out.1yr", "15-24"),
    adult.suppressed.dropout.1yr = pick("p.suppressed.dropped.out.1yr", "25-49"),
    row.names = NULL)
tab$youth.vs.adult.after.start = tab$youth.dropout.12mo.after.start / tab$adult.dropout.12mo.after.start
tab$youth.vs.adult.suppressed = tab$youth.suppressed.dropout.1yr / tab$adult.suppressed.dropout.1yr
tab$youth.dropout.multiplier = sapply(names(SIMSETS), function(cal){
    m = DIS.PARAMS[[cal]]
    if("age.15.24.disengagement.multiplier" %in% rownames(m))
        sprintf("%.2f (%.2f-%.2f)", m["age.15.24.disengagement.multiplier", 2], m["age.15.24.disengagement.multiplier", 1], m["age.15.24.disengagement.multiplier", 3])
    else "none (youth = adults)" })
cat("\n==== Dropout from care in 2022, youth (15-24) vs adults (25-49), medians ====\n")
print(format(tab, digits = 2), row.names = FALSE)
write.csv(tab, file.path("lai_runs/nextgen_lai_rerun_transmission_anchor", paste0("youth_dropout_all_rounds_", Sys.Date(), ".csv")), row.names = FALSE)
