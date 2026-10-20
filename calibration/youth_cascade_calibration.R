################################################################################################
# NEXTGEN: short calibration of the 5 youth (0-14) cascade parameters for South Africa, added on
# top of an existing South Africa calibration (e.g., the Step 2 simset run on master)
#
#   1. Picks one base simulation from the simset: best log-likelihood (master's likelihood, i.e. the
#      master data manager, which has no 0-14 cascade data) among N.CANDIDATE.SIMS simulations
#   2. Holds all of that simulation's other parameters fixed and runs a short MCMC over only the 5
#      youth parameters, with a likelihood that uses only the 0-14 awareness/engagement/suppression data
#   3. Pairs each simulation in the simset with a random draw from that youth posterior, re-runs them
#      on nextgen (no intervention), and saves that as the simset for the LAI analysis
#
# The youth parameters only change ages 0-14 (multipliers on 0-14 rates; slopes restricted to 0-14
# 10/20/26), so they don't change the 15+ fit from the original calibration.
################################################################################################
source("model/run_systematic.R")     # loads the LAI data manager (has the 0-14 cascade data)
source("calibration/mcmc_pipeline.R") # make.safe.mcmc.functions
use.cpp.model.if.possible()           # C++ ODE (includes LAI as of 10/20/26) if it compiles, otherwise R

LOCATION = "South Africa"
SIMSET.FILE = "mcmc_runs/simset_south_africa_step2_chains1234_2026-10-20.Rdata"
MASTER.DATA.MANAGER.FILE = "cached/data.manager_global_revised_ages_2026-10-15.Rdata" # the one the calibration used
N.CANDIDATE.SIMS = 100   # simulations scored to pick the base simulation
N.ITER = 20000           # youth MCMC iterations (5 parameters; check the trace plots below)
YOUTH.BURN = 5000        # iterations dropped from the start of the youth MCMC
YOUTH.CACHE.DIR = file.path(MCMC.CACHE.DIR, paste0("south_africa_youth_", Sys.Date()))
set.seed(20261020)

# when run outside RStudio (e.g., Rscript from Terminal), save the plots to a PDF
if(!interactive()){
    YOUTH.PLOT.FILE = file.path(MCMC.RUNS.DIR, paste0("youth_calibration_plots_", Sys.Date(), ".pdf"))
    pdf(YOUTH.PLOT.FILE, width = 11, height = 8.5)
    print(paste0("Plots will be saved to ", YOUTH.PLOT.FILE))
}

YOUTH.PARAMS = c("age.0.14.awareness.multiplier","age.0.14.engagement.multiplier","age.0.14.suppression.multiplier",
                 "log.OR.0.14.engagement.slope","log.OR.0.14.suppression.slope")

# same as the youth parameters in calibration/prior_distributions/south_africa_prior.R
YOUTH.PRIOR = join.distributions(
    age.0.14.awareness.multiplier = Lognormal.Distribution(log(1), log(8)/2),
    age.0.14.engagement.multiplier = Lognormal.Distribution(log(1), log(8)/2),
    age.0.14.suppression.multiplier = Lognormal.Distribution(log(1), log(8)/2),
    log.OR.0.14.engagement.slope = Normal.Distribution(0, log(4)/2),
    log.OR.0.14.suppression.slope = Normal.Distribution(0, log(4)/2))

simset = get(load(SIMSET.FILE)[1])
print(paste0("Loaded ", simset@n.sim, " simulations from ", SIMSET.FILE))
model.parameters = create.model.parameters(location = LOCATION)

##------------------------------------------------------------------------------------------##
##-- 1. Base simulation: best fit on master's likelihood among N.CANDIDATE.SIMS sims       --##
##------------------------------------------------------------------------------------------##
DATA.MANAGER.LAI = DATA.MANAGER
load(MASTER.DATA.MANAGER.FILE) # temporarily replaces DATA.MANAGER (the likelihood keeps its own copy of the data)
master.likelihood = set.likelihood.and.prior.by.location(location = LOCATION, params.start.values = simset@parameters[1,])$likelihood.to.run
DATA.MANAGER = DATA.MANAGER.LAI
rm(DATA.MANAGER.LAI)

candidates = sort(sample(1:simset@n.sim, min(N.CANDIDATE.SIMS, simset@n.sim)))
candidate.lik = sapply(candidates, function(i){ master.likelihood(simset@simulations[[i]]) })
BASE.INDEX = candidates[which.max(candidate.lik)]
base.parameters = simset@parameters[BASE.INDEX,]
base.parameters = base.parameters[setdiff(names(base.parameters), YOUTH.PARAMS)]
print(paste0("Base simulation: ", BASE.INDEX, " (log-likelihood ", round(max(candidate.lik), 1), ")"))

##------------------------------------------------------------------------------------------##
##-- 2. Short MCMC over the youth parameters only                                          --##
##------------------------------------------------------------------------------------------##
# likelihood with only the 0-14 cascade data: blank out 15+ and use only the year/age stratification
youth.data.manager = DATA.MANAGER
for(data.type in c("awareness","engagement","suppression"))
    for(suffix in c("",".lowers",".uppers"))
        youth.data.manager[[paste0(data.type, suffix)]]$year.age.location[,"15+",] = NA

# same year weights and cascade weights as the main calibration (set_likelihood_and_prior.R / likelihood.R)
WEIGHT.YEARS = 1970:2030
WEIGHTS.BY.YEAR = (1/4)^(WEIGHT.YEARS<2000) # before 2000, 1/4x
WEIGHTS.BY.YEAR[WEIGHT.YEARS>=2018] = 4 # from 2018, 4x
names(WEIGHTS.BY.YEAR) = WEIGHT.YEARS
CASCADE.WEIGHT = 3.734568

denominators = c(awareness = "prevalence", engagement = "awareness", suppression = "awareness")
youth.components = lapply(names(denominators), function(data.type){
    create.likelihood.for.data.type(data.type = data.type,
                                    data.manager = youth.data.manager,
                                    years = 1995:2023,
                                    location = LOCATION,
                                    parameters = model.parameters,
                                    denominator.data.type = denominators[[data.type]],
                                    obs.is.proportion = T,
                                    weight = WEIGHTS.BY.YEAR*CASCADE.WEIGHT,
                                    obs.correlation = 0.5,
                                    correlation.structure = "compound.symmetry",
                                    use.total = F, use.sex = F, use.age = T, use.age.sex = F)
})
youth.likelihood = function(sim){
    if(any(sapply(sim, function(x){ any(is.na(x)) })))
        return(-Inf)
    sum(sapply(youth.components, function(likelihood){ likelihood(sim) }))
}

youth.simulation = function(youth.parameters){
    run.model.for.parameters(location = LOCATION,
                             variable.parameters = c(base.parameters, youth.parameters),
                             parameters = model.parameters)
}

youth.transformations = sapply(YOUTH.PRIOR@subdistributions, function(dist){
    if(is.null(dist@transformation)) "identity" else dist@transformation@name
})
names(youth.transformations) = YOUTH.PRIOR@var.names
youth.sds = get.sds(YOUTH.PRIOR)[YOUTH.PARAMS]
youth.start = get.medians(YOUTH.PRIOR)[YOUTH.PARAMS]

safe = make.safe.mcmc.functions(simulation.function = youth.simulation,
                                log.likelihood = youth.likelihood,
                                log.prior = get.density.function(YOUTH.PRIOR, default.log = T),
                                error.log.file = file.path(MCMC.RUNS.DIR, "logs", paste0("south_africa_youth_errors_", Sys.Date(), ".csv")))

control = create.adaptive.blockwise.metropolis.control(var.names = YOUTH.PARAMS,
                                                       simulation.function = safe$simulation,
                                                       log.prior.distribution = safe$prior,
                                                       log.likelihood = safe$likelihood,
                                                       var.blocks = list(awareness = "age.0.14.awareness.multiplier",
                                                                         engagement = c("age.0.14.engagement.multiplier","log.OR.0.14.engagement.slope"),
                                                                         suppression = c("age.0.14.suppression.multiplier","log.OR.0.14.suppression.slope")),
                                                       transformations = youth.transformations,
                                                       initial.covariance.mat = diag((youth.sds/MCMC.INITIAL.STEP.SIZE.DIVISOR)^2),
                                                       burn = 0,
                                                       thin = 1)

dir.create(MCMC.CACHE.DIR, showWarnings = FALSE)
youth.mcmc = run.mcmc.with.cache(control = control,
                                 n.iter = N.ITER,
                                 starting.values = youth.start,
                                 update.frequency = 100,
                                 cache.frequency = 500,
                                 cache.dir = YOUTH.CACHE.DIR)
# if this stops partway: youth.mcmc = run.mcmc.from.cache(dir = YOUTH.CACHE.DIR, update.frequency = 100)

dir.create(file.path(MCMC.RUNS.DIR, "mcmc_files"), recursive = TRUE, showWarnings = FALSE)
save(youth.mcmc, BASE.INDEX, file = file.path(MCMC.RUNS.DIR, "mcmc_files", paste0("mcmc_south_africa_youth_", Sys.Date(), ".Rdata")))

# convergence: traces should look like flat, well-mixed noise after YOUTH.BURN, with no drift; if they're
# still drifting at the end, increase N.ITER and resume (run.mcmc.from.cache above)
print(trace.plot(youth.mcmc))

youth.simset = extract.simset(youth.mcmc, additional.burn = YOUTH.BURN, additional.thin = 1)
print(paste0("Youth posterior: ", youth.simset@n.sim, " distinct simulations"))
print(round(apply(youth.simset@parameters[,YOUTH.PARAMS], 2, quantile, probs = c(0.025,0.5,0.975)), 3))

# youth fit for the base simulation
invisible(simplot(youth.simset,
        years = 2000:2030,
        data.types = c('awareness','engagement','suppression'),
        facet.by = 'age',
        ages = c('0-14','15+'),
        proportion = T))

##------------------------------------------------------------------------------------------##
##-- 3. Pair every simulation with a youth posterior draw, re-run, save for LAI           --##
##------------------------------------------------------------------------------------------##
draw.rows = sample(1:youth.simset@n.sim, simset@n.sim, replace = T, prob = youth.simset@weights)
youth.draws = youth.simset@parameters[draw.rows, YOUTH.PARAMS, drop = F]
rownames(youth.draws) = NULL

simset.youth = simset
simset.youth@parameters = cbind(simset@parameters[, setdiff(colnames(simset@parameters), YOUTH.PARAMS), drop = F],
                                youth.draws)
simset.youth = run.intervention.on.simset(simset.youth, end.year = 2040, interventions = NO.INTERVENTION)

simset = simset.youth
save(simset, file = paste0("mcmc_runs/simset_south_africa_step2_chains1234_youth_", Sys.Date(), ".Rdata"))
print(paste0("Saved mcmc_runs/simset_south_africa_step2_chains1234_youth_", Sys.Date(), ".Rdata - use this for the LAI runs"))

# full fit, all simulations
invisible(simplot(simset,
        years = 2000:2040,
        data.types = c('awareness','engagement','suppression'),
        facet.by = 'age',
        ages = c('0-14','15+'),
        proportion = T))
invisible(simplot(simset, years = 1980:2040, data.types = c('incidence','prevalence')))

if(!interactive()) dev.off()
print("Youth calibration finished")
