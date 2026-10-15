################################################################################################
# Description: Checks the two speedups to the parameter setup and likelihood against the
#              original code paths, and times them
#
#   - Location cache (helpers/location_cache.R): get.testing.model, get.engagement.model,
#     get.suppression.rates, get.all.age.sex.transmission.multipliers run once per location
#     instead of every model run.          Off: options(gmha.use.location.cache = FALSE)
#   - Cached Cholesky factor in the likelihood (calibration/likelihood/likelihood.R) for
#     likelihoods whose covariance doesn't depend on the sim (population).
#                                          Off: options(gmha.cache.likelihood.chol = FALSE)
#
# For each location, runs two different parameter sets (MCMC starting values, and the same with
# transmission rates/multipliers x1.05) with both speedups off, then on, and compares the sims,
# each likelihood component, and the total log-likelihood. The second parameter set checks that
# the cached factor gives the right answer for a different sim than the one that created it.
#
# Run from the project root:
#   source('model/test_speedups.R')
# then paste the report printed at the end back to Claude.
################################################################################################

source('model/run_systematic.R')
source('model/diffeq_cpp.R')
options(gmha.use.cpp = TRUE)

TEST.LOCATIONS = c("South Africa", "r1.high")

report = character()
add = function(...) { line = paste0(...); report <<- c(report, line); cat(line, "\n") }
fmt = function(x) formatC(x, format = "e", digits = 2)
median.ms = function(fn, reps) 1000 * median(sapply(1:reps, function(i) system.time(fn())[["elapsed"]]))

set.speedups = function(on){
    options(gmha.use.location.cache = on, gmha.cache.likelihood.chol = on)
}

# compare every outcome array in two sims (returns worst relative difference)
compare.sims = function(sim.a, sim.b){
    nms = names(sim.a)[sapply(sim.a, is.array)]
    max(sapply(nms, function(nm){
        a = sim.a[[nm]]; b = sim.b[[nm]]
        if (!identical(dim(a), dim(b)) || any(is.na(a) != is.na(b))) return(Inf)
        scale = max(abs(a), na.rm = TRUE)
        if (scale == 0) return(max(abs(a - b), na.rm = TRUE))
        max(abs(a - b), na.rm = TRUE) / scale
    }))
}

add("==== Speedup check (", format(Sys.time(), "%Y-%m-%d %H:%M"), ") ====")
add("dmvnorm comes from: ", environmentName(environment(dmvnorm)),
    if (requireNamespace("mvtnorm", quietly = TRUE)) paste0(" | mvtnorm ", packageVersion("mvtnorm")) else "")

all.identical = TRUE

for (location in TEST.LOCATIONS) {

    LOCATION = location   # SIMULATION.FUNCTION uses the global LOCATION
    add("")
    add("---- ", location, " ----")

    details = set.likelihood.and.prior.by.location(location = location)
    likelihood = details$likelihood.to.run
    components = attr(likelihood, "components")

    start.values = details$params.start.values
    perturbed = start.values
    to.perturb = grepl("multiplier|trate", names(perturbed))
    perturbed[to.perturb] = perturbed[to.perturb] * 1.05
    param.sets = list(start.values = start.values, perturbed = perturbed)
    add("   perturbed set: ", sum(to.perturb), " transmission rate/multiplier parameters x 1.05")

    run.set = function(variable.parameters){
        sim = SIMULATION.FUNCTION(variable.parameters)
        list(sim = sim,
             components = sapply(components, function(f) f(sim)),
             total = likelihood(sim))
    }

    # original code paths
    set.speedups(FALSE)
    results.off = lapply(param.sets, run.set)
    time.off = median.ms(function() likelihood(SIMULATION.FUNCTION(start.values)), reps = 5)
    time.off.map = median.ms(function() map.model.parameters(create.model.parameters(location = location),
                                                             location = location,
                                                             sampled.parameters = replace(get.default.parameters(location),
                                                                                          names(start.values), start.values)),
                             reps = 5)
    sim.for.lik = results.off$start.values$sim
    time.off.lik = median.ms(function() likelihood(sim.for.lik), reps = 5)

    # with the speedups (likelihood cache gets created by the first sim, then reused for the second)
    set.speedups(TRUE)
    results.on = lapply(param.sets, run.set)
    time.on = median.ms(function() likelihood(SIMULATION.FUNCTION(start.values)), reps = 10)
    time.on.map = median.ms(function() map.model.parameters(create.model.parameters(location = location),
                                                            location = location,
                                                            sampled.parameters = replace(get.default.parameters(location),
                                                                                         names(start.values), start.values)),
                            reps = 10)
    time.on.lik = median.ms(function() likelihood(sim.for.lik), reps = 10)

    for (set in names(param.sets)) {
        off = results.off[[set]]; on = results.on[[set]]
        sim.diff = compare.sims(off$sim, on$sim)
        params.identical = identical(off$sim$parameters$time.varying.parameters,
                                     on$sim$parameters$time.varying.parameters)
        comp.diff = max(abs(off$components - on$components))
        total.diff = off$total - on$total
        ok = params.identical && sim.diff == 0 && comp.diff == 0 && total.diff == 0
        all.identical = all.identical && ok
        add("   [", set, "] parameters identical: ", params.identical,
            " | sim difference: ", fmt(sim.diff),
            " | worst likelihood component difference: ", fmt(comp.diff))
        add("   [", set, "] log-likelihood: original = ", format(off$total, digits = 12),
            " | with speedups = ", format(on$total, digits = 12),
            if (ok) "  IDENTICAL" else "  DIFFERENT")
        if (comp.diff != 0) {
            bad = names(off$components)[off$components != on$components]
            add("      components that differ: ", paste0(bad, collapse = ", "))
        }
    }

    add("   time (ms)              original   with speedups")
    add("   map.model.parameters   ", formatC(time.off.map, format = "f", digits = 0, width = 8),
        formatC(time.on.map, format = "f", digits = 0, width = 14))
    add("   likelihood             ", formatC(time.off.lik, format = "f", digits = 0, width = 8),
        formatC(time.on.lik, format = "f", digits = 0, width = 14))
    add("   whole iteration        ", formatC(time.off, format = "f", digits = 0, width = 8),
        formatC(time.on, format = "f", digits = 0, width = 14),
        "   (", round(time.off / time.on, 1), "x faster)")
}

set.speedups(TRUE)
add("")
add("==== Summary: ", if (all.identical) "all results IDENTICAL with the speedups on" else "SOME RESULTS DIFFER - see above", " ====")

SPEEDUP.REPORT = report
