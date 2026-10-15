################################################################################################
# Description: Checks the map.model.parameters speedups against the last committed version
#
# Changed files (compared against their committed version in git, HEAD):
#   - model/parameter_mappings/age_sex_mixing_proportions.R  (age mixing matrix built once per sex)
#   - model/parameters.R             (mixing matrices reused; engagement/suppression vectorized)
#   - model/parameter_mappings/calculate_death_rates.R  (death-rate regression fits cached)
#
# For each location and two parameter sets (MCMC starting values, and transmission rates/
# multipliers x1.05), builds the parameters, runs the sim, and computes every likelihood
# component with the OLD code, then the NEW code, and compares. Also times both.
#
# Run from the project root:
#   source('model/test_parameter_speedups.R')
# then paste the report printed at the end back to Claude.
################################################################################################

source('model/run_systematic.R')
source('model/diffeq_cpp.R')
options(gmha.use.cpp = TRUE)

TEST.LOCATIONS = c("South Africa", "r1.high")
CHANGED.FILES = c("model/parameter_mappings/age_sex_mixing_proportions.R",
                  "model/parameter_mappings/calculate_death_rates.R",
                  "model/parameters.R")

report = character()
add = function(...) { line = paste0(...); report <<- c(report, line); cat(line, "\n") }
fmt = function(x) formatC(x, format = "e", digits = 2)
median.ms = function(fn, reps) 1000 * median(sapply(1:reps, function(i) system.time(fn())[["elapsed"]]))

# source the changed files at a given version: "HEAD" (last commit) or "working" (current files)
use.version = function(version){
    for (f in CHANGED.FILES) {
        if (version == "working") {
            source(f)
        } else {
            lines = system2("git", c("show", paste0(version, ":", f)), stdout = TRUE)
            tmp = tempfile(fileext = ".R")
            writeLines(lines, tmp)
            source(tmp)
        }
    }
}

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

add("==== map.model.parameters speedup check (", format(Sys.time(), "%Y-%m-%d %H:%M"), ") ====")
add("comparing against commit ", system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE))

all.identical = TRUE

for (location in TEST.LOCATIONS) {

    LOCATION = location
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

    sampled.for = function(v) replace(get.default.parameters(location), names(v), v)
    run.set = function(v){
        params = map.model.parameters(create.model.parameters(location = location), location = location,
                                      sampled.parameters = sampled.for(v))
        sim = SIMULATION.FUNCTION(v)
        list(tvp = params$time.varying.parameters,
             sim = sim,
             components = sapply(components, function(f) f(sim)),
             total = likelihood(sim))
    }
    time.map = function(v) median.ms(function() map.model.parameters(create.model.parameters(location = location),
                                                                     location = location,
                                                                     sampled.parameters = sampled.for(v)), reps = 10)
    time.iteration = function(v) median.ms(function() likelihood(SIMULATION.FUNCTION(v)), reps = 10)

    use.version("HEAD")
    results.old = lapply(param.sets, run.set)
    t.map.old = time.map(start.values)
    t.iter.old = time.iteration(start.values)

    use.version("working")
    results.new = lapply(param.sets, run.set)
    t.map.new = time.map(start.values)
    t.iter.new = time.iteration(start.values)

    for (set in names(param.sets)) {
        old = results.old[[set]]; new = results.new[[set]]
        tvp.identical = identical(old$tvp, new$tvp)
        sim.diff = compare.sims(old$sim, new$sim)
        ok = tvp.identical && isTRUE(sim.diff == 0) &&
            identical(old$components, new$components) && identical(old$total, new$total)
        all.identical = all.identical && ok
        add("   [", set, "] time-varying parameters identical: ", tvp.identical,
            " | sim difference: ", fmt(sim.diff))
        if (!tvp.identical) {
            bad = names(old$tvp)[!mapply(identical, old$tvp[names(old$tvp)], new$tvp[names(old$tvp)])]
            add("      parameters that differ: ", paste0(bad, collapse = ", "))
        }
        add("   [", set, "] log-likelihood: old = ", format(old$total, digits = 12),
            " | new = ", format(new$total, digits = 12), if (ok) "  IDENTICAL" else "  DIFFERENT")
    }
    add("   time (ms)              old      new")
    add("   map.model.parameters ", formatC(t.map.old, format = "f", digits = 0, width = 5),
        formatC(t.map.new, format = "f", digits = 0, width = 9),
        "   (", round(t.map.old / t.map.new, 1), "x faster)")
    add("   whole iteration      ", formatC(t.iter.old, format = "f", digits = 0, width = 5),
        formatC(t.iter.new, format = "f", digits = 0, width = 9),
        "   (", round(t.iter.old / t.iter.new, 1), "x faster)")
}

use.version("working")
add("")
add("==== Summary: ", if (all.identical) "all results IDENTICAL with the new code" else "SOME RESULTS DIFFER - see above", " ====")

PARAMETER.SPEEDUP.REPORT = report
