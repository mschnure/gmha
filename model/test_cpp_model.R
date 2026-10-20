################################################################################################
# Description: Checks the C++ model (model/diffeq_cpp.cpp) against the R model (model/diffeq.R)
#
#   1. Derivatives: compute.dx vs compute.dx.cpp at several times/states
#        (same arithmetic, should agree to ~1e-12 relative or better)
#   2. Raw ODE output: odeintr + compute.dx vs the C++ integrator, block by block
#        (same integrator and tolerances, should agree to ~1e-8 relative or better)
#   3. End to end: run.model.for.parameters with R vs C++ (every outcome array in the sim),
#        plus the log-likelihood of each sim
#   4. Timing: R ODE vs C++ ODE, and how long the rest of a model run takes
#
# Uses each location's MCMC starting values (from set.likelihood.and.prior.by.location).
#
# Run from the project root (same working directory as your other scripts):
#   source('model/test_cpp_model.R')
# then paste the report printed at the end back to Claude.
################################################################################################

source('model/run_systematic.R')  # sources source_code.R; defines run.model.for.parameters
source('model/diffeq_cpp.R')      # compiles the C++ code (~30-60 seconds the first time)

TEST.LOCATIONS = c("South Africa", "r1.high")
START.YEAR = 1970
END.YEAR = 2041           # run.model.for.parameters default
N.CPP.TIMING.REPS = 20

##---------------------##
##-- REPORT HELPERS  --##
##---------------------##

report = character()
add = function(...) { line = paste0(...); report <<- c(report, line); cat(line, "\n") }
fmt = function(x) formatC(x, format = "e", digits = 2)

rel.diff = function(a, b){
    if (any(is.na(a) != is.na(b))) return(Inf)
    scale = max(abs(a), na.rm = TRUE)
    if (scale == 0) return(max(abs(a - b), na.rm = TRUE))
    max(abs(a - b), na.rm = TRUE) / scale
}

##---------------------##
##-- SETUP HELPERS   --##
##---------------------##

# Likelihood + MCMC starting values for a location (from set.likelihood.and.prior.by.location);
# falls back to default parameters (and no likelihood) if that can't be set up
get.test.details = function(location){
    defaults = get.default.parameters(location = location)
    details = tryCatch(set.likelihood.and.prior.by.location(location = location),
                       error = function(e){
                           add("   (likelihood/starting values not set up: ", conditionMessage(e),
                               "; using default parameters)")
                           NULL })
    variable.parameters = defaults
    if (!is.null(details) && !is.null(details$params.start.values)) {
        sv = details$params.start.values
        sv = sv[intersect(names(sv), names(defaults))]
        variable.parameters[names(sv)] = sv
        add("   using ", length(sv), " MCMC starting values")
    }
    list(variable.parameters = variable.parameters,
         likelihood = if (is.null(details)) NULL else details$likelihood.to.run)
}

# Same steps as run.model.for.parameters (model/run_systematic.R), up to the ODE
build.test.inputs = function(location, variable.parameters){
    sampled.parameters = get.default.parameters(location = location)
    sampled.parameters[names(variable.parameters)] = variable.parameters
    parameters = create.model.parameters(location = location)
    parameters = map.model.parameters(parameters,
                                      location = location,
                                      sampled.parameters = sampled.parameters,
                                      interventions = NO.INTERVENTION)
    initial.state = get.initial.population(year = "1970",
                                           location = location,
                                           data.manager = DATA.MANAGER,
                                           parameters = parameters,
                                           model.age.cutoffs = MODEL.AGE.CUTOFFS,
                                           ages = parameters$AGES,
                                           sexes = parameters$SEXES,
                                           seed.to.ages = c(5,6,7,8),
                                           seed.to.sexes = c(1,2),
                                           seed.n = 1)
    parameters$model.run.start = as.numeric(Sys.time())
    parameters$max.run.time = Inf   # no time limit for the comparison
    list(parameters = parameters, initial.state = initial.state)
}

# Blocks of the flattened ODE vector, in the order compute.dx returns them
get.blocks = function(parameters){
    T.len = length(parameters$AGES) * length(parameters$SEXES) * length(parameters$SUBGROUPS)
    S.len = T.len * length(parameters$HIV.STATUS)
    P.len = T.len * length(parameters$HIV.STATES)
    lengths = c(population = S.len, prevalence = P.len, incidence = T.len, diagnoses = T.len,
                hiv.mortality = S.len, non.hiv.mortality = S.len, total.mortality = S.len,
                engagement = T.len, disengagement.unsuppressed = T.len,
                disengagement.suppressed = T.len, suppression = T.len)
    ends = cumsum(lengths)
    starts = ends - lengths + 1
    setNames(lapply(seq_along(lengths), function(i) starts[i]:ends[i]), names(lengths))
}

##---------------------##
##-- RUN THE TESTS   --##
##---------------------##

add("==== C++ model check (", format(Sys.time(), "%Y-%m-%d %H:%M"), ") ====")
add("R ", R.version$major, ".", R.version$minor, " | Rcpp ", as.character(packageVersion("Rcpp")),
    " | BH ", as.character(packageVersion("BH")), " | odeintr ", as.character(packageVersion("odeintr")))

worst = c(derivative = 0, raw.ode = 0, end.to.end = 0)

for (location in TEST.LOCATIONS) {

    LOCATION = location  # some functions use the global LOCATION
    add("")
    add("---- ", location, " ----")

    test.details = get.test.details(location)
    variable.parameters = test.details$variable.parameters

    timing.setup = system.time(inputs <- build.test.inputs(location, variable.parameters))[["elapsed"]]
    parameters = inputs$parameters
    y0 = set.up.initial.diffeq.vector(inputs$initial.state, parameters)
    blocks = get.blocks(parameters)
    duration = END.YEAR + 1 - START.YEAR

    ## Raw ODE: R
    timing.R = system.time(
        raw.R <- odeintr::integrate_sys(sys = function(x, t){ compute.dx(time = t, y = x, parameters = parameters) },
                                        init = y0, start = START.YEAR, duration = duration, step_size = 1)
    )[["elapsed"]]
    raw.R = as.matrix(raw.R)
    if (timing.R > 8)
        add("   NOTE: the R ODE took ", round(timing.R, 1), "s; run.model stops at 10s (max.run.seconds), ",
            "so the R end-to-end run below may come back NA")

    ## Raw ODE: C++
    spec = prepare.cpp.model.spec(parameters, max.run.seconds = Inf)
    timing.C = system.time(
        for (i in 1:N.CPP.TIMING.REPS)
            raw.C <- gmha_run_model_cpp(spec, y0, start = START.YEAR, duration = duration, step_size = 1)
    )[["elapsed"]] / N.CPP.TIMING.REPS
    timing.spec = system.time(
        for (i in 1:N.CPP.TIMING.REPS) prepare.cpp.model.spec(parameters)
    )[["elapsed"]] / N.CPP.TIMING.REPS

    ## 1. Derivatives at a range of times, using states from the R run
    deriv.diffs = sapply(c(1970, 1975.3, 1985, 1990.5, 2000, 2012.7, 2025, 2041), function(t){
        row = which.min(abs(raw.R[, 1] - floor(t)))
        y = as.numeric(raw.R[row, -1])
        d.R = compute.dx(time = t, y = y, parameters = parameters)
        d.C = compute.dx.cpp(time = t, y = y, parameters = parameters)
        rel.diff(d.R, d.C)
    })
    worst["derivative"] = max(worst["derivative"], max(deriv.diffs))
    add("1. Derivatives (8 time points): worst relative difference = ", fmt(max(deriv.diffs)))

    ## 2. Raw ODE output, block by block
    add("2. Raw ODE output: ", nrow(raw.R), " rows (R) vs ", nrow(raw.C), " rows (C++)")
    if (!identical(dim(raw.R), dim(raw.C))) {
        add("   DIMENSIONS DIFFER - skipping the rest of this location")
        worst["raw.ode"] = Inf
        next
    }
    for (b in names(blocks)) {
        d = rel.diff(raw.R[, 1 + blocks[[b]]], raw.C[, 1 + blocks[[b]]])
        worst["raw.ode"] = max(worst["raw.ode"], d)
        add("   ", formatC(b, width = -27), fmt(d))
    }

    ## 3. End to end through run.model.for.parameters
    options(gmha.use.cpp = FALSE)
    timing.full.R = system.time(
        sim.R <- run.model.for.parameters(location = location, variable.parameters = variable.parameters)
    )[["elapsed"]]
    options(gmha.use.cpp = TRUE)
    timing.full.C = system.time(
        sim.C <- run.model.for.parameters(location = location, variable.parameters = variable.parameters)
    )[["elapsed"]]
    options(gmha.use.cpp = FALSE)

    outcome.names = names(sim.R)[sapply(sim.R, is.array)]
    e2e = sapply(outcome.names, function(nm){
        if (!identical(dim(sim.R[[nm]]), dim(sim.C[[nm]])) ||
            !identical(dimnames(sim.R[[nm]]), dimnames(sim.C[[nm]])))
            return(Inf)
        rel.diff(sim.R[[nm]], sim.C[[nm]])
    })
    worst["end.to.end"] = max(worst["end.to.end"], max(e2e))
    add("3. End to end (", length(outcome.names), " outcome arrays): worst relative difference = ",
        fmt(max(e2e)), " (", names(e2e)[which.max(e2e)], ")")

    if (!is.null(test.details$likelihood)) {
        lik = tryCatch(c(R = test.details$likelihood(sim.R), Cpp = test.details$likelihood(sim.C)),
                       error = function(e) { add("   (log-likelihood not computed: ", conditionMessage(e), ")"); NULL })
        if (!is.null(lik))
            add("   log-likelihood: R = ", format(lik[["R"]], digits = 12), " | C++ = ", format(lik[["Cpp"]], digits = 12),
                " | difference = ", fmt(lik[["R"]] - lik[["Cpp"]]))
    }

    ## 4. Timing
    add("4. Timing (seconds): ODE in R = ", round(timing.R, 3), " | ODE in C++ = ", round(timing.C, 4),
        " (", round(timing.R / timing.C), "x faster)")
    add("   C++ input prep = ", round(timing.spec, 4),
        " | parameter setup (map.model.parameters + initial population) = ", round(timing.setup, 3))
    add("   full run.model.for.parameters: R = ", round(timing.full.R, 3), " | C++ = ", round(timing.full.C, 3))
}

add("")
add("==== Summary ====")
add("worst derivative difference: ", fmt(worst["derivative"]), if (worst["derivative"] < 1e-10) "  OK" else "  CHECK")
add("worst raw ODE difference:    ", fmt(worst["raw.ode"]), if (worst["raw.ode"] < 1e-6) "  OK" else "  CHECK")
add("worst end-to-end difference: ", fmt(worst["end.to.end"]), if (worst["end.to.end"] < 1e-6) "  OK" else "  CHECK")
add("(relative differences = max |R - C++| / max |R| within each block)")

CPP.TEST.REPORT = report  # the same text, e.g. writeLines(CPP.TEST.REPORT)
