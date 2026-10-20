################################################################################################
# Description: Profiles one MCMC iteration (with the C++ ODE) to find what to speed up next
#
#   1. Times each piece of an iteration: create.model.parameters, get.default.parameters,
#      map.model.parameters, get.initial.population, the C++ ODE, process.ode.results,
#      and the likelihood
#   2. Runs R's profiler (Rprof) over repeated model runs + likelihood and lists the
#      functions that take the most time
#
# Run from the project root:
#   source('model/profile_model_run.R')
# then paste the report printed at the end back to Claude.
################################################################################################

source('model/run_systematic.R')
source('model/diffeq_cpp.R')
options(gmha.use.cpp = TRUE)

PROFILE.LOCATION = "South Africa"
N.REPS = 20

LOCATION = PROFILE.LOCATION
details = set.likelihood.and.prior.by.location(location = PROFILE.LOCATION)
variable.parameters = details$params.start.values

report = character()
add = function(...) { line = paste0(...); report <<- c(report, line); cat(line, "\n") }

# median time of N.REPS calls, in milliseconds
time.it = function(expr.fn, reps = N.REPS){
    times = sapply(1:reps, function(i) system.time(expr.fn())[["elapsed"]])
    1000 * median(times)
}

##--------------------------------##
##-- 1. TIME EACH PIECE         --##
##--------------------------------##

# build the pieces once so each step can be timed on its own
parameters.base = create.model.parameters(location = PROFILE.LOCATION)
sampled.parameters = get.default.parameters(location = PROFILE.LOCATION)
sampled.parameters[names(variable.parameters)] = variable.parameters
parameters = map.model.parameters(parameters.base, location = PROFILE.LOCATION,
                                  sampled.parameters = sampled.parameters)
initial.state = get.initial.population(year = "1970", location = PROFILE.LOCATION, data.manager = DATA.MANAGER,
                                       parameters = parameters, model.age.cutoffs = MODEL.AGE.CUTOFFS,
                                       ages = parameters$AGES, sexes = parameters$SEXES,
                                       seed.to.ages = c(5,6,7,8), seed.to.sexes = c(1,2), seed.n = 1)
y0 = set.up.initial.diffeq.vector(initial.state, parameters)
spec = prepare.cpp.model.spec(parameters, max.run.seconds = Inf)
raw = gmha_run_model_cpp(spec, y0, start = 1970, duration = 2041 + 1 - 1970, step_size = 1)
sim = SIMULATION.FUNCTION(variable.parameters)

pieces = c(
    "create.model.parameters" = time.it(function() create.model.parameters(location = PROFILE.LOCATION)),
    "get.default.parameters" = time.it(function() get.default.parameters(location = PROFILE.LOCATION)),
    "map.model.parameters" = time.it(function() map.model.parameters(parameters.base, location = PROFILE.LOCATION,
                                                                     sampled.parameters = sampled.parameters)),
    "get.initial.population" = time.it(function() get.initial.population(
        year = "1970", location = PROFILE.LOCATION, data.manager = DATA.MANAGER,
        parameters = parameters, model.age.cutoffs = MODEL.AGE.CUTOFFS,
        ages = parameters$AGES, sexes = parameters$SEXES,
        seed.to.ages = c(5,6,7,8), seed.to.sexes = c(1,2), seed.n = 1)),
    "prepare.cpp.model.spec" = time.it(function() prepare.cpp.model.spec(parameters, max.run.seconds = Inf)),
    "C++ ODE" = time.it(function() gmha_run_model_cpp(spec, y0, start = 1970, duration = 2041 + 1 - 1970, step_size = 1)),
    "process.ode.results" = time.it(function() process.ode.results(raw, parameters = parameters, start.year = 1970,
                                                                   end.year = 2041, keep.years = 1970:2041,
                                                                   location = PROFILE.LOCATION)),
    "likelihood" = time.it(function() details$likelihood.to.run(sim)),
    "whole iteration (SIMULATION.FUNCTION + likelihood)" = time.it(function() details$likelihood.to.run(SIMULATION.FUNCTION(variable.parameters)))
)

add("==== Model run profile: ", PROFILE.LOCATION, " (", format(Sys.time(), "%Y-%m-%d %H:%M"), ") ====")
add("")
add("1. Median time per call (ms), ", N.REPS, " reps each")
for (nm in names(pieces))
    add("   ", formatC(nm, width = -52), formatC(pieces[[nm]], format = "f", digits = 1, width = 8))

##--------------------------------##
##-- 2. PROFILER                --##
##--------------------------------##

prof.file = tempfile(fileext = ".out")
Rprof(prof.file, interval = 0.002)
for (i in 1:N.REPS)
    details$likelihood.to.run(SIMULATION.FUNCTION(variable.parameters))
Rprof(NULL)
prof = summaryRprof(prof.file)

total.time = prof$sampling.time
show.table = function(tab, column, n){
    tab = tab[order(-tab[[column]]), , drop = FALSE]
    tab = head(tab, n)
    for (i in seq_len(nrow(tab)))
        add("   ", formatC(rownames(tab)[i], width = -45),
            formatC(100 * tab[[column]][i] / total.time, format = "f", digits = 1, width = 6), "%")
}

add("")
add("2a. Profiler, time INCLUDING functions each one calls (top 40; % of ", round(total.time, 2), "s)")
show.table(prof$by.total, "total.time", 40)
add("")
add("2b. Profiler, time spent in the function's OWN code (top 25)")
show.table(prof$by.self, "self.time", 25)

PROFILE.REPORT = report  # same text, e.g. writeLines(PROFILE.REPORT)
