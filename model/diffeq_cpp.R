################################################################################################
# Description: R side of the C++ model (model/diffeq_cpp.cpp)
################################################################################################

# Functions
#     1. prepare.cpp.model.spec
#         Converts parameters$time.varying.parameters into the flat format the C++ code reads
#         (each knot's value expanded to the length it is used at, following R's recycling rules)
#     2. run.model.cpp
#         Drop-in replacement for run.model (model/diffeq.R): same arguments, same output
#         (uses the same set.up.initial.diffeq.vector and process.ode.results)
#     3. compute.dx.cpp
#         One derivative evaluation, for comparing against compute.dx
#
# Usage: source this file after source_code.R. To make run.model (and therefore
# run.model.for.parameters / SIMULATION.FUNCTION) use C++, set options(gmha.use.cpp = TRUE).

library(Rcpp)
# Compiles into a build folder in the project (not the session's temp folder), so it compiles once
# and later R sessions - e.g., the MCMC pipeline's parallel sessions - load the compiled model
# without compiling (and so don't need Rtools); recompiles automatically if diffeq_cpp.cpp changes
GMHA.CPP.BUILD.DIR = file.path('cached', 'cpp_build')
dir.create(GMHA.CPP.BUILD.DIR, recursive = TRUE, showWarnings = FALSE)
# pkgbuild puts Rtools on the PATH for the compile, as the RStudio console does; R sessions started
# outside the console (e.g., by an RStudio Background Job) may not have it otherwise
if (requireNamespace('pkgbuild', quietly = TRUE)) {
    pkgbuild::with_build_tools(Rcpp::sourceCpp('model/diffeq_cpp.cpp', cacheDir = GMHA.CPP.BUILD.DIR),
                               required = FALSE) # first time ~30-60 seconds
} else {
    Rcpp::sourceCpp('model/diffeq_cpp.cpp', cacheDir = GMHA.CPP.BUILD.DIR)
}

# How each time-varying parameter is used in compute.dx, and so what length the C++ code expects:
#   state: [age, sex, subgroup, hiv.status]; trans: [age, sex, subgroup];
#   trans.matrix: trans x trans (TRANSMISSION.RATES %*% ...); scalar: a single number
CPP.TIME.VARYING.PARAMETER.SHAPES = c(
    FERTILITY.RATES = 'state',
    MATERNAL.FETAL.TRANSMISSION = 'state',
    MALE.BIRTHS = 'scalar',
    AGING.RATES = 'state',
    HIV.MORTALITY.RATES = 'state',
    NON.HIV.MORTALITY.RATES = 'state',
    TESTING.RATES = 'trans',
    TRANSMISSION.RATES = 'trans.matrix',
    INFECTIOUSNESS.H = 'state',
    ENGAGEMENT.RATES = 'trans',
    UNSUPPRESSED.DISENGAGEMENT.RATES = 'trans',
    SUPPRESSED.DISENGAGEMENT.RATES = 'trans',
    SUPPRESSION.RATES = 'trans',
    UNSUPPRESSION.RATES = 'trans'
)

prepare.cpp.model.spec <- function(parameters,
                                   max.run.seconds = Inf){

    tvp = parameters$time.varying.parameters

    # the C++ code is written for exactly these parameters; stop if compute.dx has moved on
    missing.params = setdiff(names(CPP.TIME.VARYING.PARAMETER.SHAPES), names(tvp))
    extra.params = setdiff(names(tvp), names(CPP.TIME.VARYING.PARAMETER.SHAPES))
    if (length(missing.params) > 0)
        stop(paste0("Time-varying parameter(s) expected by the C++ model are missing: ",
                    paste0(missing.params, collapse = ", ")))
    if (length(extra.params) > 0)
        stop(paste0("Time-varying parameter(s) not handled by the C++ model (update model/diffeq_cpp.cpp): ",
                    paste0(extra.params, collapse = ", ")))

    trans.length = length(parameters$AGES) * length(parameters$SEXES) * length(parameters$SUBGROUPS)
    state.length = trans.length * length(parameters$HIV.STATUS)
    shape.lengths = c(state = state.length,
                      trans = trans.length,
                      trans.matrix = trans.length^2,
                      scalar = 1)

    params = lapply(names(CPP.TIME.VARYING.PARAMETER.SHAPES), function(name){
        param = tvp[[name]]
        len = shape.lengths[[CPP.TIME.VARYING.PARAMETER.SHAPES[[name]]]]

        values = vapply(param$values, function(val){
            val = as.numeric(val)
            # same recycling R does when compute.dx multiplies this value by the state
            if (length(val) == 0 || len %% length(val) != 0)
                stop(paste0("Value for ", name, " has length ", length(val),
                            ", which does not recycle evenly to the expected length ", len))
            rep_len(val, len)
        }, numeric(len))

        list(times = as.numeric(param$times),
             values = matrix(values, nrow = len, ncol = length(param$values)))
    })
    names(params) = names(CPP.TIME.VARYING.PARAMETER.SHAPES)

    # dimensions and 0-based indices of the named sexes/hiv statuses compute.dx refers to
    dims = c(n.ages = length(parameters$AGES),
             n.sexes = length(parameters$SEXES),
             n.subgroups = length(parameters$SUBGROUPS),
             n.hiv.status = length(parameters$HIV.STATUS),
             male = match('male', parameters$SEXES) - 1,
             female = match('female', parameters$SEXES) - 1,
             hiv_negative = match('hiv_negative', parameters$HIV.STATUS) - 1,
             undiagnosed = match('undiagnosed', parameters$HIV.STATUS) - 1,
             diagnosed_unengaged = match('diagnosed_unengaged', parameters$HIV.STATUS) - 1,
             engaged_unsuppressed = match('engaged_unsuppressed', parameters$HIV.STATUS) - 1,
             engaged_suppressed = match('engaged_suppressed', parameters$HIV.STATUS) - 1)
    if (any(is.na(dims)))
        stop(paste0("Could not find: ", paste0(names(dims)[is.na(dims)], collapse = ", ")))
    if (dims[['hiv_negative']] != 0)
        stop("compute.dx assumes 'hiv_negative' is the first HIV status (prevalence = dx.state[,,,-1])")
    storage.mode(dims) = 'integer'

    list(dims = dims,
         params = params,
         max.run.seconds = as.numeric(max.run.seconds))
}


# Same arguments and output as run.model (model/diffeq.R)
run.model.cpp <- function(parameters,
                          initial.state,
                          start.year,
                          end.year,
                          keep.years,
                          location,
                          max.run.seconds=10){

    # Error check on parameters - NA values (same check as run.model)
    mask = sapply(parameters$time.varying.parameters,function(param){any(sapply(param$values,function(val){any(is.na(val)) | any(is.infinite(val))}))})
    if(any(mask))
        stop(paste0("NA parameter values found in: ",
                    paste0(names(parameters$time.varying.parameters)[mask],collapse=", ")))

    spec = prepare.cpp.model.spec(parameters, max.run.seconds = max.run.seconds)

    ode.results = gmha_run_model_cpp(spec = spec,
                                     init = set.up.initial.diffeq.vector(initial.state, parameters),
                                     start = start.year,
                                     duration = end.year + 1 - start.year,
                                     step_size = 1)

    process.ode.results(ode.results,
                        parameters=parameters,
                        start.year=start.year,
                        end.year=end.year,
                        keep.years=keep.years,
                        location=location)
}


# One derivative evaluation, returns the same vector as compute.dx(time, y, parameters)
compute.dx.cpp <- function(time, y, parameters){
    gmha_compute_dx_cpp(spec = prepare.cpp.model.spec(parameters),
                        time = time,
                        y = y)
}
