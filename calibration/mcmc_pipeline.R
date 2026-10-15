################################################################################################
# Description: Runs the two-step MCMC calibration
#
# Functions
#     1. run.mcmc.step.chain: runs one chain of one step for one location (create or resume the
#        cache, save the mcmc and simset; Step 1 also saves the Step 2 starting values)
#     2. merge.step2.chains: merges the Step 2 chains for a location (most recent file for each chain)
#     3. run.mcmc.pipeline: for one or more locations, runs Step 1, then all Step 2 chains in
#        parallel (separate R sessions), then merges the Step 2 chains
#     4. use.cpp.model.if.possible: compiles the C++ ODE, falls back to the R version if it can't
#     5. make.safe.mcmc.functions: wraps the simulation, likelihood and prior so an error (or an
#        NA/NaN likelihood) for a proposed parameter set rejects that proposal instead of crashing
#        the chain; each one is saved, with its parameters, to <run>_errors_<date>.csv in the logs
#
# Settings (iterations, weights, chains, seeds, Step 1 starting value files, folders) are in
# calibration/mcmc_settings.R.
#
# Output names (location as in convert_string, e.g. "south_africa"):
#   cache:   mcmc_cache/<location>_step<step>_chain<chain>/
#   mcmc:    mcmc_runs/mcmc_files/mcmc_<location>_step<step>_chain<chain>_<date>.Rdata
#   simset:  mcmc_runs/simset_<location>_step<step>_chain<chain>_<date>.Rdata
#   merged:  mcmc_runs/mcmc_files/merged/mcmc_<location>_step2_chains1234_<date>.Rdata
#            mcmc_runs/simset_<location>_step2_chains1234_<date>.Rdata
#   logs:    mcmc_runs/logs/<location>_step<step>_chain<chain>_<date>.log
#            mcmc_runs/logs/<location>_step<step>_chain<chain>_errors_<date>.csv (rejected proposals that errored)
#   Step 2 starting values: calibration/starting_values/<date>_<location>_step2_start_values.Rdata
################################################################################################

library(bayesian.simulations)
library(distributions)

# Fix for bayesian.simulations 0.2.2 on R >= 4.3: its internal check.chains() uses && / || on the
# whole vector of chain numbers, which R 4.3 made an error for vectors longer than 1. So every
# function given an mcmc with more than one chain (extract.simset, get.rhats, likelihood.plot,
# trace.plot, ...) failed with "'length = 4' in coercion to 'logical(1)'". This replacement does the
# same checks element by element and returns the same value.
local({
    fixed.check.chains = function(mcmc, chains)
    {
        if (!is.numeric(chains) || length(chains) == 0 || any(is.na(chains)) || any(round(chains) != chains))
            stop(paste0("'chains' must be set of non-NA integers between 1 and ", mcmc@n.chains))
        if (any(chains < 1) || any(chains > mcmc@n.chains))
            stop(paste0("'chains' must be set of non-NA integers between 1 and ", mcmc@n.chains))
        unique(chains)
    }
    utils::assignInNamespace("check.chains", fixed.check.chains, ns = "bayesian.simulations")
})

mcmc.step.settings = function(step){
    settings = MCMC.STEP.SETTINGS[[paste0("step", step)]]
    if (is.null(settings))
        stop(paste0("No settings for step ", step, " in MCMC.STEP.SETTINGS (calibration/mcmc_settings.R)"))
    settings
}

mcmc.run.name = function(location, step, chain){
    paste0(convert_string(location), "_step", step, "_chain", chain)
}

mcmc.cache.dir = function(location, step, chain){
    file.path(MCMC.CACHE.DIR, mcmc.run.name(location, step, chain))
}

# Compiles the C++ ODE (model/diffeq_cpp.R) and turns it on; if compiling fails (e.g., no compiler
# on this machine), warns and uses the R version (identical results, slower)
use.cpp.model.if.possible = function(){
    ok = tryCatch({
        source('model/diffeq_cpp.R')
        TRUE
    }, error = function(e){
        warning(paste0("Could not compile the C++ model (", conditionMessage(e), "). Using the R version of the ODE."))
        FALSE
    })
    options(gmha.use.cpp = ok)
    invisible(ok)
}


mcmc.seed = function(step, chain){
    MCMC.BASE.SEED + 1000*step + chain
}

# "f1 > f2 > f3": the functions that were running when an error happened (innermost last),
# leaving out the error-handling machinery
summarize.call.stack = function(calls){
    fns = vapply(calls, function(cl) substr(paste(deparse(cl[[1]], nlines = 1), collapse = ""), 1, 50), "")
    fns = fns[!grepl("^(tryCatch|tryCatchList|tryCatchOne|doTryCatch|withCallingHandlers|fn|run.logged|h|stop|.handleSimpleError|simpleError|eval|FUN|lapply|sapply|vapply|function.*)$", fns)]
    paste(tail(fns, 15), collapse = " > ")
}

mcmc.error.log.file = function(location, step, chain, date = Sys.Date()){
    file.path(MCMC.RUNS.DIR, "logs", paste0(mcmc.run.name(location, step, chain), "_errors_", date, ".csv"))
}

# The sampler (bayesian.simulations) doesn't catch errors: an error in the simulation or likelihood for
# one proposed parameter set stops the whole chain, and resuming replays the same proposal (the cache
# fixes the random numbers), so it crashes again. These wrappers treat such a proposal as having zero
# likelihood (it is rejected) and save the error message and parameters so the cause can be fixed.
make.safe.mcmc.functions = function(simulation.function,
                                    log.likelihood,
                                    log.prior,
                                    error.log.file){
    n.errors = 0
    last.parameters = NULL

    log.error = function(where, message, parameters, call.stack = ""){
        n.errors <<- n.errors + 1
        dir.create(dirname(error.log.file), recursive = TRUE, showWarnings = FALSE)
        row = data.frame(time = format(Sys.time()), where = where,
                         message = gsub("[\r\n]+", " ", message),
                         call.stack = call.stack,
                         t(parameters), check.names = FALSE)
        new.file = !file.exists(error.log.file)
        write.table(row, file = error.log.file, sep = ",", row.names = FALSE,
                    col.names = new.file, append = !new.file, qmethod = "double")
        if (n.errors <= 20 || n.errors %% 100 == 0)
            print(paste0("Rejected a proposal: error #", n.errors, " (in ", where, "): ", message,
                         " - parameters saved to ", error.log.file))
    }

    # runs fn(); on an error, logs it (with the chain of function calls that led to it) and returns value.on.error
    run.logged = function(where, fn, parameters, value.on.error){
        call.stack = ""
        tryCatch(withCallingHandlers(fn(),
                                     error = function(e){ call.stack <<- summarize.call.stack(sys.calls()) }),
                 error = function(e){ log.error(where, conditionMessage(e), parameters, call.stack); value.on.error })
    }

    list(
        simulation = function(sampled.parameters){
            last.parameters <<- sampled.parameters
            run.logged("simulation", function() simulation.function(sampled.parameters), sampled.parameters, NULL)
        },
        likelihood = function(sim){
            if (is.null(sim))
                return(-Inf)  # the simulation errored (already logged)
            ll = run.logged("likelihood", function() log.likelihood(sim), last.parameters, -Inf)
            if (length(ll) != 1 || is.na(ll) || ll == Inf) {
                log.error("likelihood", paste0("likelihood returned ", paste0(ll, collapse = ", ")), last.parameters)
                ll = -Inf
            }
            ll
        },
        prior = function(parameters){
            lp = run.logged("prior", function() log.prior(parameters), parameters, -Inf)
            if (length(lp) != 1 || is.na(lp)) {
                log.error("prior", paste0("prior returned ", paste0(lp, collapse = ", ")), parameters)
                lp = -Inf
            }
            lp
        },
        n.errors = function() n.errors
    )
}


##-------------------------------##
##-- 1. ONE CHAIN OF ONE STEP  --##
##-------------------------------##

# resume = TRUE picks up an existing cache for this location/step/chain (e.g., after a crash);
# resume = FALSE starts fresh and overwrites any existing cache for it
run.mcmc.step.chain = function(location,
                               step,
                               chain = 1,
                               resume = FALSE){

    settings = mcmc.step.settings(step)
    if (!(chain %in% settings$chains))
        stop(paste0("Step ", step, " is set up for chain(s) ", paste0(settings$chains, collapse = ", "), ", not chain ", chain))

    assign("LOCATION", location, envir = globalenv()) # SIMULATION.FUNCTION uses the global LOCATION
    cache.dir = mcmc.cache.dir(location, step, chain)
    start.time = Sys.time()

    print(paste0("==== ", location, " | Step ", step, " | chain ", chain, " | ", format(settings$n.iter, big.mark = ","),
                 " iterations | weighted prevalence = ", settings$weighted.prevalence,
                 " | total mortality weight = ", settings$total.mortality.weight,
                 " | total weight = ", settings$total.weight,
                 " | seed = ", mcmc.seed(step, chain),
                 " | C++ ODE = ", isTRUE(getOption("gmha.use.cpp")), " ===="))

    has.cache = dir.exists(cache.dir) &&
        isTRUE(tryCatch(bayesian.simulations:::cache.exists(cache.dir), error = function(e) FALSE))

    if (resume && has.cache) {
        if (is.mcmc.cache.complete(cache.dir))
            print(paste0("Cache ", cache.dir, " is already complete; saving its results"))
        else {
            print(paste0("Resuming from cache ", cache.dir))
            run.mcmc.from.cache(dir = cache.dir, update.frequency = MCMC.UPDATE.FREQUENCY,
                                remove.cache.when.done = FALSE)
        }
    } else {
        set.seed(mcmc.seed(step, chain))
        details = set.likelihood.and.prior.by.location(location = location,
                                                       total.weight = settings$total.weight,
                                                       weighted.prevalence = settings$weighted.prevalence,
                                                       total.mortality.weight = settings$total.mortality.weight,
                                                       step = step)
        safe = make.safe.mcmc.functions(simulation.function = SIMULATION.FUNCTION,
                                        log.likelihood = details$likelihood.to.run,
                                        log.prior = get.density.function(details$prior, default.log = T),
                                        error.log.file = mcmc.error.log.file(location, step, chain))
        control = create.adaptive.blockwise.metropolis.control(var.names = details$prior@var.names,
                                                               simulation.function = safe$simulation,
                                                               log.prior.distribution = safe$prior,
                                                               log.likelihood = safe$likelihood,
                                                               var.blocks = PARAMETER.VAR.BLOCKS, # set in calibration/prior_distributions/var_blocks.R
                                                               transformations = details$transformations,
                                                               initial.covariance.mat = diag((details$sds/MCMC.INITIAL.STEP.SIZE.DIVISOR)^2), # step size
                                                               burn = 0,
                                                               thin = 1)
        dir.create(dirname(cache.dir), recursive = TRUE, showWarnings = FALSE) # the sampler only creates the last folder
        run.mcmc.with.cache(control = control,
                            n.iter = settings$n.iter,
                            starting.values = details$params.start.values,
                            update.frequency = MCMC.UPDATE.FREQUENCY,
                            cache.frequency = MCMC.CACHE.FREQUENCY,
                            cache.dir = cache.dir,
                            remove.cache.when.done = FALSE) # keep it: the mcmc is assembled from it below
    }

    # Save the mcmc and simset
    mcmc = assemble.mcmc.from.cache(cache.dir, allow.incomplete = T, chains = 1)
    dir.create(file.path(MCMC.RUNS.DIR, "mcmc_files"), recursive = TRUE, showWarnings = FALSE)
    mcmc.file = file.path(MCMC.RUNS.DIR, "mcmc_files",
                          paste0("mcmc_", mcmc.run.name(location, step, chain), "_", Sys.Date(), ".Rdata"))
    save(mcmc, file = mcmc.file)

    simset = extract.simset(mcmc,
                            additional.burn = SIMSET.ADDITIONAL.BURN,
                            additional.thin = SIMSET.ADDITIONAL.THIN)
    simset.file = file.path(MCMC.RUNS.DIR, paste0("simset_", mcmc.run.name(location, step, chain), "_", Sys.Date(), ".Rdata"))
    save(simset, file = simset.file)
    print(paste0("Saved ", mcmc.file, " and ", simset.file))

    # Step 1 hands off to Step 2: last simulation -> Step 2 starting values
    start.values.file = NA
    if (step == 1 && chain == min(settings$chains))
        start.values.file = save.step2.start.values(location, simset)

    error.files = list.files(file.path(MCMC.RUNS.DIR, "logs"),
                             pattern = paste0("^", gsub(".", "\\.", mcmc.run.name(location, step, chain), fixed = TRUE), "_errors_"),
                             full.names = TRUE)
    n.errors = sum(vapply(error.files, function(f) max(0, length(readLines(f, warn = FALSE)) - 1), numeric(1)))
    acceptance = tryCatch(round(get.total.acceptance.rate(mcmc), 3), error = function(e) NA)
    print(paste0("Done in ", format(round(difftime(Sys.time(), start.time, units = "hours"), 2)),
                 " | acceptance rate = ", acceptance,
                 " | proposals rejected because of errors = ", n.errors,
                 if (n.errors > 0) paste0(" (see ", paste0(error.files, collapse = ", "), ")") else ""))

    list(location = location, step = step, chain = chain, seed = mcmc.seed(step, chain),
         mcmc.file = mcmc.file, simset.file = simset.file, start.values.file = start.values.file,
         acceptance.rate = acceptance, n.errors = n.errors)
}


##-------------------------------##
##-- 2. MERGE THE STEP 2 CHAINS --##
##-------------------------------##

# Most recent mcmc file for each chain (from run.mcmc.step.chain); NA where a chain has none
find.latest.mcmc.files = function(location, step, chains){
    dir = file.path(MCMC.RUNS.DIR, "mcmc_files")
    files = list.files(dir)
    sapply(chains, function(chain){
        name = gsub(".", "\\.", mcmc.run.name(location, step, chain), fixed = TRUE)
        matches = files[grepl(paste0("^mcmc_", name, "_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.Rdata$"), files)]
        if (length(matches) == 0) NA else file.path(dir, sort(matches, decreasing = TRUE)[1])
    })
}

# Most recent simset file for each chain; NA where a chain has none
find.latest.simset.files = function(location, step, chains){
    files = list.files(MCMC.RUNS.DIR)
    sapply(chains, function(chain){
        name = gsub(".", "\\.", mcmc.run.name(location, step, chain), fixed = TRUE)
        matches = files[grepl(paste0("^simset_", name, "_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.Rdata$"), files)]
        if (length(matches) == 0) NA else file.path(MCMC.RUNS.DIR, sort(matches, decreasing = TRUE)[1])
    })
}

merge.step2.chains = function(location,
                              chains = mcmc.step.settings(2)$chains,
                              mcmc.files = find.latest.mcmc.files(location, step = 2, chains = chains)){
    if (any(is.na(mcmc.files)))
        stop(paste0("No Step 2 mcmc file found for ", location, " chain(s) ",
                    paste0(chains[is.na(mcmc.files)], collapse = ", "), " in ", file.path(MCMC.RUNS.DIR, "mcmc_files")))

    mcmcs = lapply(mcmc.files, function(file){
        print(paste0("loading ", file))
        e = new.env()
        get(load(file, envir = e)[1], envir = e)
    })
    mcmc = do.call(mcmc.merge.parallel, unname(mcmcs))

    chains.label = paste0("chains", paste0(chains, collapse = ""))
    dir.create(file.path(MCMC.RUNS.DIR, "mcmc_files", "merged"), recursive = TRUE, showWarnings = FALSE)
    merged.file = file.path(MCMC.RUNS.DIR, "mcmc_files", "merged",
                            paste0("mcmc_", convert_string(location), "_step2_", chains.label, "_", Sys.Date(), ".Rdata"))
    save(mcmc, file = merged.file)
    print(paste0("Saved merged mcmc: ", merged.file))

    # simset from the merged mcmc (if this fails, the merged mcmc and each chain's simset are still saved)
    simset.file = file.path(MCMC.RUNS.DIR, paste0("simset_", convert_string(location), "_step2_", chains.label, "_", Sys.Date(), ".Rdata"))
    simset.error = tryCatch({
        simset = extract.simset(mcmc,
                                additional.burn = SIMSET.ADDITIONAL.BURN,
                                additional.thin = SIMSET.ADDITIONAL.THIN)
        save(simset, file = simset.file)
        print(paste0("Saved merged simset: ", simset.file))
        NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(simset.error)) {
        print(paste0("Could not make a simset from the merged mcmc (", simset.error, "); the merged mcmc and each chain's own simset are saved"))
        simset.file = NA
    }

    invisible(list(mcmc.file = merged.file, simset.file = simset.file, simset.error = simset.error, chain.files = mcmc.files))
}


##-------------------------------##
##-- 3. THE WHOLE PIPELINE     --##
##-------------------------------##

# Runs one task (location/step/chain) in a worker R session, logging to its own file
run.mcmc.task = function(task){
    dir.create(file.path(MCMC.RUNS.DIR, "logs"), recursive = TRUE, showWarnings = FALSE)
    log.file = file.path(MCMC.RUNS.DIR, "logs", paste0(mcmc.run.name(task$location, task$step, task$chain), "_", Sys.Date(), ".log"))
    log.con = file(log.file, open = "at")
    sink(log.con, type = "output")
    sink(log.con, type = "message")
    on.exit({ sink(type = "message"); sink(type = "output"); close(log.con) })

    start.time = Sys.time()
    result = tryCatch(
        c(run.mcmc.step.chain(task$location, step = task$step, chain = task$chain, resume = task$resume),
          list(status = "done", message = "")),
        error = function(e){
            print(paste0("ERROR: ", conditionMessage(e)))
            c(task[c("location", "step", "chain")], list(status = "FAILED", message = conditionMessage(e)))
        })
    result$hours = round(as.numeric(difftime(Sys.time(), start.time, units = "hours")), 2)
    result$log.file = log.file
    result
}

run.mcmc.pipeline = function(locations,
                             steps = c(1, 2),
                             n.cores = max(1, parallel::detectCores() - 1),
                             resume = FALSE,
                             merge = TRUE){

    for (location in locations) get.location.prior(location)  # check every location is set up
    if (!all(steps %in% c(1, 2)))
        stop("steps must be 1, 2, or c(1, 2)")

    dir.create(file.path(MCMC.RUNS.DIR, "logs"), recursive = TRUE, showWarnings = FALSE)
    pipeline.log = file.path(MCMC.RUNS.DIR, "logs", paste0("pipeline_", paste0(convert_string(locations), collapse = "_"),
                                                           "_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".log"))
    say = function(...){
        line = paste0(format(Sys.time(), "%Y-%m-%d %H:%M"), "  ", paste0(...))
        cat(line, "\n")
        cat(line, "\n", file = pipeline.log, append = TRUE)
    }

    step2.chains = mcmc.step.settings(2)$chains
    max.tasks = max(if (1 %in% steps) length(locations) else 0,
                    if (2 %in% steps) length(locations) * length(step2.chains) else 0)
    n.workers = max(1, min(n.cores, max.tasks))

    say("MCMC pipeline: ", paste0(locations, collapse = ", "), " | steps ", paste0(steps, collapse = " & "),
        " | ", n.workers, " parallel R sessions | resume = ", resume)
    say("Per-chain logs: ", file.path(MCMC.RUNS.DIR, "logs"), "  (this summary: ", pipeline.log, ")")

    # Worker R sessions: same working directory, same code, same settings as this session
    say("Starting ", n.workers, " R sessions (each loads the model and compiles the C++ ODE, ~1-2 min)")
    cl = parallel::makePSOCKcluster(n.workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, setwd, getwd())
    parallel::clusterEvalQ(cl, source("calibration/mcmc_worker_setup.R"))
    parallel::clusterExport(cl, MCMC.SETTING.NAMES, envir = globalenv())

    all.results = list()
    run.tasks = function(tasks){
        results = parallel::parLapplyLB(cl, tasks, run.mcmc.task)
        for (r in results)
            say("  ", r$location, " step ", r$step, " chain ", r$chain, ": ", r$status, " (", r$hours, " h",
                if (r$status == "done") paste0(", acceptance ", r$acceptance.rate, ", ", r$n.errors, " proposals rejected for errors") else "",
                ")", if (r$status != "done") paste0(" - ", r$message, " - see ", r$log.file) else "")
        all.results <<- c(all.results, results)
        results
    }

    step2.locations = locations

    if (1 %in% steps) {
        say("Step 1: ", paste0(locations, collapse = ", "))
        results = run.tasks(lapply(locations, function(location)
            list(location = location, step = 1, chain = min(mcmc.step.settings(1)$chains), resume = resume)))
        step2.locations = locations[sapply(results, function(r) r$status == "done")]
        if (2 %in% steps && length(step2.locations) < length(locations))
            say("Skipping Step 2 for ", paste0(setdiff(locations, step2.locations), collapse = ", "), " (Step 1 failed)")
    }

    if (2 %in% steps && length(step2.locations) > 0) {
        say("Step 2: ", paste0(step2.locations, collapse = ", "), " (chains ", paste0(step2.chains, collapse = ", "), ")")
        tasks = unlist(lapply(step2.locations, function(location)
            lapply(step2.chains, function(chain) list(location = location, step = 2, chain = chain, resume = resume))),
            recursive = FALSE)
        results = run.tasks(tasks)

        if (merge) {
            for (location in step2.locations) {
                done = sapply(results, function(r) r$location == location && r$status == "done")
                if (sum(done) == length(step2.chains)) {
                    files = sapply(results[done], function(r) r$mcmc.file)
                    chains = sapply(results[done], function(r) r$chain)
                    merged = tryCatch(merge.step2.chains(location, chains = chains[order(chains)], mcmc.files = files[order(chains)]),
                                      error = function(e){ say("  ", location, ": merge FAILED - ", conditionMessage(e)); NULL })
                    if (!is.null(merged))
                        say("  ", location, ": merged ", length(chains), " chains -> ", merged$mcmc.file,
                            if (is.na(merged$simset.file)) paste0(" (merged simset FAILED: ", merged$simset.error, ")")
                            else paste0(" and ", merged$simset.file))
                } else
                    say("  ", location, ": not merging (only ", sum(done), " of ", length(step2.chains), " chains finished)")
            }
        }
    }

    say("Pipeline finished")
    invisible(all.results)
}
