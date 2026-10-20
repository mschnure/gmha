################################################################################################
# Description: Runs the two-step MCMC calibration
#
# Functions
#     1. run.mcmc.step.chain: runs one chain of one step for one location (create or resume the
#        cache, save the mcmc and simset; Step 1 also saves the Step 2 starting values)
#     2. merge.step2.chains: merges the Step 2 chains for a location (most recent file for each chain)
#     3. run.mcmc.pipeline: for one or more locations ("all" = all 15), runs every chain in parallel
#        R sessions in priority order - Step 1 for every location, then Step 2 chain 1 for every
#        location, then chain 2, ... (a location's Step 2 starts once its Step 1 is done); retries a
#        failed chain from its cache; merges each location as soon as its Step 2 chains are done
#     6. mcmc.progress: how far along every chain is (from the caches)
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
    build.output = character()
    out = textConnection("build.output", "w", local = TRUE)  # keeps what's printed even if it errors
    sink(out)
    ok = tryCatch({
        source('model/diffeq_cpp.R')
        TRUE
    }, finally = { sink(); close(out) }, error = function(e){
        # keep the compiler's own messages (R's error just says "Error 1 occurred building shared library")
        details = trimws(tail(build.output[nzchar(trimws(build.output))], 8))
        options(gmha.cpp.error = paste0(c(conditionMessage(e), details), collapse = " / "))
        warning(paste0("Could not compile the C++ model (", conditionMessage(e), "). Using the R version of the ODE."))
        FALSE
    })
    options(gmha.use.cpp = ok)
    invisible(ok)
}


# Simset from an mcmc, with burn and thin given in iterations (per chain) converted to the mcmc's own
# thinning, so it's the same set of iterations whether the mcmc was thinned by 1 or 20.
# Step 1: SIMSET.ADDITIONAL.BURN/THIN; Step 2 (each chain and merged): STEP2.SIMSET.BURN/THIN
simset.from.mcmc = function(mcmc, step = 1,
                            burn = if (step == 2) STEP2.SIMSET.BURN else SIMSET.ADDITIONAL.BURN,
                            thin = if (step == 2) STEP2.SIMSET.THIN else SIMSET.ADDITIONAL.THIN){
    extract.simset(mcmc,
                   additional.burn = ceiling(burn / mcmc@thin),
                   additional.thin = max(1, round(thin / mcmc@thin)))
}

# Light copies of mcmc files, for reviewing: the same mcmc without its simulations (the bulk of the file).
# The review's acceptance rates, likelihood and trace plots and R-hats only need the samples, likelihoods
# and acceptance counts, so loading a light copy takes seconds instead of minutes. Saved next to the full
# file in a light/ subfolder (the full file is still what simsets are made from).
light.mcmc.file = function(mcmc.file){
    file.path(dirname(mcmc.file), "light", basename(mcmc.file))
}

save.light.mcmc = function(mcmc, mcmc.file){
    file = light.mcmc.file(mcmc.file)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    mcmc@simulations = list()
    save(mcmc, file = file)
    invisible(file)
}

# Loads the light copy of an mcmc file; makes it first if it doesn't exist or is older than the full file
load.light.mcmc = function(mcmc.file){
    light = light.mcmc.file(mcmc.file)
    e = new.env()
    if (file.exists(light) && file.mtime(light) >= file.mtime(mcmc.file))
        return(get(load(light, envir = e)[1], envir = e))
    print(paste0("loading ", mcmc.file, " (first time: also saving a light copy for next time)"))
    mcmc = get(load(mcmc.file, envir = e)[1], envir = e)
    save.light.mcmc(mcmc, mcmc.file)
    mcmc@simulations = list()
    mcmc
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
                                                               thin = MCMC.THIN) # see mcmc_settings.R
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
    save.light.mcmc(mcmc, mcmc.file)

    simset = simset.from.mcmc(mcmc, step = step)
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
    save.light.mcmc(mcmc, merged.file)
    print(paste0("Saved merged mcmc: ", merged.file))

    # simset from the merged mcmc (if this fails, the merged mcmc and each chain's simset are still saved)
    simset.file = file.path(MCMC.RUNS.DIR, paste0("simset_", convert_string(location), "_step2_", chains.label, "_", Sys.Date(), ".Rdata"))
    simset.error = tryCatch({
        simset = simset.from.mcmc(mcmc, step = 2)
        save(simset, file = simset.file)
        print(paste0("Saved merged simset (", simset@n.sim, " distinct simulations, total weight ", sum(simset@weights), "): ", simset.file))
        NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(simset.error)) {
        print(paste0("Could not make a simset from the merged mcmc (", simset.error, "); the merged mcmc and each chain's own simset are saved"))
        simset.file = NA
    }

    invisible(list(mcmc.file = merged.file, simset.file = simset.file, simset.error = simset.error, chain.files = mcmc.files))
}


# Most recently saved merged Step 2 mcmc for a location (from merge.step2.chains); NA if none
find.latest.merged.mcmc.file = function(location){
    name = gsub(".", "\\.", convert_string(location), fixed = TRUE)
    files = list.files(file.path(MCMC.RUNS.DIR, "mcmc_files", "merged"), full.names = TRUE,
                       pattern = paste0("^mcmc_", name, "_step2_chains[0-9]+_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.Rdata$"))
    if (length(files) == 0) NA else files[which.max(file.mtime(files))]
}

# Makes a location's Step 2 simsets again - each chain's and the merged one - from the saved mcmc files,
# with the current STEP2.SIMSET.BURN/THIN (e.g., after changing them), without re-running or re-merging
# anything. Saved as new simset files with today's date (the review and merge use the newest).
remake.step2.simsets = function(location, chains = mcmc.step.settings(2)$chains){
    load.mcmc = function(file){ print(paste0("loading ", file)); e = new.env(); get(load(file, envir = e)[1], envir = e) }
    save.simset = function(simset, file){
        save(simset, file = file)
        print(paste0("  saved ", simset@n.sim, " distinct simulations (total weight ", sum(simset@weights), "): ", file))
        file
    }
    settings.label = paste0("burn ", STEP2.SIMSET.BURN, ", thin ", STEP2.SIMSET.THIN, " iterations per chain")
    print(paste0("==== ", location, ": remaking Step 2 simsets (", settings.label, ") ===="))

    files = character()
    mcmc.files = find.latest.mcmc.files(location, step = 2, chains = chains)
    for (i in seq_along(chains)) {
        if (is.na(mcmc.files[i])) { print(paste0("  no mcmc file for chain ", chains[i], " yet")); next }
        mcmc = load.mcmc(mcmc.files[i])
        save.light.mcmc(mcmc, mcmc.files[i])
        simset = simset.from.mcmc(mcmc, step = 2)
        files[paste0("chain ", chains[i])] = save.simset(simset, file.path(MCMC.RUNS.DIR,
            paste0("simset_", mcmc.run.name(location, 2, chains[i]), "_", Sys.Date(), ".Rdata")))
    }

    merged.file = find.latest.merged.mcmc.file(location)
    if (is.na(merged.file))
        print("  no merged mcmc yet")
    else {
        mcmc = load.mcmc(merged.file)
        save.light.mcmc(mcmc, merged.file)
        simset = simset.from.mcmc(mcmc, step = 2)
        chains.label = sub(paste0("^mcmc_", convert_string(location), "_step2_(chains[0-9]+)_.*$"), "\\1", basename(merged.file))
        files["merged"] = save.simset(simset, file.path(MCMC.RUNS.DIR,
            paste0("simset_", convert_string(location), "_step2_", chains.label, "_", Sys.Date(), ".Rdata")))
    }
    rm(list = intersect(c("simset", "mcmc"), ls())); gc()
    invisible(files)
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
                             merge = TRUE,
                             max.retries = 1){

    if (identical(locations, "all"))
        locations = names(LOCATION.PRIOR.NAMES)
    for (location in locations) get.location.prior(location)  # check every location is set up
    if (!all(steps %in% c(1, 2)))
        stop("steps must be 1, 2, or c(1, 2)")

    dir.create(file.path(MCMC.RUNS.DIR, "logs"), recursive = TRUE, showWarnings = FALSE)
    run.label = paste0(if (length(locations) > 3) paste0(length(locations), "_locations")
                       else paste0(convert_string(locations), collapse = "_"),
                       "_", format(Sys.time(), "%Y-%m-%d_%H%M"))
    pipeline.log = file.path(MCMC.RUNS.DIR, "logs", paste0("pipeline_", run.label, ".log"))
    status.file = file.path(MCMC.RUNS.DIR, "logs", paste0("pipeline_status_", run.label, ".csv"))
    say = function(...){
        line = paste0(format(Sys.time(), "%Y-%m-%d %H:%M"), "  ", paste0(...))
        cat(line, "\n")
        cat(line, "\n", file = pipeline.log, append = TRUE)
    }

    ##-- The tasks, in priority order: Step 1 for every location, then Step 2 chain 1 for every
    ##-- location, then chain 2 for every location, ... A Step 2 chain can start once its
    ##-- location's Step 1 is done (when Step 1 is part of this run).
    tasks = list()
    if (1 %in% steps)
        for (location in locations)
            tasks[[length(tasks) + 1]] = list(location = location, step = 1, chain = min(mcmc.step.settings(1)$chains),
                                              resume = resume, attempts = 0)
    step2.chains = mcmc.step.settings(2)$chains
    if (2 %in% steps)
        for (chain in step2.chains)
            for (location in locations)
                tasks[[length(tasks) + 1]] = list(location = location, step = 2, chain = chain,
                                                  resume = resume, attempts = 0)
    n.tasks = length(tasks)
    task.label = function(i) paste0(tasks[[i]]$location, " step ", tasks[[i]]$step, " chain ", tasks[[i]]$chain)
    status = rep("waiting", n.tasks)    # waiting / running / done / FAILED / skipped
    results = vector("list", n.tasks)
    started.at = rep(as.POSIXct(NA), n.tasks)
    step1.index = function(location) which(sapply(tasks, function(t) t$step == 1 && t$location == location))
    step2.indices = function(location) which(sapply(tasks, function(t) t$step == 2 && t$location == location))

    is.ready = function(i){
        if (tasks[[i]]$step == 1 || !(1 %in% steps)) return(TRUE)
        status[step1.index(tasks[[i]]$location)] == "done"
    }
    next.task = function(){
        for (i in seq_len(n.tasks))
            if (status[i] == "waiting" && is.ready(i)) return(i)
        NA
    }
    write.status = function(){
        df = do.call(rbind, lapply(seq_len(n.tasks), function(i){
            r = results[[i]]
            data.frame(location = tasks[[i]]$location, step = tasks[[i]]$step, chain = tasks[[i]]$chain,
                       status = status[i], attempts = tasks[[i]]$attempts,
                       started = if (is.na(started.at[i])) "" else format(started.at[i], "%Y-%m-%d %H:%M"),
                       hours = if (is.null(r$hours)) NA else r$hours,
                       acceptance = if (is.null(r$acceptance.rate)) NA else r$acceptance.rate,
                       errored.proposals = if (is.null(r$n.errors)) NA else r$n.errors,
                       message = if (is.null(r$message)) "" else r$message,
                       stringsAsFactors = FALSE)
        }))
        write.csv(df, status.file, row.names = FALSE)
    }

    n.workers = max(1, min(n.cores, n.tasks))
    say("MCMC pipeline: ", length(locations), " location(s) (", paste0(locations, collapse = ", "), ") | steps ",
        paste0(steps, collapse = " & "), " | ", n.tasks, " chains to run | ", n.workers, " parallel R sessions | resume = ", resume,
        " | retries per chain = ", max.retries)
    say("Status table (updated as chains finish): ", status.file)
    say("Per-chain logs: ", file.path(MCMC.RUNS.DIR, "logs"), " | progress any time: mcmc.progress()")
    write.status()

    # Compile the C++ ODE here first (or load it, if already compiled - see model/diffeq_cpp.R), so the
    # worker sessions only have to load it; compiling in all of them at once fails on Windows
    if (!isTRUE(suppressWarnings(use.cpp.model.if.possible()))) {
        say("STOPPING: the C++ ODE didn't compile in this session: ", getOption("gmha.cpp.error", ""))
        stop(paste0("The C++ ODE didn't compile (", getOption("gmha.cpp.error", ""), "). Compile it once in the ",
                    "RStudio console with source('model/diffeq_cpp.R') - it's saved in cached/cpp_build, so ",
                    "this session will load it - then start the pipeline again."))
    }
    say("C++ ODE ready (", normalizePath(GMHA.CPP.BUILD.DIR), ")")

    # Worker R sessions: same working directory, same code, same settings as this session
    say("Starting ", n.workers, " R sessions (each loads the model and the compiled C++ ODE, ~1-2 min)")
    cl = parallel::makePSOCKcluster(n.workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, setwd, getwd())
    parallel::clusterEvalQ(cl, source("calibration/mcmc_worker_setup.R"))
    parallel::clusterExport(cl, MCMC.SETTING.NAMES, envir = globalenv())

    # Every session must use the C++ ODE (~20x faster than R; same results). Any session that couldn't
    # load it retries on its own; if one still can't, stop rather than run on the slow R version
    cpp.ok = unlist(parallel::clusterEvalQ(cl, isTRUE(getOption("gmha.use.cpp"))))
    for (w in which(!cpp.ok)) {
        say("  R session ", w, ": C++ ODE didn't compile (",
            parallel::clusterEvalQ(cl[w], getOption("gmha.cpp.error", "no error message"))[[1]], "); retrying on its own")
        cpp.ok[w] = isTRUE(suppressWarnings(parallel::clusterEvalQ(cl[w], use.cpp.model.if.possible())[[1]]))
    }
    if (!all(cpp.ok)) {
        msgs = unique(unlist(parallel::clusterEvalQ(cl[!cpp.ok], getOption("gmha.cpp.error", "no error message"))))
        say("STOPPING: the C++ ODE didn't compile in ", sum(!cpp.ok), " of ", n.workers, " R sessions: ", paste0(msgs, collapse = " | "))
        stop(paste0("The C++ ODE didn't compile in ", sum(!cpp.ok), " R session(s): ", paste0(msgs, collapse = " | ")))
    }
    say("C++ ODE compiled in all ", n.workers, " R sessions")

    # parallel's own load-balancing works the same way (sendCall/recvOneResult); used directly here so
    # the next chain can be picked when a session frees up, based on what has finished so far
    worker.task = rep(NA_integer_, n.workers)
    submit = function(w, i){
        tasks[[i]]$attempts <<- tasks[[i]]$attempts + 1
        status[i] <<- "running"
        started.at[i] <<- Sys.time()
        worker.task[w] <<- i
        parallel:::sendCall(cl[[w]], run.mcmc.task, list(tasks[[i]]))
        say("  started ", task.label(i), if (tasks[[i]]$attempts > 1) paste0(" (attempt ", tasks[[i]]$attempts, ", resuming from cache)") else "")
    }
    fill.free.workers = function(){
        for (w in which(is.na(worker.task))) {
            i = next.task()
            if (is.na(i)) break
            submit(w, i)
        }
    }

    fill.free.workers()
    write.status()

    while (any(status == "running")) {
        received = parallel:::recvOneResult(cl)
        w = received$node
        i = worker.task[w]
        worker.task[w] = NA
        r = received$value
        if (inherits(r, "try-error") || !is.list(r) || is.null(r$status))
            r = list(status = "FAILED", message = paste0(as.character(r), collapse = " "), hours = NA)

        if (r$status == "done") {
            status[i] = "done"
            results[[i]] = r
            say("  finished ", task.label(i), " (", r$hours, " h, acceptance ", r$acceptance.rate, ", ",
                r$n.errors, " proposals rejected for errors)")
        } else if (tasks[[i]]$attempts <= max.retries) {
            status[i] = "waiting"
            tasks[[i]]$resume = TRUE
            results[[i]] = r
            say("  FAILED ", task.label(i), " - ", r$message, " - will retry, resuming from its cache")
        } else {
            status[i] = "FAILED"
            results[[i]] = r
            say("  FAILED ", task.label(i), " - ", r$message, " - see ", r$log.file)
            if (tasks[[i]]$step == 1 && 2 %in% steps) {
                skip = step2.indices(tasks[[i]]$location)
                status[skip] = "skipped"
                say("  skipping Step 2 for ", tasks[[i]]$location, " (Step 1 failed)")
            }
        }

        # start the next chains before merging, so no session waits on the merge
        fill.free.workers()
        write.status()

        # merge a location as soon as all of its Step 2 chains are done
        if (merge && tasks[[i]]$step == 2 && status[i] == "done") {
            location = tasks[[i]]$location
            idx = step2.indices(location)
            if (all(status[idx] == "done")) {
                chains = sapply(idx, function(j) tasks[[j]]$chain)
                files = sapply(idx, function(j) results[[j]]$mcmc.file)
                merged = tryCatch(merge.step2.chains(location, chains = chains[order(chains)], mcmc.files = files[order(chains)]),
                                  error = function(e){ say("  ", location, ": merge FAILED - ", conditionMessage(e)); NULL })
                if (!is.null(merged))
                    say("  merged ", location, " (", length(chains), " chains) -> ", merged$mcmc.file,
                        if (is.na(merged$simset.file)) paste0(" (merged simset FAILED: ", merged$simset.error, ")")
                        else paste0(" and ", merged$simset.file))
            }
        }
    }

    n.done = sum(status == "done")
    say("Pipeline finished: ", n.done, " of ", n.tasks, " chains done",
        if (any(status %in% c("FAILED", "skipped"))) paste0(" | not done: ", paste0(sapply(which(status %in% c("FAILED", "skipped")),
                                                                                          function(i) paste0(task.label(i), " (", status[i], ")")), collapse = "; ")) else "")
    write.status()
    invisible(lapply(seq_len(n.tasks), function(i) c(tasks[[i]][c("location", "step", "chain")], list(status = status[i]), results[[i]])))
}


##-------------------------------##
##-- 4. PROGRESS               --##
##-------------------------------##

# How far along every chain is, read from the caches (works while the pipeline is running, from any
# R session on the MCMC machine). Prints one row per location: s1 = Step 1, s2c1..s2c4 = Step 2
# chains; "-" = not started. Returns the details (incl. when each cache was last written) invisibly.
mcmc.progress = function(locations = names(LOCATION.PRIOR.NAMES),
                         steps = c(1, 2)){
    rows = list()
    for (location in locations)
        for (step in steps)
            for (chain in mcmc.step.settings(step)$chains) {
                dir = mcmc.cache.dir(location, step, chain)
                pct = NA
                last.saved = NA
                if (dir.exists(dir)) {
                    pct = tryCatch({
                        s = bayesian.simulations:::get.cache.sampling.status(dir)
                        round(100 * sum(s$chunks.done) / sum(s$total.chunks))
                    }, error = function(e) NA)
                    files = list.files(dir, recursive = TRUE, full.names = TRUE)
                    if (length(files) > 0) last.saved = max(file.mtime(files))
                }
                rows[[length(rows) + 1]] = data.frame(location = location, step = step, chain = chain,
                                                      column = if (step == 1) "s1" else paste0("s", step, "c", chain),
                                                      percent.done = pct,
                                                      last.saved = if (is.na(last.saved)) "" else format(last.saved, "%m-%d %H:%M"),
                                                      stringsAsFactors = FALSE)
            }
    df = do.call(rbind, rows)
    columns = unique(df$column)
    wide = matrix("-", nrow = length(locations), ncol = length(columns), dimnames = list(locations, columns))
    for (k in seq_len(nrow(df)))
        if (!is.na(df$percent.done[k]))
            wide[df$location[k], df$column[k]] = paste0(df$percent.done[k], "%")
    cat("MCMC progress (", format(Sys.time(), "%Y-%m-%d %H:%M"), "): s1 = Step 1, s2c1-s2c4 = Step 2 chains, - = not started\n", sep = "")
    print(noquote(wide))
    invisible(df)
}
