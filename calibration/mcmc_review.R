################################################################################################
# Description: Functions to review MCMC chains one by one (before or after merging)
#
# Functions
#     1. load.mcmc.chains: loads the most recent mcmc and simset file for each chain of a step
#     2. review.mcmc.chains: summary table per chain, likelihood traces, R-hats, parameter trace
#        plots, and simplots of each chain's simset against the data
#
# Needs calibration/mcmc_pipeline.R sourced first. Chains are read from mcmc_runs/, so run this on
# the machine that ran the MCMC (or copy the files over).
################################################################################################

load.saved.object = function(file){
    e = new.env()
    get(load(file, envir = e)[1], envir = e)
}

load.mcmc.chains = function(location,
                            step = 2,
                            chains = mcmc.step.settings(step)$chains){
    mcmc.files = find.latest.mcmc.files(location, step, chains)
    simset.files = find.latest.simset.files(location, step, chains)
    available = !is.na(mcmc.files)
    if (!any(available))
        stop(paste0("No step ", step, " mcmc files found for ", location, " in ", file.path(MCMC.RUNS.DIR, "mcmc_files")))
    if (any(!available))
        print(paste0("No files yet for chain(s) ", paste0(chains[!available], collapse = ", ")))
    chains = chains[available]

    mcmcs = lapply(mcmc.files[available], function(f){ print(paste0("loading ", f)); load.saved.object(f) })
    simsets = lapply(simset.files[available], function(f) if (is.na(f)) NULL else load.saved.object(f))
    names(mcmcs) = names(simsets) = paste0("chain ", chains)

    # one mcmc object holding all the chains (for plots that compare chains); chains of different
    # lengths (e.g., one still running) can't be merged, so then each chain is reviewed on its own
    merged = NULL
    if (length(mcmcs) > 1)
        merged = tryCatch(do.call(mcmc.merge.parallel, unname(mcmcs)),
                          error = function(e){ print(paste0("Chains can't be merged for review (", conditionMessage(e),
                                                            "); plotting them separately")); NULL })
    list(location = location, step = step, chains = chains,
         mcmcs = mcmcs, merged = merged, simsets = simsets,
         mcmc.files = mcmc.files[available], simset.files = simset.files[available])
}

review.mcmc.chains = function(location,
                              step = 2,
                              chains = mcmc.step.settings(step)$chains,
                              years = NULL,                 # default: 1980 to the last simulated year
                              n.trace.parameters = 12,      # trace plots for the parameters with the worst R-hats
                              trace.parameters = NULL,      # or name the parameters to plot
                              plot = TRUE){

    LOCATION <<- location
    loaded = load.mcmc.chains(location, step = step, chains = chains)
    chains = loaded$chains

    ## Summary table
    summary = do.call(rbind, lapply(seq_along(chains), function(i){
        mcmc = loaded$mcmcs[[i]]
        ll = mcmc@log.likelihoods[1, ]
        error.files = list.files(file.path(MCMC.RUNS.DIR, "logs"), full.names = TRUE,
                                 pattern = paste0("^", gsub(".", "\\.", mcmc.run.name(location, step, chains[i]), fixed = TRUE), "_errors_"))
        data.frame(chain = chains[i],
                   seed = mcmc.seed(step, chains[i]),
                   iterations.saved = mcmc@n.iter,
                   acceptance.rate = tryCatch(round(get.total.acceptance.rate(mcmc), 3), error = function(e) NA),
                   final.log.likelihood = round(ll[length(ll)], 1),
                   best.log.likelihood = round(max(ll, na.rm = TRUE), 1),
                   errored.proposals = sum(vapply(error.files, function(f) max(0, length(readLines(f, warn = FALSE)) - 1), numeric(1))),
                   file = basename(loaded$mcmc.files[i]))
    }))
    cat("\n==== ", location, ", step ", step, ": ", length(chains), " chain(s) ====\n", sep = "")
    print(summary, row.names = FALSE)

    ## R-hats (need 2+ chains of equal length)
    rhats = NULL
    if (!is.null(loaded$merged)) {
        rhats = tryCatch(get.rhats(loaded$merged), error = function(e) NULL)
        if (!is.null(rhats)) {
            cat("\nWorst R-hats (closer to 1 is better; > 1.1 suggests the chains haven't converged):\n")
            print(round(head(rhats, 15), 3))
        }
    }

    if (is.null(trace.parameters))
        trace.parameters = if (!is.null(rhats)) names(rhats)[1:min(n.trace.parameters, length(rhats))]
                           else head(loaded$mcmcs[[1]]@var.names, n.trace.parameters)

    ## Plots (each one is made separately, so one failing doesn't stop the rest)
    plot.errors = character()
    try.plot = function(name, fn){
        tryCatch(fn(), error = function(e){
            plot.errors[name] <<- conditionMessage(e)
            print(paste0("Could not make the '", name, "' plot: ", conditionMessage(e)))
            NULL
        })
    }
    plots = list()
    if (!is.null(loaded$merged)) {
        plots$likelihood = try.plot("likelihood", function() likelihood.plot(loaded$merged))
        plots$trace = try.plot("trace", function() trace.plot(loaded$merged, var.names = trace.parameters, exact.var.names = TRUE))
    }
    # one chain at a time if there is no merged object, or the merged plots failed
    if (is.null(plots$likelihood) || is.null(plots$trace)) {
        for (nm in names(loaded$mcmcs)) {
            plots[[paste0("likelihood.", nm)]] = try.plot(paste0("likelihood.", nm), function() likelihood.plot(loaded$mcmcs[[nm]]))
            plots[[paste0("trace.", nm)]] = try.plot(paste0("trace.", nm), function()
                trace.plot(loaded$mcmcs[[nm]], var.names = trace.parameters, exact.var.names = TRUE, include.rhat = FALSE))
        }
    }
    # the same simplots as calibration/analyze_parallel_mcmc.R, with every chain's simset on each plot
    simsets = loaded$simsets[!sapply(loaded$simsets, is.null)]
    simset.plot.names = character()
    if (length(simsets) > 0) {
        if (is.null(years)) {
            sim.years = simsets[[1]]@simulations[[1]]$years
            years = 1980:sim.years[length(sim.years)]
        }
        chain.simplot = function(...){ args = list(...); try.plot(paste0("simplot ", paste0(unlist(args), collapse = " ")),
                                                                  function() do.call(simplot, c(unname(simsets), list(years = years), args))) }
        simset.plots = list(
            fit = chain.simplot(),
            incidence.by.age = chain.simplot(facet.by = 'age', data.types = 'incidence'),
            prevalence.by.age = chain.simplot(facet.by = 'age', data.types = 'prevalence'),
            population.by.age = chain.simplot(data.types = 'population', facet.by = 'age'),
            cascade = chain.simplot(data.types = c('awareness', 'engagement', 'suppression'), proportion = T),
            cascade.by.age.sex = chain.simplot(data.types = c('awareness', 'engagement', 'suppression'),
                                               facet.by = c('age', 'sex'), proportion = T))
        plots = c(plots, simset.plots)
        simset.plot.names = names(simset.plots)
    }
    if (plot)
        for (nm in names(plots)) {
            p = plots[[nm]]
            if (is.null(p)) next
            if (inherits(p, "ggplot"))
                p = p + ggplot2::ggtitle(paste0(location, " step ", step, ": ", nm,
                                                if (nm %in% simset.plot.names)
                                                    paste0(" (simsets in order: ", paste0(names(simsets), collapse = ", "), ")") else ""))
            print(p)
        }

    plots = plots[!sapply(plots, is.null)]
    invisible(list(summary = summary, rhats = rhats, plots = plots, plot.errors = plot.errors, loaded = loaded))
}


# Re-runs a proposal that errored during an MCMC (a row of an <run>_errors_<date>.csv file in
# mcmc_runs/logs) with exactly the same parameters, so the error happens here and traceback()
# shows where. Returns the sim and log-likelihood if it doesn't error.
rerun.mcmc.error = function(error.file, location, row = 1, step = 2){
    errors = read.csv(error.file, check.names = FALSE, stringsAsFactors = FALSE)
    print(paste0(nrow(errors), " errored proposal(s) in ", error.file, "; re-running row ", row))
    print(paste0("  logged error (in ", errors$where[row], "): ", errors$message[row]))
    if ("call.stack" %in% names(errors))
        print(paste0("  call stack: ", errors$call.stack[row]))
    params = unlist(errors[row, setdiff(names(errors), c("time", "where", "message", "call.stack"))])

    LOCATION <<- location
    settings = mcmc.step.settings(step)
    details = set.likelihood.and.prior.by.location(location = location,
                                                   total.weight = settings$total.weight,
                                                   weighted.prevalence = settings$weighted.prevalence,
                                                   total.mortality.weight = settings$total.mortality.weight,
                                                   step = step)
    sim = SIMULATION.FUNCTION(params[details$prior@var.names])   # if it errors here, run traceback()
    log.likelihood = details$likelihood.to.run(sim)
    print(paste0("  ran without an error this time; log-likelihood = ", log.likelihood))
    invisible(list(parameters = params, sim = sim, log.likelihood = log.likelihood))
}

