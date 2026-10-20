################################################################################################
# Description: Functions to review MCMC chains one by one (before or after merging)
#
# Functions
#     1. load.mcmc.chains: loads the most recent mcmc and simset file for each chain of a step
#     2. review.mcmc.chains: summary table per chain, likelihood traces, R-hats, parameter trace
#        plots, and simplots of each chain's simset against the data
#     3. review.merged.simset: the same simplots for a location's merged Step 2 simset
#     4. review.to.pdf: all of the above for many locations at once, one PDF per location, plus an
#        overview table across locations (see calibration/review_all_locations.R)
#
# Simsets are thinned to ~max.plot.sims simulations for plotting only (weights kept), since simplot
# with the full ~20,000 is slow; the saved simsets aren't changed.
#
# Needs calibration/mcmc_pipeline.R sourced first. Chains are read from mcmc_runs/, so run this on
# the machine that ran the MCMC (or copy the files over).
################################################################################################

load.saved.object = function(file){
    e = new.env()
    get(load(file, envir = e)[1], envir = e)
}

# A simset thinned to about max.sims simulations, keeping the weighting (for plotting only)
thin.for.plotting = function(simset, max.sims = 1000){
    total = sum(simset@weights)
    if (is.null(max.sims) || total <= max.sims || any(round(simset@weights) != simset@weights))
        return(simset)
    thin.simset(simset, thin = ceiling(total / max.sims))
}

# The simplots from calibration/analyze_parallel_mcmc.R, with all the simsets given on each plot.
# try.plot(name, fn) runs each one, so one failing doesn't stop the rest
make.simset.plots = function(simsets, years, try.plot){
    p = function(name, ...){ args = list(...); try.plot(name, function() do.call(simplot, c(unname(simsets), list(years = years), args))) }
    list(fit = p("fit"),
         incidence.by.age = p("incidence.by.age", facet.by = 'age', data.types = 'incidence'),
         prevalence.by.age = p("prevalence.by.age", facet.by = 'age', data.types = 'prevalence'),
         population.by.age = p("population.by.age", data.types = 'population', facet.by = 'age'),
         cascade = p("cascade", data.types = c('awareness', 'engagement', 'suppression'), proportion = T),
         cascade.by.age.sex = p("cascade.by.age.sex", data.types = c('awareness', 'engagement', 'suppression'),
                                facet.by = c('age', 'sex'), proportion = T))
}

default.plot.years = function(simset){
    sim.years = simset@simulations[[1]]$years
    1980:sim.years[length(sim.years)]
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
                              max.plot.sims = 1000,         # each chain's simset is thinned to about this many for the simplots
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
        if (is.null(years))
            years = default.plot.years(simsets[[1]])
        simset.plots = make.simset.plots(lapply(simsets, thin.for.plotting, max.sims = max.plot.sims), years, try.plot)
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


# Most recently saved merged Step 2 simset for a location (from merge.step2.chains); NA if none
find.latest.merged.simset = function(location){
    name = gsub(".", "\\.", convert_string(location), fixed = TRUE)
    files = list.files(MCMC.RUNS.DIR, full.names = TRUE,
                       pattern = paste0("^simset_", name, "_step2_chains[0-9]+_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.Rdata$"))
    if (length(files) == 0) NA else files[which.max(file.mtime(files))]
}

# The simplots from calibration/analyze_parallel_mcmc.R for a location's merged Step 2 simset
review.merged.simset = function(location,
                                simset.file = find.latest.merged.simset(location),
                                years = NULL,
                                max.plot.sims = 1000,   # thinned to about this many for plotting
                                plot = TRUE){
    if (is.na(simset.file))
        stop(paste0("No merged Step 2 simset found for ", location, " in ", MCMC.RUNS.DIR))
    LOCATION <<- location
    print(paste0("loading ", simset.file))
    simset = load.saved.object(simset.file)
    info = data.frame(location = location,
                      simulations = simset@n.sim,             # distinct simulations
                      total.weight = sum(simset@weights),     # = saved iterations kept (repeats are weighted)
                      file = basename(simset.file))
    print(info, row.names = FALSE)

    if (is.null(years))
        years = default.plot.years(simset)
    plot.errors = character()
    try.plot = function(name, fn){
        tryCatch(fn(), error = function(e){
            plot.errors[name] <<- conditionMessage(e)
            print(paste0("Could not make the '", name, "' plot: ", conditionMessage(e)))
            NULL
        })
    }
    plots = make.simset.plots(list(thin.for.plotting(simset, max.plot.sims)), years, try.plot)
    plots = plots[!sapply(plots, is.null)]
    if (plot)
        for (nm in names(plots)) {
            p = plots[[nm]]
            if (inherits(p, "ggplot"))
                p = p + ggplot2::ggtitle(paste0(location, " merged Step 2 simset: ", nm))
            print(p)
        }
    invisible(list(info = info, plots = plots, plot.errors = plot.errors, simset.file = simset.file))
}

# Everything above for many locations: one PDF per location (summary table and R-hats, likelihood and
# trace plots, each chain's simplots, then the merged simset's simplots) in `dir`, plus overview.csv
# with one row per location. A location that fails is noted in the overview and the rest continue.
review.to.pdf = function(locations = "all",
                         step = 2,
                         dir = file.path("results", paste0("mcmc_review_", Sys.Date())),
                         max.plot.sims = 1000){
    if (identical(locations, "all"))
        locations = names(LOCATION.PRIOR.NAMES)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    text.page = function(lines){
        grid::grid.newpage()
        grid::grid.text(paste0(lines, collapse = "\n"), x = 0.02, y = 0.98, just = c("left", "top"),
                        gp = grid::gpar(fontfamily = "mono", fontsize = 8))
    }
    print.titled = function(p, title){
        if (inherits(p, "ggplot"))
            p = p + ggplot2::ggtitle(title)
        print(p)
    }

    overview = list()
    for (location in locations) {
        print(paste0("==== ", location, " ===="))
        pdf.file = file.path(dir, paste0(convert_string(location), "_step", step, "_review.pdf"))
        pdf.open = FALSE
        overview[[location]] = tryCatch({
            chains = review.mcmc.chains(location, step = step, plot = FALSE, max.plot.sims = max.plot.sims)
            merged = NULL
            if (step == 2)
                merged = tryCatch(review.merged.simset(location, plot = FALSE, max.plot.sims = max.plot.sims),
                                  error = function(e){ print(paste0("No merged simset plots: ", conditionMessage(e))); NULL })

            grDevices::pdf(pdf.file, width = 14, height = 9)
            pdf.open = TRUE
            text.page(c(paste0(location, ", step ", step, " (", Sys.Date(), ")"), "",
                        capture.output(print(chains$summary, row.names = FALSE)), "",
                        if (!is.null(chains$rhats)) c("Worst R-hats (> 1.1 suggests the chains haven't converged):",
                                                      capture.output(print(round(head(chains$rhats, 25), 3)))),
                        if (!is.null(merged)) c("", "Merged simset:", capture.output(print(merged$info, row.names = FALSE))),
                        if (length(chains$plot.errors) > 0) c("", "Plots that failed:", paste0("  ", names(chains$plot.errors), ": ", chains$plot.errors))))
            for (nm in names(chains$plots))
                print.titled(chains$plots[[nm]], paste0(location, " step ", step, ": ", nm,
                                                        if (nm %in% c("likelihood", "trace") || grepl("^(likelihood|trace)\\.", nm)) "" else " (one color per chain)"))
            if (!is.null(merged))
                for (nm in names(merged$plots))
                    print.titled(merged$plots[[nm]], paste0(location, ": ", nm, " (merged Step 2 simset)"))
            grDevices::dev.off()
            pdf.open = FALSE

            data.frame(location = location,
                       chains = nrow(chains$summary),
                       min.acceptance = min(chains$summary$acceptance.rate, na.rm = TRUE),
                       max.acceptance = max(chains$summary$acceptance.rate, na.rm = TRUE),
                       worst.rhat = if (is.null(chains$rhats)) NA else round(max(chains$rhats, na.rm = TRUE), 3),
                       errored.proposals = sum(chains$summary$errored.proposals),
                       merged.simulations = if (is.null(merged)) NA else merged$info$simulations,
                       merged.total.weight = if (is.null(merged)) NA else merged$info$total.weight,
                       pdf = pdf.file, problem = "")
        }, error = function(e){
            if (pdf.open) grDevices::dev.off()
            print(paste0("Could not review ", location, ": ", conditionMessage(e)))
            data.frame(location = location, chains = NA, min.acceptance = NA, max.acceptance = NA, worst.rhat = NA,
                       errored.proposals = NA, merged.simulations = NA, merged.total.weight = NA,
                       pdf = NA, problem = conditionMessage(e))
        })
        rm(list = intersect(c("chains", "merged"), ls())); gc()
    }
    overview = do.call(rbind, overview)
    write.csv(overview, file.path(dir, "overview.csv"), row.names = FALSE)
    print(overview, row.names = FALSE)
    print(paste0("PDFs and overview.csv in ", normalizePath(dir)))
    invisible(overview)
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

