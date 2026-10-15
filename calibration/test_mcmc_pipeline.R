################################################################################################
# Description: Quick checks of the MCMC pipeline before using it for real runs
#
#   A. Starting values (instant): Step 1 and Step 2 starting values for every location; Step 2
#      values match the files that set_likelihood_and_prior.R used to load by hand
#   B. Tiny end-to-end run (a few minutes): the error wrappers, then the whole pipeline for one
#      location with 40 iterations per chain, then the chain review, in scratch folders
#      (mcmc_cache/_pipeline_test, mcmc_runs/_pipeline_test), which are deleted at the end.
#      Nothing in calibration/starting_values is written.
#
# Run from the project root:
#   source('calibration/test_mcmc_pipeline.R')
# then paste the report printed at the end back to Claude.
################################################################################################

source("model/run_systematic.R")
source("calibration/mcmc_pipeline.R")
source("calibration/mcmc_review.R")

RUN.PART.B = TRUE
PART.B.LOCATION = "Nigeria"   # also checks the Step 1 starting values file
KEEP.TEST.OUTPUT = FALSE

report = character()
add = function(...) { line = paste0(...); report <<- c(report, line); cat(line, "\n") }
check = function(ok, what) { add(if (isTRUE(ok)) "   OK     " else "   PROBLEM ", what); isTRUE(ok) }
all.ok = TRUE

add("==== MCMC pipeline check (", format(Sys.time(), "%Y-%m-%d %H:%M"), ") ====")

##---------------------##
##-- A. START VALUES --##
##---------------------##

# what set_likelihood_and_prior.R loaded by hand before (Step 2 starting values)
OLD.STEP2.FILES = c("Kenya" = "2026-08-21_kenya_start_values.Rdata",
                    "South Africa" = "2026-08-21_south_africa_start_values.Rdata",
                    "Mozambique" = "2026-08-21_mozambique_start_values.Rdata",
                    "Tanzania" = "2026-08-21_tanzania_start_values.Rdata",
                    "Uganda" = "2026-08-21_uganda_start_values.Rdata",
                    "Zambia" = "2026-08-24_zambia_start_values.Rdata",
                    "Zimbabwe" = "2026-08-21_zimbabwe_start_values.Rdata",
                    "Malawi" = "2026-08-21_malawi_start_values.Rdata",
                    "Nigeria" = "2026-08-21_nigeria_start_values.Rdata",
                    "unaids.remainder" = "2026-08-21_unaids.remainder_start_values.Rdata",
                    "non.unaids.remainder" = "2026-08-21_non.unaids.remainder_start_values.Rdata",
                    "r1.low" = "2026-08-21_r1_low_start_values.Rdata",
                    "r1.lower.middle" = "2026-08-21_r1_lower.middle_start_values.Rdata",
                    "r1.upper.middle" = "2026-08-21_r1_upper.middle_start_values.Rdata",
                    "r1.high" = "2026-08-21_r1_high_start_values.Rdata")

add("")
add("A. Starting values")
for (location in names(LOCATION.PRIOR.NAMES)) {
    prior = get.location.prior(location)
    s1 = suppressWarnings(capture.output(v1 <- get.mcmc.start.values(location, step = 1, prior = prior)))
    s2 = suppressWarnings(capture.output(v2 <- get.mcmc.start.values(location, step = 2, prior = prior)))
    old = load.start.values.file(file.path(STARTING.VALUES.DIR, OLD.STEP2.FILES[[location]]))[prior@var.names]
    step1.source = if (location %in% names(STEP1.START.VALUE.FILES)) basename(STEP1.START.VALUE.FILES[[location]]) else "prior medians"
    all.ok = check(!any(is.na(v1)) && !any(is.na(v2)) && identical(names(v1), prior@var.names) &&
                       identical(unname(v2), unname(old)),
                   paste0(formatC(location, width = -21), "step 1: ", step1.source,
                          " | step 2: ", basename(find.step2.start.values.file(location)),
                          if (identical(unname(v2), unname(old))) " (same as before)" else " (DIFFERS from before)")) && all.ok
}

##---------------------##
##-- B. TINY RUN     --##
##---------------------##

if (RUN.PART.B) {
    add("")
    add("B. Tiny end-to-end run: ", PART.B.LOCATION, " (Step 1 -> Step 2 x", length(MCMC.STEP.SETTINGS$step2$chains), " -> merge)")

    # scratch settings (copied to the worker R sessions by run.mcmc.pipeline)
    real.settings = mget(MCMC.SETTING.NAMES, envir = globalenv())
    MCMC.STEP.SETTINGS$step1$n.iter = 40
    MCMC.STEP.SETTINGS$step2$n.iter = 40
    MCMC.CACHE.FREQUENCY = 20
    MCMC.UPDATE.FREQUENCY = 10
    SIMSET.ADDITIONAL.BURN = 0
    SIMSET.ADDITIONAL.THIN = 1
    MCMC.CACHE.DIR = "mcmc_cache/_pipeline_test"
    MCMC.RUNS.DIR = "mcmc_runs/_pipeline_test"
    STARTING.VALUES.DIR = "mcmc_runs/_pipeline_test/starting_values"

    # error wrappers: a simulation that errors should be rejected (likelihood -Inf) and logged, not crash
    test.error.file = file.path(MCMC.RUNS.DIR, "logs", "wrapper_test_errors.csv")
    safe = make.safe.mcmc.functions(simulation.function = function(p) stop("test error"),
                                    log.likelihood = function(sim) 0,
                                    log.prior = function(p) NaN,
                                    error.log.file = test.error.file)
    test.params = c(a = 1, b = 2)
    invisible(capture.output({ sim = safe$simulation(test.params); ll = safe$likelihood(sim); lp = safe$prior(test.params) }))
    logged = if (file.exists(test.error.file)) read.csv(test.error.file) else NULL
    all.ok = check(is.null(sim) && identical(ll, -Inf) && identical(lp, -Inf) && !is.null(logged) && nrow(logged) == 2 &&
                       all(c("a", "b") %in% names(logged)),
                   "errors/NaN become rejected proposals and are logged with their parameters") && all.ok

    t0 = Sys.time()
    results = tryCatch(run.mcmc.pipeline(locations = PART.B.LOCATION, steps = c(1, 2),
                                         n.cores = length(MCMC.STEP.SETTINGS$step2$chains), resume = FALSE),
                       error = function(e){ add("   PROBLEM pipeline stopped: ", conditionMessage(e)); NULL })
    add("   (took ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")

    if (!is.null(results)) {
        statuses = sapply(results, function(r) paste0("step ", r$step, " chain ", r$chain, " = ", r$status))
        all.ok = check(all(sapply(results, function(r) r$status == "done")), paste0("all runs finished: ", paste0(statuses, collapse = "; "))) && all.ok
        failed = Filter(function(r) r$status != "done", results)
        for (r in failed) add("      step ", r$step, " chain ", r$chain, ": ", r$message, " (log: ", r$log.file, ")")

        step1 = Filter(function(r) r$step == 1, results)[[1]]
        if (step1$status == "done") {
            e = new.env(); load(step1$simset.file, envir = e)
            last.sim = e$simset@parameters[e$simset@n.sim, ]
            saved = load.start.values.file(step1$start.values.file)
            all.ok = check(identical(saved, last.sim), paste0("Step 2 starting values = last Step 1 simulation (", basename(step1$start.values.file), ")")) && all.ok
            all.ok = check(identical(find.step2.start.values.file(PART.B.LOCATION), step1$start.values.file),
                           "Step 2 picks up the file Step 1 just saved") && all.ok
        }

        step2 = Filter(function(r) r$step == 2 && r$status == "done", results)
        if (length(step2) >= 2) {
            last.params = lapply(step2, function(r){ e = new.env(); load(r$simset.file, envir = e); e$simset@parameters[e$simset@n.sim, ] })
            all.ok = check(!identical(last.params[[1]], last.params[[2]]), "Step 2 chains differ from each other (different seeds)") && all.ok
        }
        merged = list.files(file.path(MCMC.RUNS.DIR, "mcmc_files", "merged"))
        all.ok = check(length(merged) == 1, paste0("merged file written: ", paste0(merged, collapse = ", "))) && all.ok
        merged.simsets = list.files(MCMC.RUNS.DIR, pattern = "_step2_chains")
        all.ok = check(length(merged.simsets) == 1, paste0("merged simset written: ", paste0(merged.simsets, collapse = ", "))) && all.ok

        # diagnostics: which bayesian.simulations functions work on the merged (4-chain) mcmc, and where any error comes from
        if (length(merged) == 1) {
            add("   diagnostics on the merged mcmc:")
            m = load.saved.object(file.path(MCMC.RUNS.DIR, "mcmc_files", "merged", merged))
            add("      n.chains = ", m@n.chains, " | n.iter = ", paste0(m@n.iter, collapse = ","),
                " | packageVersion(bayesian.simulations) = ", as.character(packageVersion("bayesian.simulations")))
            diagnose = function(label, fn){
                stack = ""
                msg = tryCatch(withCallingHandlers({ fn(); "works" },
                                                   error = function(e){ stack <<- summarize.call.stack(sys.calls()) }),
                               error = function(e) paste0("ERROR: ", conditionMessage(e)))
                add("      ", formatC(label, width = -16), msg, if (nzchar(stack)) paste0("  | calls: ", stack) else "")
            }
            diagnose("extract.simset", function() extract.simset(m, additional.burn = 0, additional.thin = 1))
            diagnose("get.rhats", function() get.rhats(m))
            diagnose("likelihood.plot", function() print(likelihood.plot(m)))
            diagnose("trace.plot", function() print(trace.plot(m, var.names = m@var.names[1:3], exact.var.names = TRUE)))
            diagnose("trace.plot no rhat", function() print(trace.plot(m, var.names = m@var.names[1:3], exact.var.names = TRUE, include.rhat = FALSE)))
            diagnose("acceptance rate", function() get.total.acceptance.rate(m))
        }
        logs = list.files(file.path(MCMC.RUNS.DIR, "logs"))
        add("   logs written: ", paste0(logs, collapse = ", "))

        review = tryCatch(suppressWarnings(review.mcmc.chains(PART.B.LOCATION, step = 2, plot = FALSE)),
                          error = function(e){ add("   PROBLEM review failed: ", conditionMessage(e)); NULL })
        if (!is.null(review)) {
            all.ok = check(nrow(review$summary) == length(MCMC.STEP.SETTINGS$step2$chains) && length(review$plots) > 0,
                           paste0("chain review: ", nrow(review$summary), " chains summarized, plots made: ",
                                  paste0(names(review$plots), collapse = ", "))) && all.ok
            add("   seeds used: ", paste0(review$summary$seed, collapse = ", "))
            if (length(review$plot.errors) > 0)
                add("   plots that failed (others were made): ",
                    paste0(names(review$plot.errors), " (", review$plot.errors, ")", collapse = "; "))
        }
    } else all.ok = FALSE

    if (!KEEP.TEST.OUTPUT) {
        unlink(MCMC.CACHE.DIR, recursive = TRUE)
        unlink(MCMC.RUNS.DIR, recursive = TRUE)
        add("   (scratch folders deleted)")
    }
    list2env(real.settings, envir = globalenv())  # back to the real settings
}

add("")
add("==== Summary: ", if (all.ok) "everything OK" else "SOME CHECKS FAILED - see above", " ====")
PIPELINE.TEST.REPORT = report
