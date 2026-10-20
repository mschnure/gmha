################################################################################################
# Description: Line-by-line profile of map.model.parameters (with the location cache on)
#
# Run from the project root in a FRESH R session (so source line numbers are kept):
#   source('model/profile_parameters.R')
# then paste the report printed at the end back to Claude.
################################################################################################

options(keep.source = TRUE)   # needed for line-level profiling; must be set before sourcing
source('model/run_systematic.R')

PROFILE.LOCATION = "South Africa"
N.REPS = 40

LOCATION = PROFILE.LOCATION
details = set.likelihood.and.prior.by.location(location = PROFILE.LOCATION)
sampled.parameters = get.default.parameters(location = PROFILE.LOCATION)
sampled.parameters[names(details$params.start.values)] = details$params.start.values
parameters.base = create.model.parameters(location = PROFILE.LOCATION)

run.once = function() map.model.parameters(parameters.base, location = PROFILE.LOCATION,
                                           sampled.parameters = sampled.parameters)
invisible(run.once())   # fills the location cache

report = character()
add = function(...) { line = paste0(...); report <<- c(report, line); cat(line, "\n") }

prof.file = tempfile(fileext = ".out")
Rprof(prof.file, interval = 0.002, line.profiling = TRUE)
for (i in 1:N.REPS) run.once()
Rprof(NULL)

total = summaryRprof(prof.file)$sampling.time
add("==== map.model.parameters profile: ", PROFILE.LOCATION, " (", format(Sys.time(), "%Y-%m-%d %H:%M"), ") ====")
add("median per call: ", round(1000 * total / N.REPS, 1), " ms")

show = function(tab, column, n, title){
    add("")
    add(title)
    tab = head(tab[order(-tab[[column]]), , drop = FALSE], n)
    for (i in seq_len(nrow(tab)))
        add("   ", formatC(100 * tab[[column]][i] / total, format = "f", digits = 1, width = 5), "%  ", rownames(tab)[i])
}

by.fn = summaryRprof(prof.file)
show(by.fn$by.total, "total.time", 35, "A. Functions, time including what they call (top 35)")
show(by.fn$by.self, "self.time", 15, "B. Functions, own time (top 15)")

by.line = summaryRprof(prof.file, lines = "show")
show(by.line$by.total, "total.time", 40, "C. Source lines (file#line), time including what they call (top 40)")

PARAMETER.PROFILE.REPORT = report
