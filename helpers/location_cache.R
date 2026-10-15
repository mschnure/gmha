################################################################################################
# Description: Caches model inputs that depend only on the location
#
# These functions read data files (e.g., IeDEA_data.xlsx) and fit regressions to fixed data, so
# they return the same thing every time for a given location. map.model.parameters calls them on
# every model run (i.e., every MCMC iteration); with the cache they run once per location per
# R session.
#
# - Sourced at the end of source_code.R (after the functions are defined)
# - Re-sourcing source_code.R starts a fresh cache (e.g., after editing a data file or one of
#   these functions)
# - options(gmha.use.location.cache = FALSE) turns the cache off
################################################################################################

LOCATION.CACHED.FUNCTIONS = c("get.testing.model",
                              "get.engagement.model",
                              "get.suppression.rates",
                              "get.all.age.sex.transmission.multipliers")

cache.by.location = function(function.name){
    original = get(function.name, envir = globalenv())
    if (isTRUE(attr(original, "gmha.location.cached")))
        original = attr(original, "gmha.original")   # already wrapped: re-wrap the original with a fresh cache
    
    cache = new.env()
    wrapped = function(location){
        if (!isTRUE(getOption("gmha.use.location.cache", TRUE)))
            return(original(location))
        key = as.character(location)
        if (!exists(key, envir = cache, inherits = FALSE))
            assign(key, original(location), envir = cache)
        get(key, envir = cache, inherits = FALSE)
    }
    attr(wrapped, "gmha.location.cached") = TRUE
    attr(wrapped, "gmha.original") = original
    assign(function.name, wrapped, envir = globalenv())
    invisible(wrapped)
}

invisible(lapply(LOCATION.CACHED.FUNCTIONS, cache.by.location))
