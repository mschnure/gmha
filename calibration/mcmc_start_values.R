################################################################################################
# Description: Priors and MCMC starting values by location and step
#
# Functions
#     1. get.location.prior
#     2. get.mcmc.start.values: Step 1 -> prior medians (or STEP1.START.VALUE.FILES);
#                               Step 2 -> most recent Step 2 starting values file for the location
#     3. save.step2.start.values: saves the last simulation of a Step 1 simset as Step 2 starting values
#     4. save.step2.start.values.from.file: same, from a saved Step 1 simset or mcmc file
#     5. find.step2.start.values.file
#
# Step 2 starting values files: calibration/starting_values/<date>_<location>_step2_start_values.Rdata
# (older files named <date>_<location>_start_values.Rdata are also found, so existing values keep working)
################################################################################################

LOCATION.PRIOR.NAMES = c("Kenya" = "KENYA.PRIOR",
                         "South Africa" = "SOUTH.AFRICA.PRIOR",
                         "Mozambique" = "MOZAMBIQUE.PRIOR",
                         "Tanzania" = "TANZANIA.PRIOR",
                         "Uganda" = "UGANDA.PRIOR",
                         "Zambia" = "ZAMBIA.PRIOR",
                         "Zimbabwe" = "ZIMBABWE.PRIOR",
                         "Malawi" = "MALAWI.PRIOR",
                         "Nigeria" = "NIGERIA.PRIOR",
                         "unaids.remainder" = "UNAIDS.REMAINDER.PRIOR",
                         "non.unaids.remainder" = "NON.UNAIDS.REMAINDER.PRIOR",
                         "r1.low" = "R1.LOW.PRIOR",
                         "r1.lower.middle" = "R1.LOWER.MIDDLE.PRIOR",
                         "r1.upper.middle" = "R1.UPPER.MIDDLE.PRIOR",
                         "r1.high" = "R1.HIGH.PRIOR")

get.location.prior = function(location){
    if (!(location %in% names(LOCATION.PRIOR.NAMES)))
        stop(paste0("No prior set up for '", location, "'. Locations set up: ",
                    paste0(names(LOCATION.PRIOR.NAMES), collapse = ", ")))
    get(LOCATION.PRIOR.NAMES[[location]])
}

# location as it appears in starting values file names (e.g., "south_africa", "r1_lower.middle")
start.values.file.stem = function(location){
    gsub("r1.", "r1_", convert_string(location), fixed = TRUE)
}

step2.start.values.file = function(location, date = Sys.Date()){
    file.path(STARTING.VALUES.DIR,
              paste0(format(as.Date(date)), "_", start.values.file.stem(location), "_step2_start_values.Rdata"))
}

# Most recent Step 2 starting values file for a location (NULL if there is none)
find.step2.start.values.file = function(location){
    stem = gsub(".", "\\.", start.values.file.stem(location), fixed = TRUE)
    files = list.files(STARTING.VALUES.DIR)
    files = files[grepl(paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}_", stem, "(_step2)?_start_values\\.Rdata$"), files)]
    if (length(files) == 0)
        return(NULL)
    dates = as.Date(substr(files, 1, 10))
    is.new.naming = grepl("_step2_start_values", files, fixed = TRUE)
    files = files[order(dates, is.new.naming, decreasing = TRUE)]  # latest date; new naming wins a tie
    file.path(STARTING.VALUES.DIR, files[1])
}

load.start.values.file = function(file){
    e = new.env()
    load(file, envir = e)
    if (!exists("params.start.values", envir = e, inherits = FALSE))
        stop(paste0("'", file, "' does not contain params.start.values"))
    get("params.start.values", envir = e)
}

get.mcmc.start.values = function(location,
                                 step,
                                 prior = get.location.prior(location)){
    if (step == 1) {
        file = unname(STEP1.START.VALUE.FILES[location])
        if (!is.na(file)) {
            print(paste0("Step 1 for ", location, ": starting from ", file))
            values = load.start.values.file(file)
        } else {
            print(paste0("Step 1 for ", location, ": starting from the prior medians"))
            values = get.medians(prior)
        }
    } else if (step == 2) {
        file = find.step2.start.values.file(location)
        if (is.null(file))
            stop(paste0("No Step 2 starting values found for ", location, " in ", STARTING.VALUES.DIR,
                        ". Run Step 1 first, or create them from a Step 1 simset/mcmc file with ",
                        "save.step2.start.values.from.file()"))
        print(paste0("Step 2 for ", location, ": starting from ", file))
        values = load.start.values.file(file)
    } else
        stop("step must be 1 or 2")

    # any prior parameters the file doesn't have (e.g., added since) start at the prior median
    missing.params = setdiff(prior@var.names, names(values))
    if (length(missing.params) > 0) {
        warning(paste0("Starting values for ", location, " (step ", step, ") are missing ", length(missing.params),
                       " parameter(s); using prior medians for: ", paste0(missing.params, collapse = ", ")))
        values[missing.params] = get.medians(prior)[missing.params]
    }
    values[prior@var.names]
}

# Saves the last simulation of a Step 1 simset as the Step 2 starting values; returns the file name
save.step2.start.values = function(location, simset, date = Sys.Date()){
    params.start.values = simset@parameters[simset@n.sim,]
    file = step2.start.values.file(location, date)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    save(params.start.values, file = file)
    print(paste0("Saved Step 2 starting values for ", location, ": ", file))
    invisible(file)
}

# Same, from a saved Step 1 simset or mcmc file (e.g., a Step 1 run from before the pipeline)
save.step2.start.values.from.file = function(location, file, date = Sys.Date()){
    e = new.env()
    obj = get(load(file, envir = e)[1], envir = e)
    if (!(methods::.hasSlot(obj, "n.sim") && methods::.hasSlot(obj, "parameters")))  # an mcmc, not a simset
        obj = simset.from.mcmc(obj)
    save.step2.start.values(location, obj, date = date)
}
