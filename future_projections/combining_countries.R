
# takes simsets and a vector of the country names 
# returns a list where each element is an outcome
# each outcome is an array with the usual dimensions but also a new dimension "location"
combine.simsets = function(...,
                           countries){
    simset.list = list(...)
    
    outcome.names = names(simset.list[[1]]@simulations[[1]])[-1:-9]
    outcome.names = outcome.names[-length(outcome.names)]
    sim.combined = simset.list[[1]]@simulations[[1]]
    n.sim = length(simset.list[[1]]@simulations)
    
    outcomes = sim.combined[outcome.names]
    dim.name.list = sapply(outcomes,dimnames)
    dim.name.list = sapply(dim.name.list,function(x){
        
        c(x,list(sim = c(1:n.sim),
                 country = countries))
    })
    
    rv = list()
    for (x in outcome.names) {
        rv[[x]] = array(NA,
                        dim = sapply(dim.name.list[[x]], length),
                        dimnames = dim.name.list[[x]])
    }
    
    for(simset.index in 1:length(simset.list)){
        for(sim.index in 1:length(simset.list[[simset.index]]@simulations)){
            for(outcome in outcome.names){
                dim.names = names(dimnames(rv[[outcome]]))
                sim.dim = which(dim.names == "sim")
                country.dim = which(dim.names == "country")
                subgroup.dim = which(dim.names == "subgroup")
                
                #set which sim, country, and subgroup dimensions I want 
                index = rep(list(quote(expr=)), length(dim.names))
                index[[sim.dim]] = sim.index
                index[[country.dim]] = simset.index # fill in simset-specific country 
                if (length(subgroup.dim) > 0) {
                    index[[subgroup.dim]] = TRUE # keep all subgroups
                }
                
                sim = simset.list[[simset.index]]@simulations[[sim.index]]
                
                rv[[outcome]] = do.call("[<-", c(list(rv[[outcome]]), index, list(value = sim[[outcome]])))
                
            }
            
        }
    }
    
    rv
    
}

# sorts the output of combine simsets (a list) by the 2022 prevalence value for each sim 
sort.by.prevalence = function(result.list){
    
    total.prev.2022 = apply(result.list$prevalence["2022",,,,,,],c("sim","country"),sum)
    
    countries = dimnames(total.prev.2022)[[2]]
    
    country.sim.indices = array(NA,
                                dim = sapply(dimnames(total.prev.2022),length),
                                dimnames = dimnames(total.prev.2022))
    
    for(country in countries){
        
        total.prev.country = total.prev.2022[,country]
        sorted.sim.index = names(sort(total.prev.country,na.last = F)) # lowest to highest 
        
        country.sim.indices[,country] = sorted.sim.index
    }
    
    rv = sapply(result.list, function(x){
        
        if(length(dim(x))==7){ # population, prevalence, mortality (x3)
            for(country in countries){
                sorted.country = x[,,,,,country.sim.indices[,country],country]
                x[,,,,,,country] = sorted.country
            }
        } else if(length(dim(x))==6){ # incidence, diagnoses, cascade transitions (4)
            for(country in countries){
                sorted.country = x[,,,,country.sim.indices[,country],country]
                x[,,,,,country] = sorted.country
            }
        } else stop("outcome with different dim length")
        
        x
    })
    
    rv
    
    
}

# takes the output of sort.by.prevalence (a list)
# applies over all of the outcomes in the list and collapses over thh country dimension 
# returns another list of outcomes 
collapse.country.dim = function(result.list){
    
    # apply over the list, sum over the last dimension of each array (country)
    rv = sapply(names(result.list), function(x){
        
        dim.names = dimnames(result.list[[x]])
        dim.names = dim.names[-length(dim.names)]
        arr = apply(result.list[[x]],c(1:(length(dim(result.list[[x]]))-1)),sum) 
        
        dim(arr) = sapply(dim.names,length) # otherwise it will collapse subgroup dimension 
        dimnames(arr) = dim.names
        
        arr
        
    }, simplify = F)
    
    rv
}

# takes the list that comes out of collapse.country.dim, where each element is an outcome 
# for each outcome in the list, takes the first sim, combines all of those arrays into a list
# returns a list of sims; each sim is list of outcomes; each outcome is an array 
convert.to.simset = function(collapsed.result.list) {
    
    dim_vars = c("years", "location", "AGES", "SEXES", "SUBGROUPS",
                  "HIV.STATUS", "HIV.STATES", "DIAGNOSED.STATES", "ENGAGED.STATES","parameters")
    
    # set up basic parameters list to add to sims 
    parsed.ages = parse.age.brackets(MODEL.AGE.CUTOFFS)
    parameters = list()
    parameters$AGES = parsed.ages$labels
    parameters$AGE.SPANS = parsed.ages$spans
    parameters$AGE.LOWERS = parsed.ages$lowers
    names(parameters$AGE.LOWERS) = parameters$AGES
    parameters$AGE.UPPERS = parsed.ages$uppers
    names(parameters$AGE.UPPERS) = parameters$AGES
    
    # Find which dimension is "sim" in each array
    sim_dims = sapply(collapsed.result.list, function(x) {
        sim_dim = which(names(dimnames(x)) == "sim")
        if (length(sim_dim) != 1) stop("Each array must have exactly one 'sim' dimension")
        sim_dim
    })
    
    # Find which dimension is "subgroup" in each array
    subgroup_dims = sapply(collapsed.result.list, function(x) {
        subgroup_dim = which(names(dimnames(x)) == "subgroup")
        if (length(subgroup_dim) != 1) stop("Each array must have exactly one 'sim' dimension")
        subgroup_dim
    })
    
    # Get number of sims (assumes all arrays have the same number of sims)
    n.sim = dim(collapsed.result.list[[1]])[sim_dims[[1]]]
    
    # Extract metadata vectors from the first array
    first.array = collapsed.result.list[[1]]
    dim.names = dimnames(first.array)
    
    metadata = lapply(dim_vars, function(dvar) {
        if(dvar=="years") vec = as.numeric(dim.names[["year"]])
        if(dvar=="location") vec = "Global"
        if(dvar=="AGES") vec = (dim.names[["age"]])
        if(dvar=="SEXES") vec = (dim.names[["sex"]])
        if(dvar=="SUBGROUPS") vec = (dim.names[["subgroup"]])
        if(dvar=="HIV.STATUS") vec = c("hiv_negative","undiagnosed","diagnosed_unengaged","engaged_unsuppressed","engaged_suppressed")
        if(dvar=="HIV.STATES") vec = c("undiagnosed","diagnosed_unengaged","engaged_unsuppressed","engaged_suppressed")
        if(dvar=="DIAGNOSED.STATES") vec = c("diagnosed_unengaged","engaged_unsuppressed","engaged_suppressed")
        if(dvar=="ENGAGED.STATES") vec = c("engaged_unsuppressed","engaged_suppressed")
        if(dvar=="parameters") vec = parameters
        return(vec)

    })
    names(metadata) = dim_vars
    
    # Initialize result list
    result = vector("list", n.sim)
    
    for (i in 1:n.sim) {
        # For each sim index, extract the i-th sim from all arrays
        sim_i = mapply(function(x, sim_dim) {
            # Create a list of slices: ":" for all dims, except take i for sim_dim
            slice = lapply(1:length(dim(x)), function(j) if (j == sim_dim) i else quote(expr = ))
            do.call(`[`, c(list(x), slice, list(drop = FALSE)))
            
        }, collapsed.result.list, sim_dims, SIMPLIFY = FALSE)

        # Preserve original names
        names(sim_i) = names(collapsed.result.list)
        
        for (j in 1:length(sim_i)) {
            arr = sim_i[[j]]
            dims = dim(arr)

            new_dims <- dims[-length(dims)]
            new_dimnames <- dimnames(arr)[-length(dims)]
        
            sim_i[[j]] <- array(arr, dim = new_dims, dimnames = new_dimnames)            
            
        }

        # Store in result
        result[[i]] = c(metadata,sim_i)
        
    }
    
    return(result)
}


## old code 

combine.countries.OLD = function(...,
                             countries){
    results = list(...)
    rv.all.data = list()
    
    rv.combined.data = list()
    
    data.types = c("population","prevalence","incidence","diagnoses","hiv.mortality",
                   "non.hiv.mortality",#"total.mortality",
                   "annual.engagement",
                   "disengagement.unsuppressed","disengagement.suppressed","annual.suppression")
    
    for(data.type in data.types){
        x = combine.countries.by.data.type(results,countries = countries,data.type=data.type)
        rv.all.data[[data.type]] = x$all.data
        rv.combined.data[[data.type]] = x$combined.data
    }
    
    rv = list("all.data" = rv.all.data,
              "combined.data" = rv.combined.data)
    
    rv
    
}

combine.countries.by.data.type.OLD = function(results,
                                          countries,
                                          data.type){
    
    if(!is.list(results))
        stop("pass all results arrays as a list")
    dim.check = sapply(results,function(x){
        (dim(x)[5])
    })
    
    if(length(unique(dim.check))!=1)
        stop("not all results array have the same number of sims")
    
    data.one.country = results[[1]][,,,data.type,,]
    dim.names = c(dimnames(data.one.country),
                  list("country"=countries))
    
    data = array(NA,
                 dim = sapply(dim.names,length),
                 dimnames = dim.names)
    
    for(country in 1:length(countries)){
        data[,,,,country] = results[[country]][,,,data.type,,]
        
    }
    
    all.data = data 
    combined.data = apply(data,c(1:4),sum)
    
    rv = list("all.data" = all.data,
              "combined.data" = combined.data)
    
    rv
    
}
# 
# z = combine.countries(unaids.results,non.unaids.results,
#                       mozambique.results,tanzania.results,
#                       uganda.results,zambia.results,
#                       zimbabwe.results,
#                       countries = countries)
# 
# y = combine.countries.by.data.type(list(unaids.results,non.unaids.results,
#                                    mozambique.results,tanzania.results,
#                                    uganda.results,zambia.results,
#                                    zimbabwe.results),
#                                    countries = countries,
#                                    data.type="annual.engagement")
# dim(x$all.prev)
# 
# test.simset = z$combined.data
# 
# names(simset@simulations[[1]])
# names(test.simset)

# make a list that combines all the data arrays; make that a "simset"; could use a location dimension 
# 10 simset objects; apply 1:nsim; for i, merge simsets; simset [1,i]
#function that takes a list of simulation objects (not a simset object); and combines them into one simulation object 
# add an extra dimension for location 
# take all the first sims; combine across arrays; return that as sim1.all countries 
# can't combine parameters or anything like that 
# other complication: the sims aren't correlated; could rank order the simulations by something; in prevalence in one year
# or aggregate prevalence over a few years 
# couldn't then run combined sensitivity analysis; country-specific 

# to make it a simulation, make it a list 



