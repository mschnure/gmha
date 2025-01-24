
# Sets up initial population by mapping surveillance data age brackets to model age brackets (using 
# map.population.ages function); adds all initial population to HIV negative status in 1970 except
# for specified seed cases 
get.initial.population = function(year,
                                  location,
                                  data.manager,
                                  parameters,
                                  model.age.cutoffs,
                                  ages,
                                  sexes,
                                  seed.to.ages,
                                  seed.to.sexes,
                                  seed.n){
    
    pop = get.surveillance.data(data.manager = data.manager,
                                location = location,
                                data.type = "population.full",
                                years = 1970,
                                keep.dimensions = c('year','age','sex','location')) 
    
    pop.ages = dimnames(pop)$age
    pop.ages = c(pop.ages[-length(pop.ages)],"100 and over")
    dimnames(pop)$age = pop.ages
    
    POPULATION.AGE.MAPPING = map.population.ages(data.manager = data.manager,
                                                 data.type = "population.full",
                                                 model.age.cutoffs = model.age.cutoffs)
    
    # sum up population from surveillance data in to correct model age brackets
    initial.pop = sapply(location,function(location){
        sapply(sexes, function(sex){
            sapply(1:length(POPULATION.AGE.MAPPING), function(age){
                age.to = names(POPULATION.AGE.MAPPING)[age] # names of mapping are the model ages - what I want to map TO
                ages.from = POPULATION.AGE.MAPPING[[age]] # list elements are the population ages - what I want to map FROM
                sum(pop[,ages.from,sex,location])
            })
        })
    })
    
    # correct the dimensions on initial.pop
    new.dim.names = list(year = "1970",
                         age = names(POPULATION.AGE.MAPPING),
                         sex = sexes)
    
    dim(initial.pop) = sapply(new.dim.names, length)
    dimnames(initial.pop) = new.dim.names
    
    # set up array for model, indexed [age,sex,subgroup,hiv status]
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    rv = array(0,
               dim = sapply(state.dim.names, length),
               dimnames = state.dim.names)
    
    # add in initial population to hiv negative 
    rv[,,,'hiv_negative'] = initial.pop
    
    #puts n hiv cases in each of those brackets (probably middle age bracket, one male/female)
    rv[seed.to.ages,seed.to.sexes,,'undiagnosed'] = seed.n 
    
    rv
}
