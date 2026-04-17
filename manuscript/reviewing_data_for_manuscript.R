library("RColorBrewer")
library("scales")
library("ggsci")
source("model/run_systematic.R")

# previous simsets
{
    load("final_simsets_and_results/all.results_all.low_2025-08-27.Rdata")
    simset.low.old = simset.list.full$no.int    
    
    load("final_simsets_and_results/all.results_all.lower.middle_2025-08-27.Rdata")
    simset.lower.middle.old = simset.list.full$no.int
    
    load("final_simsets_and_results/all.results_all.upper.middle_2025-08-27.Rdata")
    simset.upper.middle.old = simset.list.full$no.int
    
    load("final_simsets_and_results/all.results_all.high_2025-08-27.Rdata")
    simset.high.old = simset.list.full$no.int
    
    load("final_simsets_and_results/all.results_global_2025-08-27.Rdata")
    simset.global.old = simset.list.full$no.int
    
}

# updated simsets 
{
    load("final_simsets_and_results/updated_04_14_2026/all.results_all.low_2026-04-14.Rdata")
    simset.low = simset.list.full$no.int
    
    load("final_simsets_and_results/updated_04_14_2026/all.results_r1.low_2026-04-14.Rdata")
    simset.r1.low = simset.list.full$no.int
    
    load("final_simsets_and_results/updated_04_14_2026/all.results_all.lower.middle_2026-04-14.Rdata")
    simset.lower.middle = simset.list.full$no.int
    
    load("final_simsets_and_results/updated_04_14_2026/all.results_all.upper.middle_2026-04-14.Rdata")
    simset.upper.middle = simset.list.full$no.int
    
    load("final_simsets_and_results/updated_04_14_2026/all.results_all.high_2026-04-14.Rdata")
    simset.high = simset.list.full$no.int
    
    load("final_simsets_and_results/updated_04_14_2026/all.results_global_2026-04-14.Rdata")
    simset.global = simset.list.full$no.int
    
}

simplot(#simset.global.old,
        #simset.global,
        #simset.r1.low,
        # simset.low.old,
         simset.low,
        # simset.lower.middle.old,
        # simset.lower.middle,
        # simset.upper.middle.old,
        # simset.upper.middle,
        # simset.high.old,
        # simset.high,
        years = 1980:2040, 
        data.types = "population",
        facet.by='age')

simplot(simset.high,
        years = 1980:2040, 
        data.types = "prevalence",
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        facet.by='age') + geom_vline(xintercept = 2018)

simplot(simset.high,
        years = 1980:2040, 
        data.types = "incidence",
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        facet.by='age') + geom_vline(xintercept = 2018)


load("cached/all.results_r1.high_2026-04-14.Rdata")
simset.r1.high = simset.list.full$no.int

default.params = get.default.parameters(location = "r1.high")
variable.parameters.test = default.params
variable.parameters.test[simset@parameter.names] = simset.r1.high@parameters[490,] 

variable.parameters.test["age.40.to.49.transmission.multiplier.3"] = 0.5 # 1.65972766
variable.parameters.test["trate.3"] = 0.13 # 0.09182620

sim.test = run.model.for.parameters(location="r1.high",variable.parameters = variable.parameters.test)

cbind(variable.parameters.test[grepl("transmission.multiplier",names(variable.parameters.test))])
#cbind(variable.parameters.test[grepl("trate",names(variable.parameters.test))])

simplot(simset.high@simulations[[490]],
        sim.test,
        years = 1980:2040, 
        data.types = "incidence",
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        facet.by='age') + geom_vline(xintercept = 2018)



if(1==2){
    # load("cached/all.results_merged_mozambique_2025-08-25.Rdata")
    #simset.moz = simset.list.full$no.int
    #params.moz = simset.moz@parameters[1000,]
    
    variable.parameters.high=get.default.parameters(location = "r1.high")
    variable.parameters.low=get.default.parameters(location = "r1.low")
    
    params.high = parameters = create.model.parameters(location = "r1.high")
    params.low = parameters = create.model.parameters(location = "r1.low")
    
    mixing.model.high = c(unlist(params.high$male.to.female.age.model),unlist(params.high$female.to.male.age.model))
    mixing.model.low =  c(unlist(params.low$male.to.female.age.model),unlist(params.low$female.to.male.age.model))
    
    names(mixing.model.high) = names(mixing.model.low) = c("MtF.mean.age.diff.intercept","MtF.mean.age.diff.slope",
                                                           "MtF.sd.age.diff.intercept","MtF.sd.age.diff.slope",
                                                           "FtM.mean.age.diff.intercept","FtM.mean.age.diff.slope",
                                                           "FtM.sd.age.diff.intercept","FtM.sd.age.diff.slope")
    
    cbind(mixing.model.high,mixing.model.low)
    
    sim.high = run.model.for.parameters(location="r1.high",variable.parameters = variable.parameters.high)
    sim.low = run.model.for.parameters(location="r1.low",variable.parameters = variable.parameters.low)
    
    cbind(variable.parameters.low[grepl("hiv.specific",names(variable.parameters.low))],
          variable.parameters.high[grepl("hiv.specific",names(variable.parameters.high))])
    
    load("final_simsets_and_results/all.results_merged_r1.low_2025-08-25.Rdata")
    simset.low.remainder = simset.list.full$no.int
}

