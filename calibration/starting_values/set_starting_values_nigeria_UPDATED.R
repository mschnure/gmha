source("model/run_systematic.R")

prior = NIGERIA.PRIOR
params.start.values = get.medians(prior)
params.manual = params.start.values

# sim.nigeria = run.model.for.parameters(variable.parameters = params.start.values,
#                          end.year = 2040,
#                          location = "Nigeria")

params.manual["age.15.to.19.transmission.multiplier.0"] = 0.4  # 1 
params.manual["age.15.to.19.transmission.multiplier.1"] = 0.4  # 1 
params.manual["age.30.to.39.transmission.multiplier.0"] = 1.35  # 1
params.manual["age.40.to.49.transmission.multiplier.0"] = 1.3  # 1
params.manual["age.50.and.over.transmission.multiplier.0"] = 0.5  # 1
params.manual["age.50.and.over.transmission.multiplier.1"] = 0.5  # 1
params.manual["age.50.and.over.transmission.multiplier.2"] = 0.5  # 1
params.manual["age.50.and.over.transmission.multiplier.3"] = 0.25  # 1
params.manual["over.50.hiv.mortality.multiplier.0"] = 2  # 1
params.manual["over.50.hiv.mortality.multiplier.1"] = 2  # 1
params.manual["over.50.hiv.mortality.multiplier.2"] = 2  # 1
params.manual["age.0.to.4.hiv.mortality.multiplier.0"] = 2  # 1
params.manual["age.5.to.14.hiv.mortality.multiplier.0"] = 2  # 1
params.manual["age.0.to.14.hiv.mortality.multiplier.1"] = 2  # 1
params.manual["age.0.to.14.hiv.mortality.multiplier.2"] = 2  # 1
params.manual["age.25.to.49.hiv.mortality.multiplier.2"] = 2  # 1

params.start.values = params.manual

save(params.start.values,file=("calibration/starting_values/2026_10_12_nigeria_start_values.Rdata"))

if(1==2){
  sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                        end.year = 2040,
                                        location = "Nigeria")
  
  #round(cbind(params.manual),4)
  
  
  simplot(sim.nigeria,
          sim.manual,
          years=1980:2040, 
          facet.by='age', 
          data.types='incidence')
  
  
  simplot(sim.nigeria,
          sim.manual,
          years=1980:2040, 
          facet.by='age', 
          data.types='prevalence')
  
  simplot(sim.nigeria,
          sim.manual,
          years=1980:2040)  
}


