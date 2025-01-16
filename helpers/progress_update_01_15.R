source("model/run_systematic.R")
source("future_projections/extract_projection_results.R")

load("mcmc_runs/simset_south_africa_2025-01-13.Rdata")
simset.south.africa = simset

load("mcmc_runs/simset_france_2025-01-13.Rdata")
simset.france = simset

load("mcmc_runs/simset_kenya_2025-01-13.Rdata")
simset.kenya = simset


simplot(simset.kenya,
        #simset.south.africa,
        #simset.france,
        years = 1980:2030, 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france, #@simulations[[simset@n.sim]],
        years = 1980:2030, 
        data.types = "population",
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france, #@simulations[[simset@n.sim]],
        years = 1980:2030, 
        data.types = "population",
        facet.by='age', 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france,
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence', 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france,
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence', 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence', 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence', 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france,
        years=1980:2030, 
        facet.by=c('age'), 
        data.types='hiv.mortality', 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france, #@simulations[[simset@n.sim]],  
        years=1980:2030, 
        data.types='total.mortality', 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france, #@simulations[[simset@n.sim]],
        years=1980:2030, 
        facet.by=c('age'), 
        #sexes = "male",
        data.types='total.mortality', 
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france,
        years=1980:2020, 
        #facet.by=c('age','sex'), 
        data.types='engagement', 
        proportion=T,
        show.individual.sims = F)

simplot(simset.kenya,
        #simset.south.africa,
        #simset.france,
        years=1980:2020, 
        #facet.by=c('age','sex'),
        # ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='suppression', 
        proportion=T,
        show.individual.sims = F)


## MORTALITY BREAKDOWN - KENYA ## 

sim.last = simset.kenya@simulations[[simset.kenya@n.sim]]

total.mort.sim = extract.data(sim.last,
                              data.type = "total.mortality",
                              years = c(1980:2023),
                              keep.dimensions = c("year","age"))
non.hiv.mort.sim = extract.data(sim.last,
                                data.type = "non.hiv.mortality",
                                years = c(1980:2023),
                                keep.dimensions = c("year","age"))

hiv.mort.sim = extract.data(sim.last,
                            data.type = "hiv.mortality",
                            years = c(1980:2023),
                            keep.dimensions = c("year","age"))

plot.df.non.hiv.mort = melt(non.hiv.mort.sim)
plot.df.hiv.mort = melt(hiv.mort.sim)

simplot(sim.last,
        years=1980:2030, 
        facet.by=c('age'), 
        data.types='total.mortality', 
        show.individual.sims = F) + 
    geom_line(data = plot.df.non.hiv.mort, aes(x = year, y = value)) +
    #geom_line(data = plot.df.hiv.mort, aes(x = year, y = value)) +
    facet_wrap(~age, scales = "free_y") + 
    ylim(0,NA)

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age'), 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality', 
        show.individual.sims = F)

## AGE DISTRIBUTION FIGURE COMPARISON - KENYA ## 

load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/kenya_hivncd/cached/all.results_2023-04-12.Rdata")
simset.old = simset.list.full$no.int

sim.old = simset.list.full$no.int@simulations[[simset.list.full$no.int@n.sim]]
sim.old$location = "Kenya"
full.results.array.old = full.results.array

# if I do this, the plot is similarly weird (like my new results)
#full.results.array.old[,,,,c(1:101),] = full.results.array.old[,,,,c(102),] 

load("cached/all.results_kenya_2025-01-13.Rdata")
sim.new = simset.list.full$no.int@simulations[[simset.list.full$no.int@n.sim]]
full.results.array.new = full.results.array

load("mcmc_runs/mcmc_files/mcmc_kenya_2025-01-13.Rdata")
simset.new.tail = suppressWarnings(extract.simset(mcmc,
                                                  additional.burn=5998,
                                                  additional.thin=1))

simset.new.tail.no.int = run.intervention.on.simset(simset.new.tail,
                                                    end.year = 2040,
                                                    intervention = NO.INTERVENTION)

simset.list.new.tail = list(no.int = simset.new.tail.no.int)

full.results.array.new.tail = generate.full.results.array(simset.list = simset.list.new.tail) # thinned to two sims

pal = c(brewer.pal(n=12,"Paired")[2],brewer.pal(n=12,"Paired")[5],brewer.pal(n=12,"Paired")[4]) # use ALPHA = 0.8
alpha = 0.8
# full.results.array.new, full.results.array.new.tail, full.results.array.old
generate.age.distribution(full.results.array.new, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "no.int",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,200000)) +  # 200000 for kenya; 1000000 for south africa 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "Status quo, 2040",
                                 "no.int/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "no.int/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(colour = "grey37"))+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of people living with HIV")+
    guides(x =  guide_axis(angle = 45))

simplot(sim.old, sim.new,
        years=1980:2040, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence', 
        show.individual.sims = F) + geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 

