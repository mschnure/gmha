source("model/run_systematic.R")

# load("final_simsets_and_results/updated_04_14_2026/all.results_r1.high_2026-04-14.Rdata")
# simset.old = simset.list.full$no.int
# full.results.array.old = full.results.array

load("cached/all.results_r1.high_2026-06-18.Rdata")
simset.6.18 = simset.list.full$no.int
full.results.array.6.18 = full.results.array

load("cached/all.results_r1.high_2026-06-25.Rdata")
simset.6.25 = simset.list.full$no.int
full.results.array.6.25 = full.results.array

load("cached/all.results_r1.high_2026-06-29_thinned.Rdata")
simset.6.29 = simset.list.full$no.int
full.results.array.6.29 = full.results.array

simplot(simset.6.25,
        simset.6.29,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence') 

simplot(simset.6.18,
        simset.6.25,
        years=1980:2040, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence') + geom_hline(yintercept = 2500) 

transmission.params = simset.6.29@parameter.names[grepl("transmission|trate",simset.6.29@parameter.names)]
cbind(simset.6.25@parameters[2485,transmission.params],simset.6.29@parameters[100,transmission.params])


params.manual = simset.6.25@parameters[length(simset.6.25@simulations),]
params.manual["trate.0"] = .64 # 0.6631672  
params.manual["trate.1"] = .13 # 0.08307097 
#params.manual["trate.2"] # 0.1257308 

sim.manual = run.model.for.parameters(location="r1.high",variable.parameters = params.manual)

simplot(simset.6.25@simulations[[length(simset.6.25@simulations)]],
        sim.manual,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence') 



generate.age.distribution(full.results.array.6.18, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "no.int",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(NULL,NULL)) +
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "Forecasted 2040",
                                 "no.int/2040" = "Forecasted 2040"), 
                      values=alpha(c("no.int/2025" = AGE.DIST.PAL[1],
                                     "no.int/2040" = AGE.DIST.PAL[2], 
                                     "no.int/2040" = AGE.DIST.PAL[3]),AGE.DIST.ALPHA), 
                      name=NULL) +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(colour = "black",vjust = 3,
                                      margin = margin(l = 15)),
          axis.text = element_text(colour = "black"),
          legend.position = c(0.75, 0.87)
    )+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of new HIV infections") +
    scale_x_discrete( 
        labels = age.labels) +
    guides(x =  guide_axis(angle = 45))

generate.age.distribution(full.results.array.6.25, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "no.int",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(NULL,NULL)) +
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "Forecasted 2040",
                                 "no.int/2040" = "Forecasted 2040"), 
                      values=alpha(c("no.int/2025" = AGE.DIST.PAL[1],
                                     "no.int/2040" = AGE.DIST.PAL[2], 
                                     "no.int/2040" = AGE.DIST.PAL[3]),AGE.DIST.ALPHA), 
                      name=NULL) +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(colour = "black",vjust = 3,
                                      margin = margin(l = 15)),
          axis.text = element_text(colour = "black"),
          legend.position = c(0.75, 0.87)
    )+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of new HIV infections") +
    scale_x_discrete( 
        labels = age.labels) +
    guides(x =  guide_axis(angle = 45))
