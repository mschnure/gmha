source("model/run_systematic.R")
source("manuscript/manuscript_figure_settings.R")

load("cached/all.results_r1.high_2026-07-13.Rdata")
simset.7.13 = simset.list.full$no.int
full.results.array.7.13 = full.results.array

load("cached/all.results_r1.high_2026-08-05.Rdata")
simset.8.5 = simset.list.full$no.int
full.results.array.8.5 = full.results.array

simplot(#simset.7.13,
        simset.8.5, # @simulations[[length(simset.8.5@simulations)]],
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence') 

simplot(#simset.7.13,
  simset.8.5@simulations[[length(simset.8.5@simulations)]],
  years=1980:2040, 
  facet.by='age', 
  #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
  data.types='prevalence') 


simplot(#simset.7.13,#@simulations[[length(simset.7.13@simulations)]],
        #simset.8.5@simulations[[1]],
        simset.8.5@simulations[[length(simset.8.5@simulations)]],
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence') # + geom_hline(yintercept = 5000) 

transmission.params = simset.8.5@parameter.names[grepl("transmission|trate",simset.8.5@parameter.names)]
cbind(#simset.7.13@parameters[2487,transmission.params],
      simset.8.5@parameters[2484,transmission.params])
cbind(simset.8.5@parameters[2484,grepl("age.30.to.39.trans",simset.8.5@parameter.names)])

age.labels = simset.8.5@simulations[[1]]$AGES
age.labels[length(age.labels)] = "80+"

generate.age.distribution(full.results.array.7.13, 
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

generate.age.distribution(full.results.array.8.5, 
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
