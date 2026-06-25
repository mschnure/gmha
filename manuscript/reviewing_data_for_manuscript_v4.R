source("model/run_systematic.R")

load("cached/all.results_r1.high_2026-06-18.Rdata")
simset.new = simset.list.full$no.int
full.results.array.new = full.results.array

load("final_simsets_and_results/updated_04_14_2026/all.results_r1.high_2026-04-14.Rdata")
simset.old = simset.list.full$no.int
full.results.array.old = full.results.array

simplot(simset.old,
        simset.new,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence') 

simplot(simset.old,
        simset.new,
        years=1980:2040, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence') + geom_hline(yintercept = 2500) 

transmission.params = simset.new@parameter.names[grepl("transmission|trate",simset.new@parameter.names)]
cbind(simset.old@parameters[490,transmission.params],simset.new@parameters[490,transmission.params])


generate.age.distribution(full.results.array.old, 
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

generate.age.distribution(full.results.array.new, 
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
