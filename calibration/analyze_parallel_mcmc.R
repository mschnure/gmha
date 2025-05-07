library("RColorBrewer")
library("scales")
library("ggsci")
source("model/run_systematic.R")

# Load "all.results" object that has: 
    # simset.list.full$no.int --> main simset, thinned to 200 sims and run to 2040
    # full.results.array --> full array that has all outcomes for all years and all sims, by age/sex
    # summary.results --> list with different summary results like median age, percent over X age, etc. 
    # export.to.csv --> summary results formatted into an array that can be easily esported to a csv 
#load('cached/all.results_mozambique_2025-03-25.Rdata')
#load('cached/all.results_tanzania_2025-03-31.Rdata')
#load('cached/all.results_uganda_2025-03-31.Rdata')
#load('cached/all.results_zambia_2025-03-31.Rdata')
# load('cached/all.results_zimbabwe_2025-03-31.Rdata')
load('cached/all.results_global_2025-04-15.Rdata')
simset = simset.list.full$no.int

#load("cached/simset_global_2025-04-15.Rdata")

simplot(simset,
        years = 1980:2040)

simplot(simset, 
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(simset, 
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age'), 
        data.types='hiv.mortality')

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence')

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence')

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='hiv.mortality')

simplot(simset, 
        years = 1980:2030, 
        data.types = "population")

simplot(simset,
        years = 1980:2030, 
        data.types = "population",
        facet.by='age')

simplot(simset,
        years=1980:2030, 
        data.types='total.mortality')

simplot(simset,
        years=1980:2030, 
        facet.by=c('age'), 
        data.types='total.mortality')

simplot(simset,
        years=2010:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        proportion=T)

simplot(simset,
        years=2010:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age','sex'), 
        proportion=T)



# AGE DISTRIBUTION PLOT
plot.limits = c("Global" = 5500000,
                "non.unaids.remainder" = 2000000,
                "unaids.remainder" = 1500000,
                "South Africa" = 1150000,
                "Mozambique" = 400000,
                "Nigeria" = 300000,
                "Tanzania" = 250000,
                "Uganda" = 250000,
                "Kenya" = 200000,
                "Zambia" = 200000,
                "Zimbabwe" = 200000,
                "Malawi" = 160000,
                "France" = 35000
)

plot.limit = plot.limits[simset@simulations[[1]]$location]
pal = c(brewer.pal(n=12,"Paired")[2],brewer.pal(n=12,"Paired")[5],brewer.pal(n=12,"Paired")[4]) 
alpha = 0.8

generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "no.int",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,plot.limit)) +
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "Status quo, 2040",
                                 "no.int/2040" = "Status quo, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2], 
                                     "no.int/2040" = pal[3]),alpha), 
                      name=NULL) +
    theme(text = element_text(size = 18),
          axis.title.y = element_text(colour = "grey37"),
          plot.title = element_text(size = 18, colour = "grey37"))+
    labs(title = simset@simulations[[1]]$location,subtitle = NULL) +
    ylab(label = "Number of people living with HIV")+
    guides(x =  guide_axis(angle = 45))
