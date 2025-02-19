########################################
# Description: Generate results/figures 
########################################

# install.packages("RColorBrewer")
library("RColorBrewer")
library("scales")
library("ggsci")
source("model/run_systematic.R")


#load("cached/all.results_kenya_2025-02-19.Rdata")
#load("cached/all.results_south_africa_2025-02-19.Rdata") 
load("cached/all.results_france_2025-02-19.Rdata")
simset.no.int = simset.list.full$no.int    

if(simset.no.int@simulations[[1]]$location=="Kenya")
    plot.limit = 200000 
if(simset.no.int@simulations[[1]]$location=="South Africa")
    plot.limit = 1200000 
if(simset.no.int@simulations[[1]]$location=="France")
    plot.limit = 35000 

pal = c(brewer.pal(n=12,"Paired")[2],brewer.pal(n=12,"Paired")[5],brewer.pal(n=12,"Paired")[4]) 
alpha = 0.8

##--------------------##
##-- PAPER FIGURES  --##
##--------------------##

## AGE DISTR - 2025 ONLY
jpeg(file=paste0("results/for_lancet_pres/",convert_string(simset.no.int@simulations[[1]]$location),"/age_dist_2025.jpeg"),
     width = 2000,height = 1500,res=200)
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
                                     "no.int/2040" = "white", # to remove second bar
                                     "no.int/2040" = pal[3]),alpha), 
                      name=NULL) +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(colour = "grey37"))+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of people living with HIV")+
    guides(x =  guide_axis(angle = 45))
dev.off()

## AGE DISTR - both years
jpeg(file=paste0("results/for_lancet_pres/",convert_string(simset.no.int@simulations[[1]]$location),"/age_dist.jpeg"),
     width = 2000,height = 1500,res=200)
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
    theme(text = element_text(size = 20),
          axis.title.y = element_text(colour = "grey37"))+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of people living with HIV")+
    guides(x =  guide_axis(angle = 45))
dev.off()


# INCIDENCE
jpeg(file=paste0("results/for_lancet_pres/",convert_string(simset.no.int@simulations[[1]]$location),"/inc_age.jpeg"),
     width = 3000,height = 900,res=200)
simplot(simset.no.int,
        data.types = "incidence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=1990:2020, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
dev.off() 

jpeg(file=paste0("results/for_lancet_pres/",convert_string(simset.no.int@simulations[[1]]$location),"/prev_age.jpeg"),
     width = 3000,height = 900,res=200)
simplot(simset.no.int,
        data.types = "prevalence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=1990:2020, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
dev.off() 
