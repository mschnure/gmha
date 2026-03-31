# install.packages("RColorBrewer")
library("RColorBrewer")
library("scales")
library("ggsci")
library(colorspace)
source("model/run_systematic.R")
source("manuscript/manuscript_figure_settings.R")

COUNTRIES = c("Kenya","Malawi","Mozambique","Nigeria","South Africa","Tanzania","Uganda","Zambia","Zimbabwe","global")
INCOME.COUNTRIES = c("all.low","all.lower.middle","all.upper.middle","all.high")

# Results file has 4 objects: 
# simset.list.full
# full.results.array
# summary.results
# export.to.csv

#### GLOBAL AGE DISTRIBUTION ####
print("Generating global age distribution")
file = "all.results_global_2025-08-27.Rdata"
print(paste0("loading ",file))
load(file.path("final_simsets_and_results",file))
simset.no.int = simset.list.full$no.int 

age.labels = simset.no.int@simulations[[1]]$AGES
age.labels[length(age.labels)] = "80+"
plot.limit = PLOT.LIMITS[simset.no.int@simulations[[1]]$location]

jpeg(file=paste0(PLOT.DIR,"age_dist/",convert_string(simset.no.int@simulations[[1]]$location),"_age_dist.jpeg"),
     width = AGE.DIST.PLOT.WIDTH,height = AGE.DIST.PLOT.HEIGHT,res=AGE.DIST.PLOT.RES)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "no.int",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,plot.limit)) +
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
          legend.position = c(0.15, 0.87) )+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of people living with HIV") +
    scale_y_continuous( 
        breaks = seq(0, 7000000, by = 1000000),
        limits = c(0,6100000),
        labels = label_number(scale = 1e-6, suffix = "M")) +
    scale_x_discrete( 
        labels = age.labels) +
    guides(x =  guide_axis(angle = 45))
dev.off()


#### GLOBAL GENERAL POPULATION AGE DISTRIBUTION ####
print("Generating global general population age distribution")
file = "all.results_global_2025-08-27.Rdata"
print(paste0("loading ",file))
load(file.path("final_simsets_and_results",file))
simset.no.int = simset.list.full$no.int 

jpeg(file=paste0(PLOT.DIR,"age_dist_gen_pop/",convert_string(simset.no.int@simulations[[1]]$location),"_age_dist_gen_pop.jpeg"),
     width = AGE.DIST.PLOT.WIDTH,height = AGE.DIST.PLOT.HEIGHT,res=AGE.DIST.PLOT.RES)
generate.age.distribution(full.results.array, 
                          outcome="population", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "no.int",year.3="2040",
                          percent=F,
                          sexes = c("female","male")) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "Forecasted 2040",
                                 "no.int/2040" = "Forecasted 2040"), 
                      values=alpha(c("no.int/2025" = AGE.DIST.PAL[1],
                                     "no.int/2040" = AGE.DIST.PAL[2],
                                     "all.max/2040" = AGE.DIST.PAL[3]),AGE.DIST.ALPHA), 
                      name=NULL) +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(colour = "black",vjust = 3,
                                      margin = margin(l = 15)),
          axis.text = element_text(colour = "black"),
          legend.position = c(0.75, 0.87)
    )+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of individuals") +
    scale_y_continuous( 
        breaks = seq(0, 700000000, by = 100000000),
        limits = c(0,725000000),
        labels = label_number(scale = 1e-6, suffix = "M")) +
    scale_x_discrete( 
        labels = age.labels) +
    guides(x =  guide_axis(angle = 45))
dev.off()

{ # General population summary stats
    outcomes = c("population")
    dim.names = list(intervention=dimnames(full.results.array)$intervention,
                     outcome.year = paste0(rep(outcomes,each=2),rep(c(".2025",".2040"),2)))
    
    gen.pop.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                         data.types = c("population"),
                                                         years = c(2025,2040))
    
    gen.pop.over.50.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                            age.point=50,
                                                            data.types = c("population"),
                                                            years=c(2025,2040))
    
}






#### GLOBAL INCIDENCE AGE DISTRIBUTION ####
print("Generating global incidence age distribution")
file = "all.results_global_2025-08-27.Rdata"
print(paste0("loading ",file))
load(file.path("final_simsets_and_results",file))
simset.no.int = simset.list.full$no.int 

age.labels = simset.no.int@simulations[[1]]$AGES
age.labels[length(age.labels)] = "80+"

jpeg(file=paste0(PLOT.DIR,"age_dist_incidence/",convert_string(simset.no.int@simulations[[1]]$location),"_age_dist.jpeg"),
     width = AGE.DIST.PLOT.WIDTH,height = AGE.DIST.PLOT.HEIGHT,res=AGE.DIST.PLOT.RES)
print(
    generate.age.distribution(full.results.array, 
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
)
dev.off()

{ # Incidence summary stats
    outcomes = c("incidence")
    dim.names = list(intervention=dimnames(full.results.array)$intervention,
                     outcome.year = paste0(rep(outcomes,each=2),rep(c(".2025",".2040"),2)))
    
    gen.pop.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                         data.types = c("incidence"),
                                                         years = c(2025,2040))
    
    gen.pop.over.50.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                            age.point=50,
                                                            data.types = c("incidence"),
                                                            years=c(2025,2040))
    
}



#### INCOME-LEVEL AGE DISTRIBUTIONS ####
print("Generating age distributions by income")
for(country in INCOME.COUNTRIES){
    files = list.files("final_simsets_and_results/")
    files = files[grepl("all.results",files)]
    file = files[grepl(paste0("_",convert_string(country),"_2025"),files)]
    print(paste0("loading ",file))
    load(file.path("final_simsets_and_results",file))
    simset.no.int = simset.list.full$no.int 
    
    age.labels = simset.no.int@simulations[[1]]$AGES
    age.labels[length(age.labels)] = "80+"
    plot.limit = PLOT.LIMITS[simset.no.int@simulations[[1]]$location]

    jpeg(file=paste0(PLOT.DIR,"age_dist/",convert_string(simset.no.int@simulations[[1]]$location),"_age_dist.jpeg"),
         width = AGE.DIST.PLOT.WIDTH,height = AGE.DIST.PLOT.HEIGHT,res=AGE.DIST.PLOT.RES)
    print(
        generate.age.distribution(full.results.array, 
                              outcome="prevalence", 
                              intervention.1 = "no.int",year.1="2025",
                              intervention.2 = "no.int",year.2="2040",
                              intervention.3 = "no.int",year.3="2040",
                              percent=F,
                              sexes = c("female","male"),
                              plot.limits=c(0,plot.limit)) +
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
              legend.position = c(0.15, 0.87)
        )+
        labs(title = NULL,subtitle = NULL) +
        ylab(label = "Number of people living with HIV") +
        scale_x_discrete( 
            labels = age.labels) +
        guides(x =  guide_axis(angle = 45))
    )
    dev.off()
}





#### COUNTRY-LEVEL RAINBOW PLOTS ####
print("Generating rainbow plot age distributions by country")
for(country in COUNTRIES){

    files = list.files("final_simsets_and_results/")
    files = files[grepl("all.results",files)]
    file = files[grepl(paste0("_",convert_string(country),"_2025"),files)]
    print(paste0("loading ",file))
    load(file.path("final_simsets_and_results",file))
    
    #dim(full.results.array)
    
    plot.percent = F
    
    stacked.area.array = full.results.array[as.character(2025:2040),,,"prevalence",,]
    stacked.area.array = apply(stacked.area.array,c("year","age","sim"),sum) # sum over sex 
    stacked.area.array = apply(stacked.area.array,c("year","age"),median,na.rm = T) # get median estimate
    
    if(plot.percent){
        stacked.area.array = stacked.area.array/rowSums(stacked.area.array)
    }
    
    # Absolute numbers 
    stacked.area.df = reshape2::melt(stacked.area.array)
    
    #my_palette = ggsci::pal_d3("category20")(17)
    #my_palette = qualitative_hcl(17, "Set 3")
    #my_palette = colorspace::qualitative_hcl(17, "Dark 3")
    #my_palette <- sequential_hcl(17,palette = "Viridis") # I like this one, but the below version makes the older ages more yellow: 
    vir <- sequential_hcl(1000, palette = "Viridis")
    pos <- c(
        seq(0.00, 0.20, length.out = 5),   # more density in blue region
        seq(0.30, 0.60, length.out = 5),   # green region
        seq(0.70, 1.00, length.out = 7)    # yellow region
    )
    my_palette <- vir[round(pos * (length(vir) - 1)) + 1]
    
    # For visualizing in R 
    plot = ggplot(data=stacked.area.df, aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=value), stat='identity', position=position_stack(reverse = T)) +
        #facet_wrap(vars(stateName), scales="free_y") +
        ggtitle(paste0("Age Distribution, ",country)) +
        scale_fill_manual(values=my_palette) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=F)) +
        labs(y = "Cases", x = "Year", fill="Age") +
        labs(y = NULL, x = NULL, fill="Age") +
        theme_bw() +
        theme(legend.position="bottom") + 
        guides(fill = guide_legend(ncol = 6)) ; plot
    
    
    # For saving individual panels: 
    plot = ggplot(data=stacked.area.df, aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=value), stat='identity', position=position_stack(reverse = T)) +
        #facet_wrap(vars(stateName), scales="free_y") +
        #ggtitle(paste0("Age Distribution, ",country)) +
        scale_fill_manual(values=my_palette) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=F)) +
        #labs(y = "Cases", x = "Year", fill="Age") +
        labs(y = NULL, x = NULL, fill="Age") +
        theme_bw() +
        theme(legend.position="none")  + 
        theme(
            text = element_text(size = 6),
            axis.text = element_text(size = 5),
            legend.text = element_text(size = 5)
        )
    
    # for 5-row version 
    if(1==1){
        ggsave(
            filename = paste0("manuscript/manuscript_figures/stacked_bar_chart/stacked_bar_chart_", country, ".png"),
            plot = plot,
            width = 2.5,
            height = 1.4,
            units = "in"
        )    
    }
    
    # for 5-column version 
    if(1==2){
        ggsave(
            filename = paste0("manuscript/manuscript_figures/stacked_bar_chart_", country, ".png"),
            plot = plot,
            width = 1.4,
            height = 2.5,
            units = "in"
        )    
    }
    
    # Just for generating legend
    if(1==2){
        plot.for.legend = ggplot(data=stacked.area.df, aes(x=year, fill=age)) +
            geom_bar(mapping=aes(y=value), stat='identity', position=position_stack(reverse = T)) +
            #facet_wrap(vars(stateName), scales="free_y") +
            #ggtitle(paste0("Age Distribution, ",country)) +
            scale_fill_manual(values=my_palette) +
            scale_y_continuous(labels=comma) +
            guides(fill = guide_legend(reverse=F)) +
            labs(y = "Cases", x = "Year", fill="Age") +
            theme_bw() +
            theme(legend.position="bottom") + 
            guides(fill = guide_legend(ncol = 6))
        
        ggsave(filename = paste0("manuscript/manuscript_figures/plot_for_legend.png"),
               plot=plot.for.legend,width = 7, height=6)
        
        
        
    }

}


#### COUNTRY-LEVEL AGE DISTRIBUTION PLOTS ####
print("Generating original age distributions by country")
for(country in COUNTRIES[-length(COUNTRIES)]){
    files = list.files("final_simsets_and_results/")
    files = files[grepl("all.results",files)]
    file = files[grepl(paste0("_",convert_string(country),"_2025"),files)]
    print(paste0("loading ",file))
    load(file.path("final_simsets_and_results",file))
    simset.no.int = simset.list.full$no.int 
    
    plot.limit = PLOT.LIMITS[simset.no.int@simulations[[1]]$location]
    
    jpeg(file=paste0(PLOT.DIR,"age_dist/",convert_string(simset.no.int@simulations[[1]]$location),"_age_dist.jpeg"),
         width = AGE.DIST.PLOT.WIDTH,height = AGE.DIST.PLOT.HEIGHT,res=AGE.DIST.PLOT.RES)
    print(
        generate.age.distribution(full.results.array, 
                                  outcome="prevalence", 
                                  intervention.1 = "no.int",year.1="2025",
                                  intervention.2 = "no.int",year.2="2040",
                                  intervention.3 = "no.int",year.3="2040",
                                  percent=F,
                                  sexes = c("female","male"),
                                  plot.limits=c(0,plot.limit)) +
            scale_fill_manual(labels = c("no.int/2025" = "2025",
                                         "no.int/2040" = "Forecasted 2040",
                                         "no.int/2040" = "Forecasted 2040"), 
                              values=alpha(c("no.int/2025" = AGE.DIST.PAL[1],
                                             "no.int/2040" = AGE.DIST.PAL[2], 
                                             "no.int/2040" = AGE.DIST.PAL[3]),AGE.DIST.ALPHA), 
                              name=NULL) +
            theme(text = element_text(size = 20),
                  axis.title.y = element_text(colour = "grey37"))+
            labs(title = NULL,subtitle = NULL) +
            ylab(label = "Number of people living with HIV")+
            guides(x =  guide_axis(angle = 45))
    )
    dev.off()
}








