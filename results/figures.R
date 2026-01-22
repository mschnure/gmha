########################################
# Description: Generate results/figures 
########################################

# install.packages("RColorBrewer")
library("RColorBrewer")
library("scales")
library("ggsci")
source("model/run_systematic.R")

COUNTRIES = c("Mozambique","Uganda","Kenya","Zambia",
    "Zimbabwe","unaids.remainder","non.unaids.remainder",
    "r1.low","r1.lower.middle","r1.upper.middle","r1.high",
    "all.low","all.lower.middle","all.upper.middle","all.high",
    "global",
    "Malawi" ,"South Africa"#,
    #"Tanzania","Nigeria"
    
)

# do these ones manually and check simset.no.int@simulations[[1]]$location to make sure they save in the right location
# Tanzania, Nigeria, global, global.income, all.low, all.lower.middle, all.upper.middle, all.high 

country = COUNTRIES[14]

#for(country in COUNTRIES){
    
    files = list.files("final_simsets_and_results/")
    files = files[grepl("all.results",files)]
    file = files[grepl(paste0("_",convert_string(country),"_2025"),files)]
    print(paste0("loading ",file))
    load(file.path("final_simsets_and_results",file))
    
    #load("final_simsets_and_results/all.results_global_2025-08-27.Rdata")
    simset.no.int = simset.list.full$no.int    
    
    plot.limits = c("Global" = 6000000,
                    "non.unaids.remainder" = 2250000,
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
                    "France" = 35000,
                    "all.low" = 1000000,
                    "all.lower.middle" = 1600000,
                    "all.upper.middle" = 1750000,
                    "all.high" = 100000
    )
    plot.limit = plot.limits[simset.no.int@simulations[[1]]$location]
    
    pal = c(brewer.pal(n=12,"Paired")[2],brewer.pal(n=12,"Paired")[5],brewer.pal(n=12,"Paired")[4]) 
    alpha = 0.8
    
    ## AGE DISTR - both years
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/age_dist.jpeg"),
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
                                     "no.int/2040" = "Forecasted 2040",
                                     "no.int/2040" = "Forecasted 2040"), 
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
    
    
    if(simset.no.int@simulations[[1]]$location=="Global"){
        age.labels = simset.no.int@simulations[[1]]$AGES
        age.labels[length(age.labels)] = "80+"
        
        jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/age_dist.jpeg"),
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
                                         "no.int/2040" = "Forecasted 2040",
                                         "no.int/2040" = "Forecasted 2040"), 
                              values=alpha(c("no.int/2025" = pal[1],
                                             "no.int/2040" = pal[2], 
                                             "no.int/2040" = pal[3]),alpha), 
                              name=NULL) +
            theme(text = element_text(size = 20),
                  axis.title.y = element_text(colour = "black",vjust = 3,
                                              margin = margin(l = 15)),
                  axis.text = element_text(colour = "black"),
                  legend.position = c(0.15, 0.87)
            )+
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
    }
    
    # income plots
    if(simset.no.int@simulations[[1]]$location %in% c("all.low","all.lower.middle","all.upper.middle","all.high")){
        age.labels = simset.no.int@simulations[[1]]$AGES
        age.labels[length(age.labels)] = "80+"
        
        jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/age_dist.jpeg"),
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
                                         "no.int/2040" = "Forecasted 2040",
                                         "no.int/2040" = "Forecasted 2040"), 
                              values=alpha(c("no.int/2025" = pal[1],
                                             "no.int/2040" = pal[2], 
                                             "no.int/2040" = pal[3]),alpha), 
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
        dev.off()
        
    }
    
    
    ### ALL YEARS ### 
    
    # INCIDENCE, 1980 to 2025
    # Total
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),
                     "/",Sys.Date(),"/inc_age_total.jpeg"),
         width = 750,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "incidence",
            ages = c("All ages"),
            facet.by = "age",
            years=1980:2025, 
            show.individual.sims = F,
            for.paper = T,
            ncol=1) +
        theme(strip.text.x = element_blank(),
              axis.title.x=element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    # By age 
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/inc_age_all.jpeg"),
         width = 3000,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "incidence",
            ages = c("All ages","0-14","15-49","50 and over"),
            facet.by = "age",
            years=1980:2025, 
            show.individual.sims = F,
            for.paper = T,
            ncol=4) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    # PREVALENCE, 1980 to 2025
    # Total
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),
                     "/",Sys.Date(),"/prev_age_total.jpeg"),
         width = 750,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "prevalence",
            ages = c("All ages"),
            facet.by = "age",
            years=1980:2025, 
            show.individual.sims = F,
            for.paper = T,
            ncol=1) +
        theme(strip.text.x = element_blank(),
              axis.title.x=element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    # By age 
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/prev_age_all.jpeg"),
         width = 3000,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "prevalence",
            ages = c("All ages","0-14","15-49","50 and over"),
            facet.by = "age",
            years=1980:2025, 
            show.individual.sims = F,
            for.paper = T,
            ncol=4) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    
    ## SPLIT BY INSTEAD OF FACET.BY ##
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/inc_one_panel.jpeg"),
         width = 2000,height = 1500,res=200)
    simplot(simset.no.int,
            data.types = "incidence",
            ages = c("0-14","15-49","50 and over"),
            split.by = "age",
            years=1980:2025,
            show.individual.sims = F,
            for.paper = T) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off()
    
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/prev_one_panel.jpeg"),
         width = 2000,height = 1500,res=200)
    simplot(simset.no.int,
            data.types = "prevalence",
            ages = c("0-14","15-49","50 and over"),
            split.by = "age",
            years=1980:2025,
            show.individual.sims = F,
            for.paper = T) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off()
    
    
    write.csv(export.to.csv, file = paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/full.export.csv"))
    
#}


## not using 
if(1==2){
    ## AGE DISTR - 2025 ONLY
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/age_dist_2025.jpeg"),
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
    
    
    
    # INCIDENCE, 1980 to 2040
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/inc_age_all_projection.jpeg"),
         width = 3000,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "incidence",
            ages = c("All ages","0-14","15-49","50 and over"),
            facet.by = "age",
            years=1980:2040, 
            show.individual.sims = F,
            for.paper = T,
            ncol=4) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    # PREVALENCE, 1980 to 2040
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/prev_age_all_projection.jpeg"),
         width = 3000,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "prevalence",
            ages = c("All ages","0-14","15-49","50 and over"),
            facet.by = "age",
            years=1980:2040, 
            show.individual.sims = F,
            for.paper = T,
            ncol=4) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    
    
    ### MOST RECENT YEARS ONLY ### 
    
    
    # INCIDENCE, 2005 to 2025
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/inc_age_recent.jpeg"),
         width = 3000,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "incidence",
            ages = c("All ages","0-14","15-49","50 and over"),
            facet.by = "age",
            years=2005:2025, 
            show.individual.sims = F,
            for.paper = T,
            ncol=4) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    # PREVALENCE, 2005 to 2025
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/prev_age_recent.jpeg"),
         width = 3000,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "prevalence",
            ages = c("All ages","0-14","15-49","50 and over"),
            facet.by = "age",
            years=2005:2025, 
            show.individual.sims = F,
            for.paper = T,
            ncol=4) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    
    
    # INCIDENCE, 2000 to 2040
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/inc_age_recent_projection.jpeg"),
         width = 3000,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "incidence",
            ages = c("All ages","0-14","15-49","50 and over"),
            facet.by = "age",
            years=2000:2040, 
            show.individual.sims = F,
            for.paper = T,
            ncol=4) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    
    # PREVALENCE, 2000 to 2040
    jpeg(file=paste0("results/for_lancet_PAPER/",convert_string(simset.no.int@simulations[[1]]$location),"/",Sys.Date(),"/prev_age_recent_projection.jpeg"),
         width = 3000,height = 900,res=200)
    simplot(simset.no.int,
            data.types = "prevalence",
            ages = c("All ages","0-14","15-49","50 and over"),
            facet.by = "age",
            years=2000:2040, 
            show.individual.sims = F,
            for.paper = T,
            ncol=4) +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
    dev.off() 
    

}