library(colorspace)
source('future_projections/create_lai_interventions.R')

DATE.TO.LOAD = "2026-01-22"
#PROB.5.YEAR = 1
PROB.1.YEAR = 1

load(paste0("cached/simset.noint_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
load(paste0("cached/simset.es_",(PROB.1.YEAR*100),"_",Sys.Date(),".Rdata"))
load(paste0("cached/simset.eu_",(PROB.1.YEAR*100),"_",Sys.Date(),".Rdata"))
load(paste0("cached/simset.du_",(PROB.1.YEAR*100),"_",Sys.Date(),".Rdata"))
load(paste0("cached/simset.es.direct_",(PROB.1.YEAR*100),"_",Sys.Date(),".Rdata"))
load(paste0("cached/simset.eu.direct_",(PROB.1.YEAR*100),"_",Sys.Date(),".Rdata"))
load(paste0("cached/simset.du.direct_",(PROB.1.YEAR*100),"_",Sys.Date(),".Rdata"))
load(paste0("cached/simset.all_",(PROB.1.YEAR*100),"_",Sys.Date(),".Rdata"))
load(paste0("cached/simset.all.direct_",(PROB.1.YEAR*100),"_",Sys.Date(),".Rdata"))

if(1==2){
    load(paste0("cached/simset.noint_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.es_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.eu_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.du_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.es.direct_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.eu.direct_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.du.direct_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.all_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.all.direct_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata"))    
}

if(PROB.1.YEAR==0.25){
    simset.list.full.25 = list(no.int = simset.no.int,
                               es = simset.es,
                               eu = simset.eu,
                               du = simset.du,
                               es.direct = simset.es.direct,
                               eu.direct = simset.eu.direct,
                               du.direct = simset.du.direct,
                               all = simset.all,
                               all.direct = simset.all.direct
    )
}

if(PROB.1.YEAR==1){
    simset.list.full.100 = list(no.int = simset.no.int,
                                es = simset.es,
                                eu = simset.eu,
                                du = simset.du,
                                es.direct = simset.es.direct,
                                eu.direct = simset.eu.direct,
                                du.direct = simset.du.direct,
                                all = simset.all,
                                all.direct = simset.all.direct
    )
}

simset.list.full = c(simset.list.full.25,
                     simset.list.full.100)

full.results.array = generate.full.results.array(simset.list = simset.list.full)
dimnames(full.results.array)$intervention = paste0(
    dimnames(full.results.array)$intervention,
    rep(c(".25", ".100"), each = length(unique(dimnames(full.results.array)$intervention)))
)

dimnames(full.results.array)$intervention[1] = "no.int"

save(full.results.array, file = paste0("cached/full.results.array_",Sys.Date(),".Rdata"))

YEARS.TO.SUMMARIZE = 2022:2030
INTERVENTIONS = dimnames(full.results.array)$intervention[c(-1,-10)] # remove no intervention

infections.averted = calculate.infections.averted(full.results.array,
                                                  output = "number",
                                                  interventions = INTERVENTIONS,
                                                  years = YEARS.TO.SUMMARIZE)

infections.averted.15.to.24 = calculate.infections.averted(full.results.array,
                                                  output = "number",
                                                  ages = c("15-19","20-24"),
                                                  interventions = INTERVENTIONS,
                                                  years = YEARS.TO.SUMMARIZE)

#table(is.na(infections.averted))

initiated.on.lai = full.results.array[as.character(YEARS.TO.SUMMARIZE),,,c("lai.art.es","lai.art.eu","lai.art.du"),,INTERVENTIONS, drop = F]
initiated.on.lai = apply(initiated.on.lai,c("outcome","sim","intervention"),sum)
initiated.on.lai = aperm(initiated.on.lai, c("sim", "intervention", "outcome"))
initiated.on.lai.total = apply(initiated.on.lai,c("sim","intervention"),sum)

dim.names = c(dimnames(initiated.on.lai)[1:2],
              list("outcome" = c(dimnames(initiated.on.lai)$outcome,"total")))

initiated.on.lai = array(c(initiated.on.lai,initiated.on.lai.total),
                         dim = sapply(dim.names,length),
                         dimnames = dim.names)

# Numbers initiated on LAI by intervention and arrow 
lai.by.arrow = apply(initiated.on.lai,c("outcome","intervention"),quantile,probs = c(0.025,0.5,0.975),na.rm = T)
write.csv(lai.by.arrow, file = paste0("cached/lai.by.arrow_",Sys.Date(),".csv"))

# LAI per infection averted 
initiated.per.infection.averted = initiated.on.lai[,,"total"]/infections.averted
lai.per.IA = apply(initiated.per.infection.averted,c("intervention"),quantile,probs = c(0.025,0.5,0.975),na.rm = T)
write.csv(lai.per.IA, file = paste0("cached/lai.per.IA_",Sys.Date(),".csv"))

initiated.per.infection.averted.15.to.24 = initiated.on.lai[,,"total"]/infections.averted.15.to.24
lai.per.IA.15.to.24 = apply(initiated.per.infection.averted.15.to.24,c("intervention"),quantile,probs = c(0.025,0.5,0.975),na.rm = T)
write.csv(lai.per.IA.15.to.24, file = paste0("cached/lai.per.IA.15.to.24_",Sys.Date(),".csv"))


if(1==2){
    infections.averted.summary = calculate.infections.averted.summary(full.results.array,
                                                                      interventions = INTERVENTIONS,
                                                                      years = YEARS.TO.SUMMARIZE)
    
    infections.averted.15.to.24.summary = calculate.infections.averted.summary(full.results.array,
                                                                               interventions = INTERVENTIONS,
                                                                               ages = c("15-19","20-24"),
                                                                               years = YEARS.TO.SUMMARIZE)
    
    infections.averted.summary
    infections.averted.15.to.24.summary   

    write.csv(infections.averted.summary, file = paste0("cached/nextgen_infections_averted_",Sys.Date(),".csv"))
    write.csv(infections.averted.15.to.24.summary, file = paste0("cached/nextgen_infections_averted_15-24_",Sys.Date(),".csv"))
    
    
    
    
    ## Boxplot
    scenarios = c("25%, With \n suppression requirement", "25%, Without \n suppression requirement", 
                  "100%, With \n suppression requirement", "100%, Without \n  suppression requirement")
    
    df = data.frame(scenario = factor(scenarios, levels = scenarios),
                    infections.averted.estimate = infections.averted.summary[2,"inf.averted",c("all.25","all.direct.25",
                                                                                               "all.100","all.direct.100")],
                    infections.averted.lower = infections.averted.summary[1,"inf.averted",c("all.25","all.direct.25",
                                                                                               "all.100","all.direct.100")],
                    infections.averted.upper = infections.averted.summary[3,"inf.averted",c("all.25","all.direct.25",
                                                                                               "all.100","all.direct.100")]
                    
    )
    
    
    cols = c(
        "#D55E00",  # orange
        "#0072B2",  # blue
        "#009E73",  # green
        "#CC79A7",  # magenta
        "#56B4E9"   # bright cyan/sky blue
    )
    
    
    jpeg(file=paste0("../../5_Manuscripts_conferences/2_Conferences/2026_AIDS/nextgen/barplot_",Sys.Date(),".jpeg"),
         width = 3000,height = 1000,res=200)
    ggplot(df, aes(x = scenario, y = infections.averted.estimate)) +
        geom_col(fill = cols[2:5], color = "black", width = 0.6) +  # bars
        geom_errorbar(aes(ymin = infections.averted.lower, ymax = infections.averted.upper), width = 0.2, color = "black") +  # CI lines
        theme_minimal() +
        ylab("Cumulative infections averted, 2022-2030") +
        xlab(NULL) + 
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)}) + 
        theme(
            axis.title.y = element_text(size = 16),   # x-axis title larger
            axis.text.x  = element_text(size = 14),   # x-axis tick labels
            axis.text.y  = element_text(size = 14)    # y-axis tick labels
        )
    dev.off() 
    
    ## Simplot
    
    load(paste0("cached/simset.noint_25_1yr_",DATE.TO.LOAD,".Rdata"))
    load(paste0("cached/simset.all_25_1yr_",DATE.TO.LOAD,".Rdata"))
    simset.all.25 = simset.all
    load(paste0("cached/simset.all.direct_25_1yr_",DATE.TO.LOAD,".Rdata"))
    simset.all.direct.25 = simset.all.direct
    
    load(paste0("cached/simset.all_100_1yr_",DATE.TO.LOAD,".Rdata"))
    simset.all.100 = simset.all
    load(paste0("cached/simset.all.direct_100_1yr_",DATE.TO.LOAD,".Rdata"))
    simset.all.direct.100 = simset.all.direct
    
    if(1==2){
        load(paste0("cached/simset.noint_25_",DATE.TO.LOAD,".Rdata"))
        load(paste0("cached/simset.all.direct_25_",DATE.TO.LOAD,".Rdata"))
        simset.all.direct.25 = simset.all.direct
        
        load(paste0("cached/simset.all_100_",DATE.TO.LOAD,".Rdata"))
        simset.all.100 = simset.all
        load(paste0("cached/simset.all.direct_100_",DATE.TO.LOAD,".Rdata"))
        simset.all.direct.100 = simset.all.direct  
    }

    
    simplot(#simset.no.int,
            simset.all.25,
            simset.all.direct.25,
            #simset.all.100,
            #simset.all.direct.100,
            years=2020:2030, 
            #ages = c("15-24"),
            data.types = c("incidence"))
    
    
    p = simplot(simset.no.int,
            simset.all.25,
            simset.all.direct.25,
            simset.all.100,
            simset.all.direct.100,
            data.types = "incidence",
            ages = c("All ages"),
            facet.by = "age",
            years=2010:2030, 
            show.individual.sims = F,
            for.paper = T,
            ncol=1) +
        theme(strip.text.x = element_blank(),
              axis.title.x=element_blank(),
              axis.title.y = element_text(size = 16),
              text = element_text(size = 20),
              legend.position = "none")+
        scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},limits = c(0,NA))
    

    
    ribbon_cols = lighten(cols, amount = 0.3)
    
    point_idx  <- which(sapply(p$layers, function(l) inherits(l$geom, "GeomPoint")))
    
    ## ---- POINTS ----
    p$layers[[point_idx]]$mapping$colour <- NULL
    p$layers[[point_idx]]$aes_params$colour <- "grey10"
    
    jpeg(file=paste0("../../5_Manuscripts_conferences/2_Conferences/2026_AIDS/nextgen/fig_",Sys.Date(),".jpeg"),
         width = 3000,height = 1000,res=200)
    p +
        scale_color_manual(values = cols) +
        scale_fill_manual(values = ribbon_cols) + 
        ylab("Annual New HIV infections, 2010-2030") 
    dev.off()
    
    labels <- c("Baseline","25%, suppression requirement", "25%, no suppression requirement", 
                "100%, suppression requirement", "100%, no suppression requirement")
    
    # Dummy data for the labels
    df <- data.frame(
        sim = factor(labels, levels = labels),
        xmin = 1:5,
        xmax = 1:5,
        ymin = 0,
        ymax = 1
    )
    
    # Plot only colored boxes
    ggplot(df) +
        geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sim)) +
        scale_fill_manual(values = ribbon_cols, name = "Simulations") +
        theme(legend.position = "left",
              legend.background = element_rect(color = "black", size = 0.5, fill = "white"),
              legend.key = element_rect(color = NA, fill = "white")  # keeps each swatch clean
        )

    
    
    
}