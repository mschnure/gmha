library(colorspace)
source('future_projections/create_lai_interventions.R')
source('future_projections/generate_lai_outcomes.R')

## Set up dataframes, labels ## 

## Boxplot
scenarios = c("25% uptake,\n With suppression \n requirement", 
              "25% uptake,\n No suppression \n requirement", 
              "100% uptake,\n With suppression \n requirement", 
              "100% uptake,\n No suppression \n requirement")

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

## Simplot
DATE.TO.LOAD = "2026-01-22"
load(paste0("cached/simset.noint_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.all_25_1yr_",DATE.TO.LOAD,".Rdata"))
simset.all.25 = simset.all
load(paste0("cached/simset.all.direct_25_1yr_",DATE.TO.LOAD,".Rdata"))
simset.all.direct.25 = simset.all.direct

load(paste0("cached/simset.all_100_1yr_",DATE.TO.LOAD,".Rdata"))
simset.all.100 = simset.all
load(paste0("cached/simset.all.direct_100_1yr_",DATE.TO.LOAD,".Rdata"))
simset.all.direct.100 = simset.all.direct

## SAVE FIGURES FOR POSTER ##

## Boxplot ## 
jpeg(file="../../5_Manuscripts_conferences/2_Conferences/2026_AIDS/nextgen/poster/PanelB.jpeg",
     width = 3000,height = 1500,res=200)
ggplot(df, aes(x = scenario, y = infections.averted.estimate)) +
    geom_col(fill = cols[2:5], color = "black", width = 0.6) +  # bars
    geom_errorbar(aes(ymin = infections.averted.lower, ymax = infections.averted.upper), width = 0.2, color = "black") +  # CI lines
    theme_minimal() +
    ylab("Cumulative infections averted,\n2022-2030") +
    xlab(NULL) + 
    scale_y_continuous(labels = function(x){ paste0(x / 1000, "k") }) + 
    theme(
        axis.title.y = element_text(size = 30,  margin = margin(r = 15)),   # y-axis title larger, to the left 
        axis.text.x  = element_text(size = 25),   # x-axis tick labels
        axis.text.y  = element_text(size = 28)    # y-axis tick labels
    )
dev.off() 

## Simplot ## 
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
          axis.title.y = element_text(size = 30, margin = margin(r = 15)),
          text = element_text(size = 28),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){ paste0(x / 1000, "k") },limits = c(0,NA))


ribbon_cols = lighten(cols, amount = 0.3)

# add points
point_idx  <- which(sapply(p$layers, function(l) inherits(l$geom, "GeomPoint")))
p$layers[[point_idx]]$mapping$colour <- NULL
p$layers[[point_idx]]$aes_params$colour <- "grey10"

jpeg(file = "../../5_Manuscripts_conferences/2_Conferences/2026_AIDS/nextgen/poster/PanelA.jpeg",
     width = 3000,height = 1500,res=200)
p +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = ribbon_cols) + 
    ylab("Annual New HIV infections,\n 2010-2030") 
dev.off()

## Label ##
# Dummy data for the labels
df <- data.frame(
    sim = factor(labels, levels = labels),
    xmin = 1:5,
    xmax = 1:5,
    ymin = 0,
    ymax = 1
)

# Plot only colored boxes
jpeg(file = "../../5_Manuscripts_conferences/2_Conferences/2026_AIDS/nextgen/poster/Legend.jpeg",
     width = 3000,height = 1500,res=200)
ggplot(df) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sim)) +
    scale_fill_manual(values = ribbon_cols, name = "Simulations") +
    theme(
        legend.position    = "left",
        legend.background  = element_rect(color = "black", size = 0.5, fill = "white"),
        legend.key         = element_rect(color = NA, fill = "white"),
        legend.title       = element_text(size = 36, face = "bold"),
        legend.text        = element_text(size = 30),
        legend.key.size    = unit(2, "cm"),
        legend.spacing.y   = unit(0.5, "cm"),
        legend.margin      = margin(10, 40, 10, 10),  # top, RIGHT, bottom, left
        legend.box.margin  = margin(10, 60, 10, 10)   # space outside the legend box
    )
dev.off()

## Save figures for abstract ## 
if(1==2){
    jpeg(file=paste0("../../5_Manuscripts_conferences/2_Conferences/2026_AIDS/nextgen/abstract/barplot_",Sys.Date(),".jpeg"),
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
    
    
    # simplot(#simset.no.int,
    #     simset.all.25,
    #     simset.all.direct.25,
    #     #simset.all.100,
    #     #simset.all.direct.100,
    #     years=2020:2030, 
    #     #ages = c("15-24"),
    #     data.types = c("incidence"))
    
    
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
    
    jpeg(file=paste0("../../5_Manuscripts_conferences/2_Conferences/2026_AIDS/nextgen/abstract/fig_",Sys.Date(),".jpeg"),
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


