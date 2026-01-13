
DATE.TO.LOAD = "2026-01-13"
PROB.5.YEAR = 0.25

load("cached/simset.noint_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")
load("cached/simset.es_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")
load("cached/simset.eu_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")
load("cached/simset.du_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")
load("cached/simset.es.direct_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")
load("cached/simset.eu.direct_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")
load("cached/simset.du.direct_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")
load("cached/simset.all_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")
load("cached/simset.all.direct_",(PROB.5.YEAR*100),"_",DATE.TO.LOAD,".Rdata")

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

# simset.list.full.100 = list(no.int = simset.no.int,
#                            es = simset.es,
#                            eu = simset.eu,
#                            du = simset.du,
#                            es.direct = simset.es.direct,
#                            eu.direct = simset.eu.direct,
#                            du.direct = simset.du.direct,
#                            all = simset.all,
#                            all.direct = simset.all.direct
# )

simset.list.full = c(simset.list.full.25,
                     simset.list.full.100)

full.results.array = generate.full.results.array(simset.list = simset.list.full)
dimnames(full.results.array)$intervention = paste0(
    dimnames(full.results.array)$intervention,
    rep(c(".25", ".100"), each = length(unique(dimnames(full.results.array)$intervention)))
)

dimnames(full.results.array)$intervention[1] = "no.int"

YEARS.TO.SUMMARIZE = 2022:2030
INTERVENTIONS = dimnames(full.results.array)$intervention[c(-1,-10)] # remove no interventionss
AGES = NULL # c("15-19","20-24")

infections.averted = calculate.infections.averted(full.results.array,
                                                  output = "number",
                                                  interventions = INTERVENTIONS,
                                                  years = YEARS.TO.SUMMARIZE)

initiated.on.lai = full.results.array[as.character(YEARS.TO.SUMMARIZE),,,c("lai.art.es","lai.art.eu","lai.art.du"),,INTERVENTIONS, drop = F]
initiated.on.lai = apply(initiated.on.lai,c("outcome","sim","intervention"),sum)
initiated.on.lai = aperm(initiated.on.lai, c("sim", "intervention", "outcome"))
initiated.on.lai.total = apply(initiated.on.lai,c("sim","intervention"),sum)

dim.names = c(dimnames(initiated.on.lai)[1:2],
              list("outcome" = c(dimnames(initiated.on.lai)$outcome,"total")))

initiated.on.lai = array(c(initiated.on.lai,initiated.on.lai.total),
                         dim = sapply(dim.names,length),
                         dimnames = dim.names)

initiated.per.infection.averted = initiated.on.lai[,,"total"]/infections.averted
lai.per.IA = apply(initiated.per.infection.averted,c("intervention"),quantile,probs = c(0.025,0.5,0.975),na.rm = T)

if(1==2){
    infections.averted.summary = calculate.infections.averted.summary(full.results.array,
                                                                      interventions = INTERVENTIONS,
                                                                      years = YEARS.TO.SUMMARIZE)
    
    infections.averted.15.to.24.summary = calculate.infections.averted.summary(full.results.array,
                                                                               interventions = INTERVENTIONS,
                                                                               c("15-19","20-24"),
                                                                               years = YEARS.TO.SUMMARIZE)
    
    infections.averted.summary
    infections.averted.15.to.24.summary        
}