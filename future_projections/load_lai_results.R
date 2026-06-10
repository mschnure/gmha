
DATE.TO.LOAD = "2026-01-22"

print("loading 100% interventions")
load(paste0("cached/simset.noint_100_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.es_100_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.eu_100_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.du_100_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.es.direct_100_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.eu.direct_100_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.du.direct_100_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.all_100_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.all.direct_100_1yr_",DATE.TO.LOAD,".Rdata"))    

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

print("loading 25% interventions")
load(paste0("cached/simset.noint_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.es_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.eu_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.du_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.es.direct_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.eu.direct_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.du.direct_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.all_25_1yr_",DATE.TO.LOAD,".Rdata"))
load(paste0("cached/simset.all.direct_25_1yr_",DATE.TO.LOAD,".Rdata"))  

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


simset.list.full = c(simset.list.full.25,
                     simset.list.full.100)

