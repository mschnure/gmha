library("RColorBrewer")
library("scales")
library("ggsci")
source("model/run_systematic.R")

load("final_simsets_and_results/all.results_all.low_2025-08-27.Rdata")
simset.low = simset.list.full$no.int

load("final_simsets_and_results/all.results_all.lower.middle_2025-08-27.Rdata")
simset.lower.middle = simset.list.full$no.int

load("final_simsets_and_results/all.results_all.upper.middle_2025-08-27.Rdata")
simset.upper.middle = simset.list.full$no.int

load("final_simsets_and_results/all.results_all.high_2025-08-27.Rdata")
simset.high = simset.list.full$no.int

load("final_simsets_and_results/all.results_global_2025-08-27.Rdata")
simset.high = simset.list.full$no.int

simset = simset.low

simplot(simset,
        years = 1980:2040, 
        data.types = "population",
        facet.by='age')

simplot(simset,
        years = 1980:2040, 
        data.types = "prevalence",
        facet.by='age')
