aware.mcmc = extract.data(sim.mcmc,years="2040",sexes="female",data.type = "awareness",keep.dimensions = c("year","age"))
supp.mcmc = extract.data(sim.mcmc,years="2040",sexes="female",data.type = "suppression",keep.dimensions = c("year","age"))

supp.mcmc = supp.mcmc/aware.mcmc

aware.manual = extract.data(sim.manual,years="2040",sexes="female",data.type = "awareness",keep.dimensions = c("year","age"))
supp.manual = extract.data(sim.manual,years="2040",sexes="female",data.type = "suppression",keep.dimensions = c("year","age"))

supp.manual = supp.manual/aware.manual

cbind(t(supp.mcmc),t(supp.manual))


inc.mcmc.2040 = extract.data(sim.mcmc,years="2040",sexes="female",data.type = "incidence",keep.dimensions = c("year","age"))
inc.mcmc.2025 = extract.data(sim.mcmc,years="2025",sexes="female",data.type = "incidence",keep.dimensions = c("year","age"))

cbind(t(inc.mcmc.2025),t(inc.mcmc.2040))


median_index <- function(counts) {
    cum_sum <- cumsum(counts)  # Compute cumulative sum
    median_pos <- ceiling(sum(counts) / 2)  # Middle position
    return(which(cum_sum >= median_pos)[1])  # Find first index reaching median
}

index = median_index(inc.mcmc.2025)
colnames(inc.mcmc.2025)[index]

calculate.median.age.for.sim(sim.mcmc,data.type = "prevalence",year=c(2025),sexes = c("male","female")) # 42
calculate.median.age.for.sim(sim.mcmc,data.type = "prevalence",year=c(2040),sexes = c("male","female")) # 53

calculate.median.age.for.sim(sim.mcmc,data.type = "incidence",year=c(2025),sexes = c("male","female")) # 26
calculate.median.age.for.sim(sim.mcmc,data.type = "incidence",year=c(2040),sexes = c("male","female")) # 32