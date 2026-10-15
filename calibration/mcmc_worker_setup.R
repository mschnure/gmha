################################################################################################
# Description: Loads everything needed to run MCMC chains in an R session
#              (used by run_mcmc_pipeline.R for each parallel R session, and by setup_run_mcmc.R)
################################################################################################

suppressPackageStartupMessages({
    library(bayesian.simulations)
    library(distributions)
    library(ggplot2)
})
source("model/run_systematic.R")      # model, data, likelihoods, priors, settings (via source_code.R)
source("calibration/mcmc_pipeline.R") # run.mcmc.step.chain, merge.step2.chains, run.mcmc.pipeline
use.cpp.model.if.possible()           # C++ ODE if it compiles here, otherwise the R version
