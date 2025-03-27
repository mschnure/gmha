library(data.table)
library(RColorBrewer)
library(scales)
library(ggsci)

source('helpers/convert_strings.R')
source('data_manager/extract_suppressed_values.R')
source('data_manager/data_manager_functions.R')
source('model/diffeq.R')
source('model/age_mappings.R') 
NO.INTERVENTION = c()

load('cached/data.manager_global_2025-03-27.Rdata') # includes new median values for suppressed data 
#load('cached/data.manager_global_2024-12-30.Rdata') 

source('model/parameter_mappings/age_mixing.R') 
source('model/parameter_mappings/get_testing_projection_and_param_table.R')
source('model/parameter_mappings/age_sex_mixing_proportions.R') 
source('model/parameter_mappings/get_age_sex_transmission_multipliers.R')
source('model/parameter_mappings/get_engagement_disengagement_projection_and_param_table.R') 
source('model/parameter_mappings/get_suppression_rebound_data.R')

source('model/parameter_mappings/calculate_death_rates.R')
source('model/parameters.R')
source('model/get_initial_population.R')
source('model/simplot.R')

# likelihood and priors 
source('calibration/likelihood/likelihood.R')
#source("calibration/likelihood/individual_likelihoods.R")
source('calibration/make_joint_distribution.R')
source('calibration/prior_distributions/var_blocks.R')
source('calibration/set_likelihood_and_prior.R') # sources all individual priors 

# future projections
source('future_projections/create_and_run_interventions_functions.R')
source('future_projections/extract_projection_results.R')



