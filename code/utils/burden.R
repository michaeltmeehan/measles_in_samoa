library(tidyr)
library(purrr)

#' burden.R
#'
#' Utility functions for calculating measles burden metrics including
#' effective reproduction number (Reff), final size, infections, and deaths.
#' These helpers are thin wrappers around core epidemic calculations used in
#' burden estimation and bootstrapping for uncertainty.
#'
#' Note: This file assumes presence of helper functions elsewhere in the
#' project: calc_reff, calc_final_size, calc_immunity, simulate_immunity,
#' and forecast_vaccine_coverage.


#' Calculate infections from final size, population and immunity
#'
#' @param final_size Numeric vector or matrix of final size (proportion infected
#'   among susceptibles) by age group.
#' @param population Numeric vector of population counts by age group.
#' @param immunity Numeric vector of immunity (proportion immune) by age group.
#' @return Numeric vector or matrix of estimated infections by age group.
#' @examples
#' # infections <- calc_infections(final_size, population, immunity)
calc_infections = function(final_size, population, immunity){
  susceptible_population = population * (1 - immunity)
  infections = final_size * susceptible_population
  return(infections)
}


#+ Calculate deaths from infections and burden parameters
#'
#' @param infections Numeric vector or matrix of infections by age group.
#' @param burden_pars List containing burden parameters. Must include 'ifr'
#'   (infection fatality ratio) which can be a scalar or vector by age.
#' @return Numeric vector or matrix of estimated deaths by age group.
calc_deaths = function(infections, burden_pars){
  deaths = infections * burden_pars$ifr
  return(deaths)
}


#+ Calculate burden metrics for a single scenario
#'
#' Computes Reff, final size, infections and deaths for given inputs.
#'
#' @param r0 Basic reproduction number (scalar).
#' @param contact_matrix Contact matrix (age x age).
#' @param population Numeric vector of population counts by age group.
#' @param immunity Numeric vector of immunity (proportion immune) by age group.
#' @param burden_pars List of burden parameters (e.g. ifr).
#' @param symmetrize Logical indicating whether to symmetrize contact matrix
#'   when computing Reff.
#' @return A list with elements: reff, final_size, infections, deaths.
calc_burden = function(r0, contact_matrix, population, immunity, burden_pars, symmetrize=FALSE){
  reff = calc_reff(r0, contact_matrix, population, immunity, symmetrize)
  final_size = calc_final_size(r0, contact_matrix, population, immunity)
  infections = calc_infections(final_size, population, immunity)
  deaths = calc_deaths(infections, burden_pars)
  
  burden = list(
    reff = reff,
    final_size = final_size,
    infections = infections,
    deaths = deaths
  )
  
  return(burden)
}

#+ Bootstrap burden estimates to obtain uncertainty intervals
#'
#' For a given year and inputs, this function computes the central burden
#' estimates and bootstrapped uncertainty intervals by resampling immunity
#' (based on seropositivity/sample sizes) and recomputing burden metrics.
#'
#' @param year Integer year for which to calculate immunity.
#' @param r0 Basic reproduction number.
#' @param contact_matrix Contact matrix (age x age).
#' @param population Numeric vector of population counts by age group.
#' @param vaccine_coverage Vaccine coverage data structure used by
#'   calc_immunity.
#' @param vaccine_pars Parameters for vaccine effect used by calc_immunity.
#' @param seropositivity Seropositivity data used by calc_immunity.
#' @param sample_size Named list or vector of sample sizes by year.
#' @param burden_pars List of burden parameters (e.g. ifr).
#' @param replicates Number of bootstrap replicates (default 1000).
#' @param delta Numeric smoothing/perturbation parameter passed to
#'   simulate_immunity.
#' @param symmetrize Logical, passed to calc_burden.
#' @return A named list of data.frames (one per metric) with columns: central,
#'   lower, upper, and age_group (when applicable).
bootstrap_burden = function(year,
                            r0, 
                            contact_matrix,
                            population, 
                            vaccine_coverage,
                            vaccine_pars,
                            seropositivity,
                            sample_size,
                            burden_pars, 
                            replicates=1000, 
                            delta=0.2, 
                            symmetrize=FALSE){
  
  immunity = calc_immunity(year, vaccine_coverage, vaccine_pars, seropositivity)
  
  central_burden = calc_burden(r0, contact_matrix, population, immunity, burden_pars, symmetrize)
  
  simulated_burden = replicate(replicates, {
    simulated_immunity = simulate_immunity(immunity, sample_size[[as.character(min(c(year, 2024)))]], delta)
    calc_burden(r0, contact_matrix, population, simulated_immunity, burden_pars, symmetrize)
  }, simplify=FALSE
  )
  
  # Calculate 95% CI for each burden metric
  ci = lapply(names(central_burden), function(metric) {
    simulated_metric = sapply(simulated_burden, function(x) x[[metric]])
    if(is.null(nrow(simulated_metric))){
      simulated_metric = matrix(simulated_metric, nrow=1)
    }
    lower = apply(simulated_metric, 1, quantile, probs=0.025)
    upper = apply(simulated_metric, 1, quantile, probs=0.975)
    df = data.frame(central=central_burden[[metric]], lower=lower, upper=upper)
    if(!is.null(rownames(df))){
      df$age_group = factor(rownames(df), levels=rownames(df))
      rownames(df) = NULL
    }
    return(df)
  })
  names(ci) = names(central_burden)
  
  return(ci)
}


#+ Forecast burden across scenarios
#'
#' Uses forecast_vaccine_coverage to generate vaccine coverage for each
#' scenario, then runs bootstrap_burden and combines results across scenarios.
#'
#' @param year Year for burden calculation.
#' @param scenarios Character vector of scenario names.
#' @param r0 Basic reproduction number.
#' @param contact_matrix Contact matrix (age x age).
#' @param population Numeric vector of population counts by age group.
#' @param vaccine_pars Parameters for vaccine effect.
#' @param seropositivity Seropositivity data used by calc_immunity.
#' @param sample_size Named list or vector of sample sizes by year.
#' @param burden_pars List of burden parameters (e.g. ifr).
#' @param forecast_pars Parameters used by forecast_vaccine_coverage.
#' @param replicates Number of bootstrap replicates.
#' @param delta Numeric smoothing/perturbation parameter passed to
#'   simulate_immunity.
#' @param symmetrize Logical passed to calc_burden.
#' @return A named list of data.frames (one per metric) with combined
#'   Scenario column.
forecast_burden = function(year,
                           scenarios,
                           r0, 
                           contact_matrix,
                           population,
                           vaccine_pars,
                           seropositivity,
                           sample_size,
                           burden_pars,
                           forecast_pars,
                           replicates=1000, 
                           delta=0.2, 
                           symmetrize=FALSE){
  
  burden = lapply(scenarios, function(scenario){
    vaccine_coverage = forecast_vaccine_coverage(scenario, forecast_pars)
    year_to_forecast = ifelse(scenario == "baseline", 2024, year)
    burden_scenario = bootstrap_burden(year_to_forecast,
                                       r0, 
                                       contact_matrix,
                                       population, 
                                       vaccine_coverage,
                                       vaccine_pars,
                                       seropositivity,
                                       sample_size,
                                       burden_pars, 
                                       replicates, 
                                       delta, 
                                       symmetrize)
    return(burden_scenario)
  })
  names(burden) = scenarios
  # Loop through burden metrics to combine scenario results
  metrics = names(burden[[1]])
  burden_results = lapply(metrics, function(metric){
    df_list = lapply(names(burden), function(scenario){
      df = burden[[scenario]][[metric]]
      df$Scenario = scenario
      return(df)
    })
    result_df = bind_rows(df_list)
    return(result_df)
  })
  names(burden_results) = metrics
  return(burden_results)
}
