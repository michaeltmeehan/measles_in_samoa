library(tidyr)
library(purrr)


calc_infections = function(final_size, population, immunity){
  susceptible_population = population * (1 - immunity)
  infections = final_size * susceptible_population
  return(infections)
}


calc_deaths = function(infections, burden_pars){
  deaths = infections * burden_pars$ifr
  return(deaths)
}


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


burden_year_r0 = function(years,
                       r0_values, 
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
  
  grid = crossing(Year = years, R0 = r0_values)
  
  burden = pmap(grid, function(Year, R0){
    bootstrap_burden(year = Year,
                     r0 = R0,
                     contact_matrix = contact_matrix,
                     population = population,
                     vaccine_coverage = vaccine_coverage,
                     vaccine_pars = vaccine_pars,
                     seropositivity = seropositivity,
                     sample_size = sample_size,
                     burden_pars = burden_pars,
                     replicates = replicates,
                     delta = delta,
                     symmetrize = symmetrize)
  })
  
  metrics = names(burden[[1]])
  burden_results = lapply(metrics, function(metric){
    df_list = lapply(seq_along(burden), function(i){
      df = burden[[i]][[metric]]
      df$Year = grid$Year[i]
      df$R0 = grid$R0[i]
      return(df)
    })
    result_df = bind_rows(df_list)
    return(result_df)
  })
  names(burden_results) = metrics
  return(burden_results)
}



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
    burden_scenario = bootstrap_burden(year,
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
